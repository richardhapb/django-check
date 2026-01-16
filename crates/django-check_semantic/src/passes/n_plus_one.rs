//! N+1 query detection pass.

use ruff_python_ast::ModModule;
use ruff_python_ast::{Expr, Stmt, visitor::Visitor, visitor::walk_expr, visitor::walk_stmt};
use ruff_text_size::Ranged;
use std::collections::HashMap;
use std::collections::HashSet;

// Assume these imports exist in your crate structure
use crate::diagnostic::NPlusOneDiagnostic;
use crate::ir::binding::{DjangoSymbol, DjangoSymbolId, QuerySetState};
use crate::ir::model::ModelGraph;
use crate::passes::Pass;
use crate::passes::{extract_attribute_chain, functions::QueryFunction};

const DIAGNOSTIC_CODE: &str = "N+1";

/// Manages variable visibility and bindings.
#[derive(Debug)]
struct SymbolTable {
    /// Stack of scopes. Each scope maps variable names to Symbol IDs.
    scopes: Vec<HashMap<String, DjangoSymbolId>>,
    /// The actual store of symbols. IDs index into this vector.
    store: Vec<DjangoSymbol>,
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            store: Vec::new(),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Allocates a new symbol and binds it to a name in the current scope.
    fn declare(&mut self, name: &str, symbol: DjangoSymbol) -> DjangoSymbolId {
        let id = DjangoSymbolId::new(self.store.len() as u32);
        self.store.push(symbol);

        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), id);
        }
        id
    }

    fn lookup(&self, name: &str) -> Option<&DjangoSymbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.get(name) {
                return self.store.get(id.0 as usize);
            }
        }
        None
    }
}

/// Stateless helper to determine what an Expression evaluates to.
struct QuerySetResolver<'a> {
    model_graph: &'a ModelGraph,
    symbols: &'a SymbolTable,
}

impl<'a> QuerySetResolver<'a> {
    fn new(model_graph: &'a ModelGraph, symbols: &'a SymbolTable) -> Self {
        Self {
            model_graph,
            symbols,
        }
    }

    /// Attempts to resolve an expression into a Django Symbol.
    fn resolve(&self, expr: &Expr) -> Option<DjangoSymbol> {
        match expr {
            Expr::Call(call) => self.resolve_call(call),
            Expr::Attribute(attr) => self.resolve_attribute(attr),
            Expr::Name(name) => self.symbols.lookup(&name.id).cloned(),
            _ => None,
        }
    }

    fn resolve_call(&self, call: &ruff_python_ast::ExprCall) -> Option<DjangoSymbol> {
        // Check for `list(qs)` wrapper
        if let Some(name) = call.func.as_name_expr()
            && name.id == "list"
        {
            let first_arg = call.arguments.args.first()?;
            return self.resolve(first_arg);
        }

        // Check for `get_object_or_404(Model, ...)`
        if let Some(name) = call.func.as_name_expr()
            && name.id.ends_with("get_object_or_404")
        {
            // First arg is usually the model class
            if let Some(Expr::Name(model_name)) = call.arguments.args.first() {
                return Some(DjangoSymbol::ModelInstance(QuerySetState::new(
                    model_name.id.to_string(),
                )));
            }
        }

        // Check for QuerySet method chaining (e.g. `User.objects.filter(...)`)
        // We look at the function being called (e.g. `User.objects.filter`)
        let Expr::Attribute(attr) = &*call.func else {
            return None;
        };

        let method_name = &attr.attr.id;

        // Methods that return a single model instance, not a queryset
        const SINGLE_INSTANCE_METHODS: [&str; 5] = ["get", "first", "last", "earliest", "latest"];

        // Resolve the base (e.g., `User.objects`)
        let base_sym = self.resolve(&attr.value)?;

        match base_sym {
            DjangoSymbol::QuerySet(mut state) => {
                // Check if this method returns a single instance
                if SINGLE_INSTANCE_METHODS.contains(&method_name.as_str()) {
                    return Some(DjangoSymbol::ModelInstance(state));
                }

                // Apply the side effect of the call (e.g. .prefetch_related) to the state
                // Note: We are creating a new state here, effectively simulating
                // the immutability/chaining of Django QuerySets.
                self.apply_queryset_mutation(&mut state, call, method_name);
                Some(DjangoSymbol::QuerySet(state))
            }
            DjangoSymbol::ModelInstance(_) => {
                // Calling a method on a model instance that returns a related queryset
                // e.g., `user.orders.all()` or `user.orders.filter(...)`
                // The base here would be `user.orders` which might not resolve,
                // but if we got here with ModelInstance, something else is going on.
                // This branch handles cases like `Model.objects.get(...).related.all()`
                None
            }
        }
    }

    fn resolve_attribute(&self, attr: &ruff_python_ast::ExprAttribute) -> Option<DjangoSymbol> {
        // Try to resolve the value being accessed
        if let Some(base_sym) = self.resolve(&attr.value) {
            match base_sym {
                DjangoSymbol::ModelInstance(state) | DjangoSymbol::QuerySet(state) => {
                    // Accessing `user.orders` -> Returns a QuerySet for the related model
                    // Accessing an attribute on a queryset (e.g., `orders.users`)
                    // If it's a relation on the model, return a QuerySet of the related model
                    if let Some(related_model) = self
                        .model_graph
                        .get_relation(&state.model_name, &attr.attr.id)
                    {
                        let mut new_state = QuerySetState::new(related_model.to_string());
                        // FIXME: Currently we use the current relations to be inherited by the related
                        // model, probably a better idea is to detect dynamically the appropiates
                        // prefetched relations but for now this is ok.
                        new_state
                            .prefetched_relations
                            .extend(state.prefetched_relations);
                        return Some(DjangoSymbol::QuerySet(new_state));
                    }
                    // Could be accessing a regular field, return None
                    return None;
                }
            }
        }

        // Fallback: Check if it is a Model Class access (e.g., `User.objects`)
        // This usually requires checking if the base is a known Model definition
        let (base_name, chain) = extract_attribute_chain(attr);
        if let Some(model) = self.model_graph.get(base_name) {
            // logic to ensure we are accessing .objects or a manager
            if chain.contains(&"objects") {
                return Some(DjangoSymbol::QuerySet(QuerySetState::new(
                    model.name.clone(),
                )));
            }
        }

        None
    }

    fn apply_queryset_mutation(
        &self,
        state: &mut QuerySetState,
        call: &ruff_python_ast::ExprCall,
        method_name: &str,
    ) {
        const SAFE_QS_METHODS: [&str; 2] = ["select_related", "prefetch_related"];
        const SAFE_NO_QS_METHODS: [&str; 2] = ["values", "values_list"];

        if SAFE_NO_QS_METHODS.contains(&method_name) {
            state.is_values_query = true;
        } else if SAFE_QS_METHODS.contains(&method_name) {
            let fields: Vec<String> = call
                .arguments
                .args
                .iter()
                .filter_map(|a| a.as_string_literal_expr())
                .map(|s| s.value.to_string())
                .collect();

            // Helper from binding.rs
            let relations = crate::ir::binding::parse_relation_fields(&fields);
            state.prefetched_relations.extend(relations);
        }
    }
}

#[derive(Debug, Clone)]
struct LoopContext {
    /// The variable name iterating (e.g. "user" in "for user in users")
    loop_var: String,
    /// The state of the queryset being iterated
    queryset_state: QuerySetState,
}

/// Tracks call site information for interprocedural analysis
#[derive(Debug, Clone)]
struct CallSiteInfo {
    /// The queryset states for each positional argument
    arg_states: Vec<Option<QuerySetState>>,
}

pub struct NPlusOnePass<'a> {
    filename: &'a str,
    source: &'a str,
    model_graph: &'a ModelGraph,
    functions: &'a [QueryFunction],

    // State
    symbols: SymbolTable,
    active_loops: Vec<LoopContext>,
    diagnostics: Vec<NPlusOneDiagnostic>,
    /// Track flagged expressions to avoid double-counting
    flagged_ranges: HashSet<(usize, usize)>,
    /// Track call sites: function_name -> call site info
    call_sites: HashMap<String, CallSiteInfo>,
    /// Current function name being analyzed (for parameter lookup)
    current_function: Option<String>,
}

impl<'a> NPlusOnePass<'a> {
    pub fn new(
        filename: &'a str,
        source: &'a str,
        model_graph: &'a ModelGraph,
        functions: &'a [QueryFunction],
    ) -> Self {
        Self {
            filename,
            source,
            model_graph,
            functions,
            symbols: SymbolTable::new(),
            active_loops: Vec::new(),
            diagnostics: Vec::new(),
            flagged_ranges: HashSet::new(),
            call_sites: HashMap::new(),
            current_function: None,
        }
    }

    fn resolver(&self) -> QuerySetResolver<'_> {
        QuerySetResolver::new(self.model_graph, &self.symbols)
    }

    fn record_assignment(&mut self, target: &Expr, value: &Expr) {
        // Resolve the right-hand side
        let Some(symbol) = self.resolver().resolve(value) else {
            return;
        };

        // Bind to left-hand side
        if let Expr::Name(name) = target {
            self.symbols.declare(&name.id, symbol);
        }
    }

    fn check_n_plus_one_access(
        &mut self,
        expr: &Expr,
        attr: &ruff_python_ast::ExprAttribute,
    ) -> bool {
        let range = expr.range();
        let range_key = (range.start().to_usize(), range.end().to_usize());

        // Skip if already flagged (parent expression already caught this)
        if self.flagged_ranges.contains(&range_key) {
            return false;
        }

        let (base_name, chain) = extract_attribute_chain(attr);
        let mut found_issue = false;

        // We only care if the base variable matches a currently active loop variable
        for ctx in &self.active_loops {
            if base_name != ctx.loop_var {
                continue;
            }

            // Walk the chain (e.g. user.profile.settings)
            let mut current_model = ctx.queryset_state.model_name.clone();

            for (i, segment) in chain.iter().enumerate() {
                // Is this segment a relation?
                if !self.model_graph.is_relation(&current_model, segment) {
                    // If not a relation, we might need to update current_model
                    // if it's a structural field, but for N+1 we usually stop.
                    // (Simplified logic: assuming standard Django FK traversal)
                    break;
                }

                // Is it safe?
                // The 'safe' check needs to account for the depth of the chain.
                // e.g. "profile" must be in prefetched.
                // If we are at "user.profile.settings", "profile" must be checked.

                // Construct the full path up to this point for lookup
                // e.g. "profile" or "profile__settings"
                let relation_path = segment;

                if !ctx.queryset_state.is_access_safe(relation_path) {
                    self.diagnostics.push(build_diagnostic(
                        self.filename,
                        self.source,
                        expr,
                        &ctx.loop_var,
                        &chain[0..=i].join("."),
                    ));
                    self.flagged_ranges.insert(range_key);
                    found_issue = true;
                    break; // Once flagged, stop to avoid noisy duplicates
                }

                // Advance the model
                if let Some(next_model) = self.model_graph.get_relation(&current_model, segment) {
                    current_model = next_model.to_string();
                } else {
                    break;
                }
            }
        }

        found_issue
    }

    /// Extract model name from a type annotation like `QuerySet[Model]` or `Iterable[Model]`
    fn extract_model_from_annotation(&self, annotation: &Expr) -> Option<String> {
        if let Expr::Subscript(subscript) = annotation
            // Check if the base is QuerySet or Iterable
            && let Expr::Name(name) = &*subscript.value
            && (name.id == "QuerySet" || name.id == "Iterable" || name.id == "Iterator")
            // Extract the model from the slice
            && let Expr::Name(model_name) = &*subscript.slice
        {
            return Some(model_name.id.to_string());
        }
        None
    }

    /// Process function parameters to extract QuerySet bindings from type hints
    /// Uses call site info if available for interprocedural analysis
    fn process_function_params(&mut self, func: &ruff_python_ast::StmtFunctionDef) {
        let func_name = func.name.id.to_string();
        let call_info = self.call_sites.get(&func_name);

        for (idx, param) in func
            .parameters
            .args
            .iter()
            .chain(func.parameters.posonlyargs.iter())
            .enumerate()
        {
            if let Some(ref annotation) = param.parameter.annotation
                && let Some(model_name) = self.extract_model_from_annotation(annotation)
            {
                // Check if we have call site info with prefetch state
                let state = if let Some(info) = call_info {
                    if let Some(Some(arg_state)) = info.arg_states.get(idx) {
                        arg_state.clone()
                    } else {
                        QuerySetState::new(model_name)
                    }
                } else {
                    QuerySetState::new(model_name)
                };

                let symbol = DjangoSymbol::QuerySet(state);
                self.symbols.declare(&param.parameter.name.id, symbol);
            }
        }
    }

    /// Collect call site information from a call expression
    fn record_call_site(&mut self, call: &ruff_python_ast::ExprCall) {
        // Get the function name being called
        let func_name = match &*call.func {
            Expr::Name(name) => name.id.to_string(),
            _ => return,
        };

        // Resolve argument states
        let arg_states: Vec<Option<QuerySetState>> = call
            .arguments
            .args
            .iter()
            .map(|arg| {
                if let Some(DjangoSymbol::QuerySet(state)) = self.resolver().resolve(arg) {
                    Some(state)
                } else {
                    None
                }
            })
            .collect();

        self.call_sites
            .insert(func_name, CallSiteInfo { arg_states });
    }

    /// Handle a generator/comprehension by setting up the loop context
    fn process_comprehension_generators(
        &mut self,
        generators: &'a [ruff_python_ast::Comprehension],
    ) {
        for generator in generators {
            let iter_symbol = self.resolver().resolve(&generator.iter);

            if let Some(symbol) = iter_symbol
                && let Expr::Name(target_name) = &generator.target
            {
                let context = match symbol {
                    DjangoSymbol::QuerySet(qs) => Some(LoopContext {
                        loop_var: target_name.id.to_string(),
                        queryset_state: qs,
                    }),
                    DjangoSymbol::ModelInstance(state) => Some(LoopContext {
                        loop_var: target_name.id.to_string(),
                        queryset_state: state,
                    }),
                };

                if let Some(ctx) = context {
                    // Declare the loop variable in the symbol table
                    self.symbols.declare(
                        &ctx.loop_var,
                        DjangoSymbol::ModelInstance(ctx.queryset_state.clone()),
                    );
                    self.active_loops.push(ctx);
                }
            }
        }
    }

    fn pop_comprehension_generators(&mut self, generators: &[ruff_python_ast::Comprehension]) {
        for generator in generators {
            if let Expr::Name(_) = &generator.target
                && !self.active_loops.is_empty()
            {
                self.active_loops.pop();
            }
        }
    }

    /// First pass: collect variable bindings and call site information
    fn collect_call_sites(&mut self, stmt: &'a Stmt) {
        match stmt {
            Stmt::Assign(assign) => {
                for target in &assign.targets {
                    self.record_assignment(target, &assign.value);
                }
                // Check for calls in the assignment value
                self.collect_calls_from_expr(&assign.value);
            }
            Stmt::AnnAssign(assign) => {
                if let Some(ref value) = assign.value {
                    self.record_assignment(&assign.target, value);
                    self.collect_calls_from_expr(value);
                }
            }
            Stmt::Expr(expr_stmt) => {
                self.collect_calls_from_expr(&expr_stmt.value);
            }
            Stmt::FunctionDef(_) | Stmt::ClassDef(_) => {
                // Don't recurse into function/class definitions in first pass
            }
            Stmt::For(for_stmt) => {
                self.collect_calls_from_expr(&for_stmt.iter);
                for stmt in &for_stmt.body {
                    self.collect_call_sites(stmt);
                }
            }
            Stmt::If(if_stmt) => {
                for stmt in &if_stmt.body {
                    self.collect_call_sites(stmt);
                }
                for stmt in &if_stmt.elif_else_clauses {
                    for s in &stmt.body {
                        self.collect_call_sites(s);
                    }
                }
            }
            Stmt::While(while_stmt) => {
                for stmt in &while_stmt.body {
                    self.collect_call_sites(stmt);
                }
            }
            Stmt::With(with_stmt) => {
                for stmt in &with_stmt.body {
                    self.collect_call_sites(stmt);
                }
            }
            Stmt::Try(try_stmt) => {
                for stmt in &try_stmt.body {
                    self.collect_call_sites(stmt);
                }
                for handler in &try_stmt.handlers {
                    let Some(handler) = handler.as_except_handler() else {
                        continue;
                    };
                    for stmt in &handler.body {
                        self.collect_call_sites(stmt);
                    }
                }
            }
            _ => {}
        }
    }

    /// Recursively collect call expressions
    fn collect_calls_from_expr(&mut self, expr: &'a Expr) {
        match expr {
            Expr::Call(call) => {
                self.record_call_site(call);
                // Also check arguments for nested calls
                for arg in &call.arguments.args {
                    self.collect_calls_from_expr(arg);
                }
            }
            Expr::Attribute(attr) => {
                self.collect_calls_from_expr(&attr.value);
            }
            Expr::BinOp(binop) => {
                self.collect_calls_from_expr(&binop.left);
                self.collect_calls_from_expr(&binop.right);
            }
            Expr::List(list) => {
                for elt in &list.elts {
                    self.collect_calls_from_expr(elt);
                }
            }
            Expr::Tuple(tuple) => {
                for elt in &tuple.elts {
                    self.collect_calls_from_expr(elt);
                }
            }
            _ => {}
        }
    }
}

fn build_diagnostic(
    filename: &str,
    source: &str,
    expr: &Expr,
    loop_var: &str,
    attr_chain: &str,
) -> NPlusOneDiagnostic {
    let range = expr.range();
    let start = range.start().to_usize();

    let line = source[..start].chars().filter(|&c| c == '\n').count() + 1;
    let col = start - source[..start].rfind('\n').map(|p| p + 1).unwrap_or(0) + 1;

    NPlusOneDiagnostic::new(
        filename,
        line,
        col,
        DIAGNOSTIC_CODE,
        format!("{loop_var}.{attr_chain}"),
        format!("Potential N+1 query: accessing `{loop_var}.{attr_chain}` inside loop",),
    )
}

impl<'a> Visitor<'a> for NPlusOnePass<'a> {
    fn visit_stmt(&mut self, stmt: &'a Stmt) {
        match stmt {
            Stmt::Assign(assign) => {
                for target in &assign.targets {
                    self.record_assignment(target, &assign.value);
                }
                walk_stmt(self, stmt);
            }
            Stmt::AnnAssign(assign) => {
                if let Some(ref value) = assign.value {
                    self.record_assignment(&assign.target, value);
                }
                walk_stmt(self, stmt);
            }
            Stmt::For(for_stmt) => {
                // Resolve the iterable
                let iter_symbol = self.resolver().resolve(&for_stmt.iter);
                let mut loop_entered = false;

                // Determine if this constitutes a monitored loop
                if let Some(symbol) = iter_symbol
                    && let Expr::Name(target_name) = for_stmt.target.as_ref()
                {
                    let context = match symbol {
                        DjangoSymbol::QuerySet(qs) => Some(LoopContext {
                            loop_var: target_name.id.to_string(),
                            queryset_state: qs,
                        }),
                        DjangoSymbol::ModelInstance(state) => Some(LoopContext {
                            loop_var: target_name.id.to_string(),
                            queryset_state: state,
                        }),
                    };

                    if let Some(ctx) = context {
                        // Declare the loop variable in the symbol table as a ModelInstance
                        self.symbols.declare(
                            &ctx.loop_var,
                            DjangoSymbol::ModelInstance(ctx.queryset_state.clone()),
                        );
                        self.active_loops.push(ctx);
                        loop_entered = true;
                    }
                }

                walk_stmt(self, stmt);

                if loop_entered {
                    self.active_loops.pop();
                }
            }
            Stmt::FunctionDef(func) => {
                self.symbols.push_scope();
                self.process_function_params(func);
                walk_stmt(self, stmt);
                self.symbols.pop_scope();
            }
            Stmt::ClassDef(_) => {
                self.symbols.push_scope();
                walk_stmt(self, stmt);
                self.symbols.pop_scope();
            }
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'a Expr) {
        match expr {
            Expr::Attribute(attr) => {
                let flagged = self.check_n_plus_one_access(expr, attr);
                // If we flagged this access, don't recurse into children
                // to avoid double-counting the same relation access
                if !flagged {
                    walk_expr(self, expr);
                }
            }
            // Handle list comprehensions
            Expr::ListComp(comp) => {
                self.symbols.push_scope();
                self.process_comprehension_generators(&comp.generators);

                // Visit the element expression (where N+1 might occur)
                self.visit_expr(&comp.elt);

                // Visit the iterables and conditions
                for generator in &comp.generators {
                    self.visit_expr(&generator.iter);
                    for condition in &generator.ifs {
                        self.visit_expr(condition);
                    }
                }

                self.pop_comprehension_generators(&comp.generators);
                self.symbols.pop_scope();
            }
            // Handle set comprehensions
            Expr::SetComp(comp) => {
                self.symbols.push_scope();
                self.process_comprehension_generators(&comp.generators);

                self.visit_expr(&comp.elt);

                for generator in &comp.generators {
                    self.visit_expr(&generator.iter);
                    for condition in &generator.ifs {
                        self.visit_expr(condition);
                    }
                }

                self.pop_comprehension_generators(&comp.generators);
                self.symbols.pop_scope();
            }
            // Handle generator expressions
            Expr::Generator(comp) => {
                self.symbols.push_scope();
                self.process_comprehension_generators(&comp.generators);

                self.visit_expr(&comp.elt);

                for generator in &comp.generators {
                    self.visit_expr(&generator.iter);
                    for condition in &generator.ifs {
                        self.visit_expr(condition);
                    }
                }

                self.pop_comprehension_generators(&comp.generators);
                self.symbols.pop_scope();
            }
            // Handle dict comprehensions
            Expr::DictComp(comp) => {
                self.symbols.push_scope();
                self.process_comprehension_generators(&comp.generators);

                self.visit_expr(&comp.key);
                self.visit_expr(&comp.value);

                for generator in &comp.generators {
                    self.visit_expr(&generator.iter);
                    for condition in &generator.ifs {
                        self.visit_expr(condition);
                    }
                }

                self.pop_comprehension_generators(&comp.generators);
                self.symbols.pop_scope();
            }
            _ => walk_expr(self, expr),
        }
    }
}

impl<'a> Pass<'a> for NPlusOnePass<'a> {
    type Output = Vec<NPlusOneDiagnostic>;

    fn run(&mut self, module: &'a ModModule) -> Self::Output {
        // Pass 1: Collect call site information and variable bindings
        // (but don't analyze function bodies for N+1)
        for stmt in &module.body {
            self.collect_call_sites(stmt);
        }

        // Pass 2: Full analysis with call site info available
        for stmt in &module.body {
            self.visit_stmt(stmt);
        }
        std::mem::take(&mut self.diagnostics)
    }
}

#[cfg(test)]
mod tests {
    use crate::passes::functions::QueryFunctionPass;
    use crate::passes::model_graph::ModelGraphPass;

    use super::*;
    use ruff_python_parser::parse_module;

    fn run_pass(source: &str) -> Vec<NPlusOneDiagnostic> {
        let parsed = parse_module(source).expect("should parse");
        let mut graph_pass = ModelGraphPass::new("test.py", source);
        let graph = graph_pass.run(parsed.syntax());
        let mut function_pass = QueryFunctionPass::new();
        let functions = function_pass.run(parsed.syntax());

        let mut pass = NPlusOnePass::new("test.py", source, &graph, &functions);
        pass.run(parsed.syntax())
    }

    #[test]
    fn detect_unsafe_loop() {
        // Exists a relation to App
        let source = r#"
class User(Model):
    related_field = models.ForeignKey("some.App")

qs = User.objects.filter(active=True)
for item in qs:
    print(item.related_field)
"#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("related_field"));
    }

    #[test]
    fn detect_unsafe_with_two_statements() {
        // Exists a relation to App
        let source = r#"
class User(Model):
    related_field = models.ForeignKey("some.App")

qs = User.objects.filter(active=True)
for item in qs:
    print(item.related_field)
    print(item.related_field)
"#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 2);
        assert!(diags[0].message.contains("related_field"));
    }

    #[test]
    fn safe_with_prefetch() {
        let source = r#"
class User(Model):
    related_field = models.ForeignKey("some.App")

qs = User.objects.filter(active=True).prefetch_related('related_field')
for item in qs:
    print(item.related_field)
"#;
        let diags = run_pass(source);
        assert!(diags.is_empty());
    }

    #[test]
    fn warn_on_unprefetched_field() {
        let source = r#"
class User(Model):
    related_field = models.ForeignKey("some.App")

qs = User.objects.filter(active=True).prefetch_related('other_field')
for item in qs:
    print(item.related_field)
"#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn reverse_lookup_detection() {
        let source = r#"
class TheoAnalysis(Model):
    name = models.CharField(max_length=20)

class Performance(Model):
    analysis = models.ForeignKey("some.TheoAnalysis", related_name="relative_performances")

tier1 = TheoAnalysis.objects.filter(id__gt=1)
for t in tier1:
    perfs = t.relative_performances
"#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn nested_loop_detection() {
        let source = r#"
class TheoAnalysis(Model):
    name = models.CharField(max_length=20)

class Performance(Model):
    analysis = models.ForeignKey("some.TheoAnalysis", related_name="relative_performances")

tier1 = TheoAnalysis.objects.filter(id__gt=1)
for t in tier1:
    for rp in t.relative_performances.all():
        pass
"#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn many_to_many_relation_unprefetched_warning() {
        let source = r#"
class TheoAnalysis(Model):
    name = models.CharField(max_length=20)

class Performance(Model):
    analysis = models.ManyToManyField("some.TheoAnalysis", related_name="performances")

analyses = TheoAnalysis.objects.all()

for a in analyses:
    for p in a.performances.all():
        pass
"#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn access_using_relation_detected() {
        let source = r#"
class TheoAnalysis(Model):
    name = models.CharField(max_length=20)

class Performance(Model):
    analysis = models.ManyToManyField("some.TheoAnalysis", related_name="performances")
    pattern = models.ForeignKey(Pattern)

class Pattern(Model):
    name = models.CharField(max_length=20)

a = TheoAnalysis.objects.get(id=1)
performances = a.performances

for p in performances:
    print(p.pattern)
"#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn construct_with_direct_relation_detected() {
        let source = r#"
class TheoAnalysis(Model):
    name = models.CharField(max_length=20)

class Performance(Model):
    analysis = models.ManyToManyField("some.TheoAnalysis", related_name="performances")
    pattern = models.ForeignKey(Pattern)

class Pattern(Model):
    name = models.CharField(max_length=20)

performances = TheoAnalysis.objects.get(id=1).performances

for p in performances:
    print(p.pattern)
"#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn access_using_chained_detected() {
        let source = r#"
class TheoAnalysis(Model):
    pattern = models.ForeignKey("app.Pattern")

class Performance(Model):
    analysis = models.ForeignKey("some.TheoAnalysis")

class Pattern(Model):
    name = models.CharField(max_length=20)


performances = Performance.objects.select_related("analysis").all()

for p in performances:
    # analysis is ok
    # pattern is N+1
    print(p.analysis.pattern)
"#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn second_level_related_access() {
        let source = r#"
class TheoAnalysis(models.Model):
    tier = models.CharField(max_length=11, choices=Tiers.choices)

class Tier1(models.Model):
    theo_analysis = models.ForeignKey("theo.TheoAnalysis", on_delete=models.CASCADE, related_name="tier1s", null=True)

class Tier1RelativePerformance(models.Model):
    tier1 = models.ForeignKey("theo.Tier1", on_delete=models.CASCADE, related_name="relative_performances")

theo_analysis = TheoAnalysis.objects.filter(id=analysis_id, tier=Tiers.TIER1).first()
tier1 = theo_analysis.tier1s.filter(
    failed_reasons__isnull=True).select_related('ticker').prefetch_related('relative_performances')

for t in tier1:
    for rp in t.relative_performances.all():
        pass
        "#;

        let diags = run_pass(source);
        assert!(diags.is_empty());
    }

    #[test]
    fn detect_traceback_relation_access() {
        let source = r#"
class TheoAnalysis(models.Model):
    tier = models.CharField(max_length=11, choices=Tiers.choices)

class Tier1(models.Model):
    analysis = models.ForeignKey("theo.TheoAnalysis", on_delete=models.CASCADE, related_name="tier1s", null=True)

def foo(t1: QuerySet[Tier1]):
    for t in t1:
        print(t.analysis)

tier1 = Tier1.objects.all()
foo(tier1)
        "#;

        let diags = run_pass(source);
        assert!(!diags.is_empty());
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn no_capture_valid_traceback_relation_access() {
        let source = r#"
class TheoAnalysis(models.Model):
    tier = models.CharField(max_length=11, choices=Tiers.choices)

class Tier1(models.Model):
    analysis = models.ForeignKey("theo.TheoAnalysis", on_delete=models.CASCADE, related_name="tier1s", null=True)

def foo(t1: QuerySet[Tier1]):
    for t in t1:
        print(t.analysis)

tier1 = Tier1.objects.select_related("analysis").all()
foo(tier1)
        "#;

        let diags = run_pass(source);
        assert!(diags.is_empty());
    }

    #[test]
    fn detect_traceback_using_iterable() {
        let source = r#"
class TheoAnalysis(models.Model):
    tier = models.CharField(max_length=11, choices=Tiers.choices)

class Tier1(models.Model):
    analysis = models.ForeignKey("theo.TheoAnalysis", on_delete=models.CASCADE, related_name="tier1s", null=True)

def foo(t1: Iterable[Tier1]):
    for t in t1:
        print(t.analysis)

tier1 = Tier1.objects.all()
foo(tier1)
    "#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn detect_n1_after_get_object_or_404() {
        let source = r#"
class User(Model):
    pass

class Order(Model):
    user = models.ForeignKey(User, related_name="orders")

user = get_object_or_404(User, id=1)
orders = user.orders.all()

for order in orders:
    print(order)  # Should NOT warn (no relation access)
    print(order.user)  # Should warn (N+1)
"#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn detect_n1_directly_all_in_for() {
        let source = r#"
class User(Model):
    pass

class Order(Model):
    user = models.ForeignKey(User, related_name="orders")

user = User.objects.get(id=1)
for order in user.orders.all():
    print(order)  # Should NOT warn (no relation access)
    print(order.user)  # Should warn (N+1)
    "#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn detect_n1_nested_relation_access_in_for() {
        let source = r#"
class User(Model):
    pass

class Method(Model):
    pass

class Payment(Model):
    user = models.ForeignKey(User, related_name="payments")
    method = models.ForeignKey(Method, related_name="payments")

class Order(Model):
    users = models.ManyToManyField(User, related_name="orders")

orders = Order.objects.all()
for user in orders.users:
    for payment in user.payments:
        print(payment.method)
    "#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 2);
    }

    #[test]
    fn detect_n1_in_list() {
        let source = r#"
class User(Model):
    pass

class Order(Model):
    user = models.ForeignKey(User, related_name="orders")

orders = list(Order.objects.all())
for order in orders:
    print(order.user)
    "#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn detect_n1_directly_in_for() {
        let source = r#"
class User(Model):
    pass

class Order(Model):
    user = models.ForeignKey(User, related_name="orders")

user = User.objects.get(id=1)
for order in user.orders:
    print(order)  # Should NOT warn (no relation access)
    print(order.user)  # Should warn (N+1)
    "#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn detect_n1_filter_directly_in_for() {
        let source = r#"
class User(Model):
    pass

class Order(Model):
    user = models.ForeignKey(User, related_name="orders")

user = User.objects.get(id=1)
for order in user.orders.filter(id__lt=10):
    print(order)  # Should NOT warn (no relation access)
    print(order.user)  # Should warn (N+1)
    "#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn detect_n1_using_iterator() {
        let source = r#"
class User(Model):
    pass

class Order(Model):
    user = models.ForeignKey(User, related_name="orders")

orders = Order.objects.all()
for order in orders.iterator():
    print(order.user)
        "#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn no_detect_valid_n1_using_iterator() {
        let source = r#"
class User(Model):
    pass

class Order(Model):
    user = models.ForeignKey(User, related_name="orders")

orders = Order.objects.select_related("user").all()
for order in orders.iterator():
    print(order.user)
    "#;
        let diags = run_pass(source);
        assert!(diags.is_empty());
    }

    #[test]
    fn detect_n1_using_comprehension() {
        let source = r#"
class User(Model):
    pass

class Order(Model):
    user = models.ForeignKey(User, related_name="orders")

orders = Order.objects.all()
print([order.user for order in orders])
        "#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 1);
    }
}
