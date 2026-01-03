//! N+1 query detection pass.
//!
//! Detects potential N+1 query patterns in Django code by tracking:
//! - QuerySet bindings and their prefetch state
//! - Loop iteration over querysets
//! - Attribute access on loop variables that would trigger additional queries

use ruff_python_ast::{Expr, ModModule, Stmt, visitor::Visitor};
use ruff_text_size::Ranged;
use std::collections::HashMap;

use crate::diagnostic::NPlusOneDiagnostic;
use crate::ir::binding::{BindingKind, QuerySetState};
use crate::ir::model::{ModelDef, ModelGraph};
use crate::passes::extract_attribute_chain;
use crate::passes::{Pass, functions::QueryFunction};

const DIAGNOSTIC_CODE: &str = "N+1";

pub(crate) struct ScopeManager {
    scopes: Vec<ScopeState>,
}

impl ScopeManager {
    pub fn new() -> Self {
        Self {
            scopes: vec![ScopeState::new()],
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(ScopeState::new());
    }

    fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    fn insert_in_current_scope(&mut self, name: String, kind: BindingKind) {
        self.scopes
            .iter_mut()
            .last()
            .map(|s| s.symbols.insert(name, kind));
    }

    fn lookup<'a>(&'a self, name: &str) -> Option<&'a BindingKind> {
        for scope in self.scopes.iter().rev() {
            if let Some(kind) = scope.symbols.get(name) {
                return Some(kind);
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
struct LoopContext {
    loop_var: String,
    queryset_state: QuerySetState,
}

#[derive(Debug)]
struct ScopeState {
    symbols: HashMap<String, BindingKind>,
}

impl ScopeState {
    fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }
}

pub struct QuerySetClassifier<'a> {
    scope_manager: &'a ScopeManager,
}

impl<'a> QuerySetClassifier<'a> {
    pub fn new(scope_manager: &'a ScopeManager) -> Self {
        Self { scope_manager }
    }

    fn classify_expr(&self, expr: &Expr, model_graph: &ModelGraph) -> BindingKind {
        let mut curr_expr = expr;
        let mut model = None;

        if let Expr::Call(call) = expr
            && let Expr::Name(func_name) = call.func.as_ref()
            && func_name.id.ends_with("get_object_or_404") // matches the async version too
             // First arg is the model class
            && let Some(Expr::Name(model_name)) = call.arguments.args.first()
        {
            return BindingKind::ModelInstance(model_name.id.to_string());
        }

        let mut calls = Vec::new();

        loop {
            match curr_expr {
                Expr::Call(call) => {
                    calls.push(call);

                    if let Some(name) = call.func.as_name_expr()
                        && name.id == "list"
                    {
                        // Capture when the queryset is wrapped in a list,
                        // like list(User.objects.all())
                        curr_expr = call.arguments.args.first().unwrap_or(&call.func);
                        continue;
                    }

                    curr_expr = &call.func;
                }
                Expr::Attribute(attr) => {
                    curr_expr = &attr.value;
                    if model.is_some() {
                        continue; // Already captured
                    }
                    let (base, chain) = extract_attribute_chain(attr);
                    model = model_graph.get(base);

                    if model.is_some() {
                        // Check for chained access if a model name
                        // was detected
                        for expr in chain.iter() {
                            if model_graph.is_relation(base, expr) {
                                model = model_graph
                                    .get_relation(base, expr)
                                    .and_then(|m| model_graph.get(m));
                                break;
                            }
                        }
                    }

                    if model.is_none()
                        && let Some(captured) = self.scope_manager.lookup(base)
                    {
                        match captured {
                            BindingKind::QuerySet(state) => {
                                let mut state = state.clone();
                                calls.iter().for_each(|c| state.apply_call(c));

                                if let Some(related_model) =
                                    model_graph.get(state.model_name.as_str())
                                {
                                    model = self.try_get_related_model(
                                        &chain,
                                        &related_model.name,
                                        model_graph,
                                    );

                                    // Skip if the model has been found in the last step
                                    if model.is_some() {
                                        continue;
                                    }

                                    return captured.clone();
                                }
                            }
                            BindingKind::ModelInstance(instance) => {
                                model = self.try_get_related_model(&chain, instance, model_graph);

                                // Skip if the model was found in the last step
                                if model.is_some() {
                                    continue;
                                }

                                model = model_graph
                                    .get_relation(base, attr.attr.id.as_str())
                                    .and_then(|m| model_graph.get(m));
                            }
                            _ => {}
                        }
                    }
                }
                Expr::Name(name) => {
                    if model.is_none()
                        && let Some(kind) = self.scope_manager.lookup(name.id.as_str())
                        && let BindingKind::QuerySet(state) = kind
                    {
                        let mut state = state.clone();
                        calls.iter().for_each(|c| state.apply_call(c));
                        return BindingKind::QuerySet(state);
                    };

                    break;
                }
                _ => return BindingKind::Unknown,
            }
        }

        let Some(model) = model else {
            return BindingKind::Unknown;
        };

        let mut state = QuerySetState::new(model.name.clone());

        calls.iter().for_each(|c| state.apply_call(c));

        BindingKind::QuerySet(state)
    }

    fn try_get_related_model(
        &self,
        chain: &[&str],
        model_name: &str,
        model_graph: &'a ModelGraph,
    ) -> Option<&'a ModelDef> {
        let mut model = None;
        // Check for chained access if a model variable
        // was detected
        for expr in chain.iter() {
            if model_graph.is_relation(model_name, expr) {
                model = model_graph
                    .get_relation(model_name, expr)
                    .and_then(|m| model_graph.get(m));
                break;
            }
        }

        model
    }
}

pub struct NPlusOnePass<'a> {
    filename: &'a str,
    source: &'a str,
    scope_manager: ScopeManager,
    model_graph: &'a ModelGraph,
    active_loops: Vec<LoopContext>,
    functions: &'a [QueryFunction],
    diagnostics: Vec<NPlusOneDiagnostic>,
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
            scope_manager: ScopeManager::new(),
            active_loops: Vec::new(),
            diagnostics: Vec::new(),
            functions,
        }
    }

    fn record_assignment(&mut self, target: &'a Expr, value: &'a Expr) {
        let classifier = QuerySetClassifier::new(&self.scope_manager);
        let kind = classifier.classify_expr(value, self.model_graph);
        if let Expr::Name(name) = target {
            self.scope_manager
                .insert_in_current_scope(name.id.to_string(), kind);
        }
    }

    fn make_diagnostic(&self, expr: &Expr, loop_var: &str, attr_name: &str) -> NPlusOneDiagnostic {
        let range = expr.range();
        let start = range.start().to_usize();

        let line = self.source[..start].chars().filter(|&c| c == '\n').count() + 1;
        let col = start - self.source[..start].rfind('\n').map(|p| p + 1).unwrap_or(0) + 1;

        NPlusOneDiagnostic::new(
            self.filename,
            line,
            col,
            DIAGNOSTIC_CODE,
            format!("{}.{}", loop_var, attr_name),
            format!(
                "Potential N+1 query: accessing `{}.{}` inside loop",
                loop_var, attr_name
            ),
        )
    }

    fn check_relation_chain(&self, state: &QuerySetState, chain: &[&'a str]) -> bool {
        let Some(model_name) = self.model_graph.get(&state.model_name) else {
            return false;
        };

        let mut current_model = model_name;

        for attr in chain.iter() {
            let is_relation = self.model_graph.is_relation(&current_model.name, attr);

            let should_warn = is_relation && !state.is_access_safe(attr);
            if should_warn {
                return true;
            }

            let Some(new_current_model) = self
                .model_graph
                .get_relation(&current_model.name, attr)
                .and_then(|m| self.model_graph.get(m))
            else {
                return false;
            };

            current_model = new_current_model;
        }

        false
    }
}

impl<'a> Pass<'a> for NPlusOnePass<'a> {
    type Output = Vec<NPlusOneDiagnostic>;

    fn run(&mut self, module: &'a ModModule) -> Self::Output {
        for stmt in &module.body {
            self.visit_stmt(stmt);
        }
        std::mem::take(&mut self.diagnostics)
    }
}

impl<'a> Visitor<'a> for NPlusOnePass<'a> {
    fn visit_stmt(&mut self, stmt: &'a Stmt) {
        match stmt {
            Stmt::Assign(assign) => {
                for target in &assign.targets {
                    self.record_assignment(target, &assign.value);
                }
                ruff_python_ast::visitor::walk_stmt(self, stmt);
            }
            Stmt::AnnAssign(assign) => {
                if let Some(ref value) = assign.value {
                    self.record_assignment(&assign.target, value);
                }
                ruff_python_ast::visitor::walk_stmt(self, stmt);
            }
            Stmt::For(for_stmt) => {
                let mut pushed_loop = false;

                let mut expr = for_stmt.iter.as_ref();
                let mut curr_qs = None;
                let mut is_call = false;

                loop {
                    match expr {
                        Expr::Name(iter_name) => {
                            if let Expr::Name(target) = for_stmt.target.as_ref() {
                                if curr_qs.is_none() {
                                    curr_qs =
                                        self.scope_manager.lookup(iter_name.id.as_str()).cloned();
                                }

                                match &curr_qs {
                                    Some(BindingKind::QuerySet(context)) => {
                                        self.active_loops.push(LoopContext {
                                            loop_var: target.id.to_string(),
                                            queryset_state: context.clone(),
                                        });
                                        pushed_loop = true;
                                    }
                                    Some(BindingKind::ModelInstance(instance)) => {
                                        self.active_loops.push(LoopContext {
                                            loop_var: target.id.to_string(),
                                            queryset_state: QuerySetState::new(instance.clone()),
                                        });
                                        pushed_loop = true;
                                    }
                                    _ => {}
                                }
                            }
                            break;
                        }
                        // In both cases are not necessary to record the classification because
                        // are temporary values in the `for` and we call walk_stmt here, then
                        // drop the classification is ok
                        Expr::Call(call) => {
                            let classifier = QuerySetClassifier::new(&self.scope_manager);
                            // Ensure the data is stored in cases when for is in the form
                            // `for order in user.orders.all(): ... where user is an instance of User
                            curr_qs = Some(classifier.classify_expr(expr, self.model_graph));
                            expr = &call.func;
                            is_call = true;
                        }
                        Expr::Attribute(attr) => {
                            let classifier = QuerySetClassifier::new(&self.scope_manager);
                            // Ensure the data is stored in cases when for is in the form
                            // `for order in user.orders: ... where user is an instance of User
                            // if expr is a Call, that can be in the form user.orders.filter(..)
                            if !is_call {
                                curr_qs = Some(classifier.classify_expr(expr, self.model_graph));
                            }

                            expr = &attr.value
                        }
                        _ => break,
                    }
                }

                ruff_python_ast::visitor::walk_stmt(self, stmt);

                if pushed_loop {
                    self.active_loops.pop();
                }
            }
            Stmt::FunctionDef(_) | Stmt::ClassDef(_) => {
                self.scope_manager.push_scope();
                ruff_python_ast::visitor::walk_stmt(self, stmt);
                self.scope_manager.pop_scope();
            }
            _ => ruff_python_ast::visitor::walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'a Expr) {
        match expr {
            Expr::Attribute(attr) => {
                let (base_name, attr_chain) = extract_attribute_chain(attr);

                for ctx in &self.active_loops {
                    if base_name != ctx.loop_var {
                        continue;
                    }

                    let is_n_plus_one = self.check_relation_chain(&ctx.queryset_state, &attr_chain);

                    if is_n_plus_one {
                        self.diagnostics.push(self.make_diagnostic(
                            expr,
                            &ctx.loop_var,
                            &attr_chain.join("."),
                        ));
                    }
                }
                return;
            }
            Expr::Call(call) => {
                let mut diagnostics = Vec::new();
                // Detect if a query set is passed to a function
                // and the function expect a queryset, also the function
                // expects a queryset in that parameter
                for arg in call.arguments.args.iter() {
                    if let Expr::Name(name) = arg
                        && let Some(bind) = self.scope_manager.lookup(&name.id)
                        && let Some(f) = self.functions.iter().find(|f| {
                            call.func
                                .as_name_expr()
                                .is_some_and(|fname| fname.id.as_str() == f.name)
                        })
                        && let Some((arg_idx, _)) = call
                            .arguments
                            .args
                            .iter()
                            .enumerate()
                            .find(|(_, a)| a.as_name_expr().is_some_and(|n| n.id == name.id))
                        && let BindingKind::QuerySet(state) = bind
                    {
                        // If it is safe, look for a no prefetched relation
                        for a in f.args.iter() {
                            if a.idx == arg_idx
                                && a.attr_accesses.iter().any(|aa| {
                                    self.model_graph.is_relation(&a.model_name, aa)
                                        && !state.is_access_safe(aa)
                                })
                            {
                                diagnostics.push(self.make_diagnostic(expr, &name.id, &a.var_name));
                            }
                        }
                    }
                }
                if !diagnostics.is_empty() {
                    self.diagnostics.append(&mut diagnostics);
                }
            }

            _ => {}
        }
        ruff_python_ast::visitor::walk_expr(self, expr);
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

    //     #[test]
    //     fn detect_traceback_using_iterable() {
    //         let source = r#"
    // class TheoAnalysis(models.Model):
    //     tier = models.CharField(max_length=11, choices=Tiers.choices)
    //
    // class Tier1(models.Model):
    //     analysis = models.ForeignKey("theo.TheoAnalysis", on_delete=models.CASCADE, related_name="tier1s", null=True)
    //
    // def foo(t1: Iterable[Tier1]):
    //     for t in t1:
    //         print(t.analysis)
    //
    // tier1 = Tier1.objects.all()
    // foo(tier1)
    //     "#;
    //         let diags = run_pass(source);
    //         assert_eq!(diags.len(), 1);
    //     }
    //
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

    //     #[test]
    //     fn detect_n1_nested_relation_access_in_for() {
    //         let source = r#"
    // class User(Model):
    //     pass
    //
    // class Method(Model):
    //     pass
    //
    // class Payment(Model):
    //     user = models.ForeignKey(User, related_name="payments")
    //     method = models.ForeignKey(Method, related_name="payments")
    //
    // class Order(Model):
    //     users = models.ManyToManyField(User, related_name="orders")
    //
    // orders = Order.objects.all()
    // for user in orders.users:
    //     for payment in user.payments:
    //         print(payment.method)
    //     "#;
    //         let diags = run_pass(source);
    //         assert_eq!(diags.len(), 1);
    //     }

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

    //     #[test]
    //     fn detect_n1_using_comprehension() {
    //         let source = r#"
    // class User(Model):
    //     pass
    //
    // class Order(Model):
    //     user = models.ForeignKey(User, related_name="orders")
    //
    // orders = Order.objects.all()
    // print([order.user for order in orders])
    //     "#;
    //         let diags = run_pass(source);
    //         assert_eq!(diags.len(), 1);
    //     }
}
