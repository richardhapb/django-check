//! N+1 query detection pass.
//!
//! Detects potential N+1 query patterns in Django code by tracking:
//! - QuerySet bindings and their prefetch state
//! - Loop iteration over querysets
//! - Attribute access on loop variables that would trigger additional queries

use ruff_python_ast::{Expr, ModModule, Stmt, visitor::Visitor};
use ruff_text_size::Ranged;
use std::collections::HashMap;

use crate::diagnostic::Diagnostic;
use crate::ir::binding::{
    BindingKind, QuerySetContext, QuerySetState, SafeMethod, parse_relation_fields,
};
use crate::ir::model::ModelGraph;
use crate::passes::Pass;

const DIAGNOSTIC_CODE: &str = "N+1";

#[derive(Debug, Clone)]
struct LoopContext {
    loop_var: String,
    queryset_context: QuerySetContext,
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

pub struct NPlusOnePass<'a> {
    filename: &'a str,
    source: &'a str,
    scopes: Vec<ScopeState>,
    model_graph: &'a ModelGraph,
    active_loops: Vec<LoopContext>,
    diagnostics: Vec<Diagnostic>,
}

impl<'a> NPlusOnePass<'a> {
    pub fn new(filename: &'a str, source: &'a str, model_graph: &'a ModelGraph) -> Self {
        Self {
            filename,
            source,
            model_graph,
            scopes: vec![ScopeState::new()],
            active_loops: Vec::new(),
            diagnostics: Vec::new(),
        }
    }

    fn current_scope_mut(&mut self) -> &mut ScopeState {
        self.scopes
            .last_mut()
            .expect("scope stack should never be empty")
    }

    fn push_scope(&mut self) {
        self.scopes.push(ScopeState::new());
    }

    fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    fn lookup(&self, name: &str) -> Option<&BindingKind> {
        for scope in self.scopes.iter().rev() {
            if let Some(kind) = scope.symbols.get(name) {
                return Some(kind);
            }
        }
        None
    }

    fn record_assignment(&mut self, target: &'a Expr, value: &'a Expr) {
        let kind = self.classify_expr(value);
        if let Expr::Name(name) = target {
            self.current_scope_mut()
                .symbols
                .insert(name.id.to_string(), kind);
        }
    }

    fn classify_expr(&self, expr: &'a Expr) -> BindingKind {
        let mut safe_methods = Vec::new();
        let mut curr_expr = expr;
        let mut model = None;

        loop {
            match curr_expr {
                Expr::Call(call) => {
                    if let Some(safe_method) = self.get_safe_method(call) {
                        safe_methods.push(safe_method);
                    }

                    curr_expr = &call.func;
                }
                Expr::Attribute(attr) => {
                    curr_expr = &attr.value;
                    if model.is_some() {
                        continue; // Already captured
                    }
                    let (base, chain) = self.extract_attribute_chain(attr);
                    model = self.model_graph.get(&base);

                    if model.is_some() {
                        // Check for chained access if a model name
                        // was detected
                        for expr in chain.iter() {
                            if self.model_graph.is_relation(&base, expr) {
                                model = self
                                    .model_graph
                                    .get_relation(&base, expr)
                                    .and_then(|m| self.model_graph.get(m));
                                break;
                            }
                        }
                    }

                    if model.is_none()
                        && let Some(captured) = self.lookup(&base)
                    {
                        match captured {
                            BindingKind::QuerySet(ctx) => {
                                if let Some(related_model) = ctx
                                    .model
                                    .as_ref()
                                    .and_then(|m| self.model_graph.get(m.as_str()))
                                {
                                    // Check for chained access if a model variable
                                    // was detected
                                    for expr in chain.iter() {
                                        if self.model_graph.is_relation(&related_model.name, expr) {
                                            model = self
                                                .model_graph
                                                .get_relation(&related_model.name, expr)
                                                .and_then(|m| self.model_graph.get(m));
                                            break;
                                        }
                                    }

                                    // Skip if the model was found in the last step
                                    if model.is_some() {
                                        continue;
                                    }

                                    model = self
                                        .model_graph
                                        .get_relation(&base, attr.attr.id.as_str())
                                        .and_then(|m| self.model_graph.get(m));
                                }
                            }
                            BindingKind::Unknown => {}
                        }
                    }
                }
                Expr::Name(name) => {
                    if model.is_none()
                        && let Some(kind) = self.lookup(name.id.as_str())
                    {
                        match kind {
                            BindingKind::QuerySet(ctx) => {
                                let state = match ctx.state {
                                    QuerySetState::Safe(_) => {
                                        return BindingKind::QuerySet(QuerySetContext {
                                            state: QuerySetState::Safe(safe_methods),
                                            model: ctx.model.clone(),
                                        });
                                    }
                                    QuerySetState::Unsafe => QuerySetState::Unsafe,
                                };
                                return BindingKind::QuerySet(QuerySetContext {
                                    state,
                                    model: model.map(|m| m.name.clone()),
                                });
                            }
                            BindingKind::Unknown => {}
                        };
                    }
                    break;
                }
                _ => break,
            }
        }

        let Some(model_instance) = model else {
            return BindingKind::Unknown;
        };

        let state = if safe_methods.is_empty() {
            QuerySetState::Unsafe
        } else {
            QuerySetState::Safe(safe_methods)
        };

        BindingKind::QuerySet(QuerySetContext {
            state,
            model: Some(model_instance.name.clone()),
        })
    }

    fn get_safe_method(&self, call: &ruff_python_ast::ExprCall) -> Option<SafeMethod> {
        const SAFE_QS_METHODS: [&str; 2] = ["select_related", "prefetch_related"];
        const SAFE_NO_QS_METHODS: [&str; 3] = ["values", "values_list", "iterator"];

        let mut safe_method = None;

        let attr = call.func.as_attribute_expr()?;
        let name = attr.attr.id.as_str();

        if SAFE_NO_QS_METHODS.contains(&name) {
            return Some(SafeMethod {
                name: name.to_string(),
                prefetched_relations: Vec::new(),
            });
        }

        if SAFE_QS_METHODS.contains(&name) {
            let fields: Vec<String> = call
                .arguments
                .args
                .iter()
                .filter_map(|a| a.as_string_literal_expr())
                .map(|s| s.value.to_string())
                .collect();

            safe_method = Some(SafeMethod {
                name: name.to_string(),
                prefetched_relations: parse_relation_fields(&fields),
            });
        }

        safe_method
    }

    fn make_diagnostic(&self, expr: &Expr, loop_var: &str, attr_name: &str) -> Diagnostic {
        let range = expr.range();
        let start = range.start().to_usize();

        let line = self.source[..start].chars().filter(|&c| c == '\n').count() + 1;
        let col = start - self.source[..start].rfind('\n').map(|p| p + 1).unwrap_or(0) + 1;

        Diagnostic::new(
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

    /// Extract the attribute chain from a complete sentence
    ///
    /// Args
    ///     attr: The attribute where begin to backward for capturing the chain
    ///
    /// Returns
    ///     tuple with the base element of the sentence and all the posterior expressions
    fn extract_attribute_chain(
        &self,
        attr: &ruff_python_ast::ExprAttribute,
    ) -> (String, Vec<String>) {
        let mut chain = vec![attr.attr.id.to_string()];
        let mut current = attr.value.as_ref();

        loop {
            match current {
                Expr::Attribute(attr) => {
                    chain.push(attr.attr.id.to_string());
                    current = attr.value.as_ref();
                }
                Expr::Call(call) => {
                    current = call.func.as_ref();
                }
                Expr::Name(name) => {
                    chain.reverse();
                    return (name.id.to_string(), chain);
                }
                _ => return (String::new(), Vec::new()),
            }
        }
    }

    fn check_relation_chain(&self, ctx: &QuerySetContext, chain: &[String]) -> bool {
        let Some(ref model_name) = ctx.model else {
            return false;
        };

        let mut current_model = model_name.as_str();

        for attr in chain.iter() {
            let is_relation = self.model_graph.is_relation(current_model, attr);

            let should_warn = match &ctx.state {
                QuerySetState::Unsafe => is_relation,
                QuerySetState::Safe(methods) => {
                    is_relation
                        && !methods
                            .iter()
                            .any(|p| p.prefetched_relations.iter().any(|a| a == attr))
                }
            };

            if should_warn {
                return true;
            }

            let Some(new_current_model) = self.model_graph.get_relation(current_model, attr) else {
                return false;
            };

            current_model = new_current_model;
        }

        false
    }
}

impl<'a> Pass<'a> for NPlusOnePass<'a> {
    type Output = Vec<Diagnostic>;

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

                if let Expr::Name(iter_name) = for_stmt.iter.as_ref()
                    && let Some(BindingKind::QuerySet(context)) = self.lookup(iter_name.id.as_str())
                    && let Expr::Name(target) = for_stmt.target.as_ref()
                {
                    self.active_loops.push(LoopContext {
                        loop_var: target.id.to_string(),
                        queryset_context: context.clone(),
                    });
                    pushed_loop = true;
                }

                ruff_python_ast::visitor::walk_stmt(self, stmt);

                if pushed_loop {
                    self.active_loops.pop();
                }
            }
            Stmt::FunctionDef(_) | Stmt::ClassDef(_) => {
                self.push_scope();
                ruff_python_ast::visitor::walk_stmt(self, stmt);
                self.pop_scope();
            }
            _ => {
                ruff_python_ast::visitor::walk_stmt(self, stmt);
            }
        }
    }

    fn visit_expr(&mut self, expr: &'a Expr) {
        if let Expr::Attribute(attr) = expr {
            let (base_name, attr_chain) = self.extract_attribute_chain(attr);

            for ctx in &self.active_loops {
                if base_name != ctx.loop_var {
                    continue;
                }

                let is_n_plus_one = self.check_relation_chain(&ctx.queryset_context, &attr_chain);

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
        ruff_python_ast::visitor::walk_expr(self, expr);
    }
}

#[cfg(test)]
mod tests {
    use crate::passes::model_graph::ModelGraphPass;

    use super::*;
    use ruff_python_parser::parse_module;

    fn run_pass(source: &str) -> Vec<Diagnostic> {
        let parsed = parse_module(source).expect("should parse");
        let mut graph_pass = ModelGraphPass::new("test.py", source);
        let graph = graph_pass.run(parsed.syntax());

        let mut pass = NPlusOnePass::new("test.py", source, &graph);
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
}
