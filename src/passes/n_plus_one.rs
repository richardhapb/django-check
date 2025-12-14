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
use crate::ir::binding::{BindingKind, QuerySetState, SafeMethod, parse_relation_fields};
use crate::passes::Pass;

const DIAGNOSTIC_CODE: &str = "N+1";

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

pub struct NPlusOnePass<'a> {
    filename: &'a str,
    source: &'a str,
    scopes: Vec<ScopeState>,
    active_loops: Vec<LoopContext>,
    diagnostics: Vec<Diagnostic>,
}

impl<'a> NPlusOnePass<'a> {
    pub fn new(filename: &'a str, source: &'a str) -> Self {
        Self {
            filename,
            source,
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

    fn record_assignment(&mut self, target: &Expr, value: &Expr) {
        if let Expr::Name(name) = target {
            let kind = self.classify_expr(value);
            self.current_scope_mut()
                .symbols
                .insert(name.id.to_string(), kind);
        }
    }

    fn classify_expr(&self, expr: &Expr) -> BindingKind {
        self.classify_expr_inner(expr, &mut Vec::new())
    }

    fn classify_expr_inner(&self, expr: &'a Expr, chain: &mut Vec<&'a Expr>) -> BindingKind {
        match expr {
            Expr::Call(call) => {
                chain.push(expr);
                self.classify_expr_inner(&call.func, chain)
            }
            Expr::Attribute(attr) => {
                if attr.attr.id.as_str() == "objects" {
                    let method = self.get_safe_method(chain);
                    let state = match method {
                        Some(m) => QuerySetState::Safe(m),
                        None => QuerySetState::Unsafe,
                    };
                    BindingKind::QuerySet(state)
                } else {
                    self.classify_expr_inner(&attr.value, chain)
                }
            }
            Expr::Name(name) => {
                if let Some(kind) = self.lookup(name.id.as_str()) {
                    match kind {
                        BindingKind::QuerySet(_) if self.get_safe_method(chain).is_some() => {
                            BindingKind::QuerySet(QuerySetState::Safe(
                                self.get_safe_method(chain).unwrap(),
                            ))
                        }
                        _ => kind.clone(),
                    }
                } else {
                    BindingKind::Unknown
                }
            }
            _ => BindingKind::Unknown,
        }
    }

    fn get_safe_method(&self, chain: &[&Expr]) -> Option<SafeMethod> {
        const SAFE_QS_METHODS: [&str; 2] = ["select_related", "prefetch_related"];
        const SAFE_NO_QS_METHODS: [&str; 3] = ["values", "values_list", "iterator"];

        let mut safe_method = None;

        for expr in chain {
            let call = expr.as_call_expr()?;
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
                    && let Some(BindingKind::QuerySet(state)) = self.lookup(iter_name.id.as_str())
                    && let Expr::Name(target) = for_stmt.target.as_ref()
                {
                    self.active_loops.push(LoopContext {
                        loop_var: target.id.to_string(),
                        queryset_state: state.clone(),
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
        if let Expr::Attribute(attr) = expr
            && let Expr::Name(name) = attr.value.as_ref()
        {
            let attr_name = attr.attr.id.as_str();

            for ctx in &self.active_loops {
                if name.id.as_str() != ctx.loop_var {
                    continue;
                }

                let should_warn = match &ctx.queryset_state {
                    QuerySetState::Unsafe => true,
                    QuerySetState::Safe(method) => {
                        !method.prefetched_relations.iter().any(|a| a == attr_name)
                    }
                };

                if should_warn {
                    self.diagnostics
                        .push(self.make_diagnostic(expr, &ctx.loop_var, attr_name));
                }
            }
        }

        ruff_python_ast::visitor::walk_expr(self, expr);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ruff_python_parser::parse_module;

    fn run_pass(source: &str) -> Vec<Diagnostic> {
        let parsed = parse_module(source).expect("should parse");
        let mut pass = NPlusOnePass::new("test.py", source);
        pass.run(parsed.syntax())
    }

    #[test]
    fn detect_unsafe_loop() {
        let source = r#"
qs = Model.objects.filter(active=True)
for item in qs:
    print(item.related_field)
"#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("related_field"));
    }

    #[test]
    fn safe_with_prefetch() {
        let source = r#"
qs = Model.objects.filter(active=True).prefetch_related('related_field')
for item in qs:
    print(item.related_field)
"#;
        let diags = run_pass(source);
        assert!(diags.is_empty());
    }

    #[test]
    fn warn_on_unprefetched_field() {
        let source = r#"
qs = Model.objects.filter(active=True).prefetch_related('other_field')
for item in qs:
    print(item.related_field)
"#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn nested_loop_detection() {
        let source = r#"
tier1 = TheoAnalysis.objects.filter(id=1).first().tier1s.all()
for t in tier1:
    for rp in t.relative_performances.all():
        pass
"#;
        let diags = run_pass(source);
        assert_eq!(diags.len(), 1);
    }
}
