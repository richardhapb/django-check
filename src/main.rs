use ruff_python_ast::ModModule;
use ruff_python_ast::{Expr, Stmt, visitor::Visitor};
use ruff_python_parser::{Parsed, parse_module};
use ruff_text_size::Ranged;
use std::collections::HashMap;
use std::env::home_dir;
use std::fs;
use std::path::Path;
use walkdir::WalkDir;

fn main() {
    let home = home_dir().unwrap();
    let path = home.join("dev/agora_hedge/main/app");
    let parser = Parser::new();

    if let Err(e) = parser.analyze_directory(&path) {
        eprintln!("Error: {}", e);
    }
}

#[derive(Debug, Clone)]
enum QuerySetState {
    Safe,
    Unsafe,
}

#[derive(Debug, Clone)]
enum BindingKind {
    QuerySet(QuerySetState),
    Unknown,
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

#[derive(Debug, Clone)]
struct LoopContext {
    loop_var: String,
    unsafe_queryset: bool,
}

#[derive(Debug)]
struct Diagnostic {
    filename: String,
    line: usize,
    col: usize,
    access: String,
    message: String,
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}: --> {} \n{}",
            self.filename, self.line, self.col, self.access, self.message
        )
    }
}

struct Analyzer<'a> {
    filename: String,
    source: &'a str,
    scopes: Vec<ScopeState>,
    active_loops: Vec<LoopContext>,
    diagnostics: Vec<Diagnostic>,
}

impl<'a> Analyzer<'a> {
    fn new(filename: String, source: &'a str) -> Self {
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

    /// Look up a binding across all scopes (innermost first)
    fn lookup(&self, name: &str) -> Option<&BindingKind> {
        for scope in self.scopes.iter().rev() {
            if let Some(kind) = scope.symbols.get(name) {
                return Some(kind);
            }
        }
        None
    }

    /// Check if a name refers to an unsafe queryset
    fn is_unsafe_queryset(&self, name: &str) -> bool {
        matches!(
            self.lookup(name),
            Some(BindingKind::QuerySet(QuerySetState::Unsafe))
        )
    }

    /// Classify an expression to determine if it produces a queryset and its safety
    fn classify_expr(&self, expr: &Expr) -> BindingKind {
        self.classify_expr_inner(expr, &mut Vec::new())
    }

    fn classify_expr_inner(&self, expr: &Expr, chain: &mut Vec<String>) -> BindingKind {
        match expr {
            Expr::Call(call) => {
                // Track method name if it's an attribute call
                if let Expr::Attribute(attr) = call.func.as_ref() {
                    chain.push(attr.attr.id.to_string());
                }
                self.classify_expr_inner(&call.func, chain)
            }
            Expr::Attribute(attr) => {
                if attr.attr.id.as_str() == "objects" {
                    // Found .objects - this is a queryset origin
                    // Check if any method in the chain makes it safe
                    let is_safe = self.is_safe_method(&chain);
                    let state = if is_safe {
                        QuerySetState::Safe
                    } else {
                        QuerySetState::Unsafe
                    };
                    BindingKind::QuerySet(state)
                } else {
                    self.classify_expr_inner(&attr.value, chain)
                }
            }
            Expr::Name(name) => {
                // Propagate binding kind if we're chaining off a known queryset
                if let Some(kind) = self.lookup(name.id.as_str()) {
                    // Check if the current chain of methods is safe
                    match kind {
                        BindingKind::QuerySet(query_set_state) if self.is_safe_method(&chain) => {
                            BindingKind::QuerySet(QuerySetState::Safe)
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

    fn is_safe_method(&self, chain: &[String]) -> bool {
        let safe_methods = [
            "select_related",
            "prefetch_related",
            "only",
            "defer",
            "values",
            "values_list",
            "iterator",
        ];
        chain.iter().any(|m| safe_methods.contains(&m.as_str()))
    }

    /// Record an assignment in the current scope
    fn record_assignment(&mut self, target: &Expr, value: &Expr) {
        if let Expr::Name(name) = target {
            let kind = self.classify_expr(value);
            self.current_scope_mut()
                .symbols
                .insert(name.id.to_string(), kind);
        }
    }

    /// Emit an N+1 diagnostic
    fn make_n1_diagnostic(&self, expr: &Expr, loop_var: &str, attr_name: &str) -> Diagnostic {
        let range = expr.range();
        let line = self.source[..range.start().to_usize()]
            .chars()
            .filter(|&c| c == '\n')
            .count()
            + 1;
        let col = range.start().to_usize()
            - self.source[..range.start().to_usize()]
                .rfind('\n')
                .map(|p| p + 1)
                .unwrap_or(0)
            + 1;

        Diagnostic {
            filename: self.filename.clone(),
            line,
            col,
            access: format!("{}.{}", loop_var, attr_name),
            message: format!(
                "Potential N+1 query: accessing `{}.{}` inside loop over unoptimized queryset",
                loop_var, attr_name
            ),
        }
    }
}

impl<'a> Visitor<'a> for Analyzer<'a> {
    fn visit_stmt(&mut self, stmt: &'a Stmt) {
        match stmt {
            // Track assignments
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

            // Handle for loops - push loop context before traversing body
            Stmt::For(for_stmt) => {
                let mut pushed_loop = false;

                // Check if iterating over an unsafe queryset
                if let Expr::Name(iter_name) = for_stmt.iter.as_ref()
                    && self.is_unsafe_queryset(iter_name.id.as_str())
                    // TODO: Handle tuples or another assignment method
                    && let Expr::Name(target) = for_stmt.target.as_ref()
                {
                    self.active_loops.push(LoopContext {
                        loop_var: target.id.to_string(),
                        unsafe_queryset: true,
                    });
                    pushed_loop = true;
                }

                ruff_python_ast::visitor::walk_stmt(self, stmt);

                if pushed_loop {
                    self.active_loops.pop();
                }
            }

            // Handle scope-creating statements
            Stmt::FunctionDef(_) | Stmt::ClassDef(_) => {
                self.push_scope();
                ruff_python_ast::visitor::walk_stmt(self, stmt);
                self.pop_scope();
            }

            // default: just walk
            _ => {
                ruff_python_ast::visitor::walk_stmt(self, stmt);
            }
        }
    }

    fn visit_expr(&mut self, expr: &'a Expr) {
        if let Expr::Attribute(attr) = expr {
            if let Expr::Name(name) = attr.value.as_ref() {
                let new_diags: Vec<_> = self
                    .active_loops
                    .iter()
                    .filter(|ctx| ctx.unsafe_queryset && name.id.as_str() == ctx.loop_var)
                    .map(|ctx| self.make_n1_diagnostic(expr, &ctx.loop_var, attr.attr.id.as_str()))
                    .collect();

                self.diagnostics.extend(new_diags);
            }
        }

        ruff_python_ast::visitor::walk_expr(self, expr);
    }
}

struct Parser;

impl Parser {
    fn new() -> Self {
        Self
    }

    pub fn parse_module(
        &self,
        file_content: &str,
    ) -> Result<Parsed<ModModule>, Box<dyn std::error::Error>> {
        Ok(parse_module(file_content)?)
    }

    pub fn analyze_directory(&self, dir: &Path) -> Result<(), Box<dyn std::error::Error>> {
        let mut total_diagnostics = 0;

        for entry in WalkDir::new(dir)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.file_name().to_string_lossy().ends_with(".py"))
        {
            let file_content = fs::read_to_string(entry.path())?;
            let parsed = self.parse_module(&file_content)?;
            let module = parsed.syntax();
            let filename = entry
                .path()
                .strip_prefix(dir)
                .unwrap_or(entry.path())
                .to_string_lossy()
                .to_string();

            let mut analyzer = Analyzer::new(filename, &file_content);

            for stmt in &module.body {
                analyzer.visit_stmt(stmt);
            }

            for diag in &analyzer.diagnostics {
                println!("{}\n", diag);
                total_diagnostics += 1;
            }
        }

        println!("\nTotal N+1 warnings: {}", total_diagnostics);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detect_positive() {
        let source = r#"
theo_analysis = TheoAnalysis.objects.filter(id=analysis_id, tier=Tiers.TIER1).first()
tier1 = theo_analysis.tier1s.filter(failed_reasons__isnull=True)

for t in tier1:
    for rp in t.relative_performances.all():
        pass
        "#;

        let parser = Parser::new();
        let parsed = parser.parse_module(&source).expect("should be parsed");
        let module = parsed.syntax();

        let mut analyzer = Analyzer::new("test".into(), &source);

        for stmt in &module.body {
            analyzer.visit_stmt(stmt);
        }

        assert_eq!(analyzer.diagnostics.len(), 1);
    }

    #[test]
    fn avoid_false_positive() {
        let source = r#"
theo_analysis = TheoAnalysis.objects.filter(id=analysis_id, tier=Tiers.TIER1).first()
tier1 = theo_analysis.tier1s.filter(
    failed_reasons__isnull=True).select_related('ticker').prefetch_related('relative_performances')

for t in tier1:
    for rp in t.relative_performances.all():
        pass
        "#;

        let parser = Parser::new();
        let parsed = parser.parse_module(&source).expect("should be parsed");
        let module = parsed.syntax();

        let mut analyzer = Analyzer::new("test".into(), &source);

        for stmt in &module.body {
            analyzer.visit_stmt(stmt);
        }

        assert!(analyzer.diagnostics.is_empty());
    }
}
