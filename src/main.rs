use ruff_python_ast::{Expr, ExprName, Stmt, StmtFor};
use ruff_python_parser::parse_module;
use std::collections::HashMap;
use std::env::home_dir;
// use std::env::current_dir;
use std::fs;
use std::path::Path;
use walkdir::WalkDir;

fn main() {
    // let cwd = current_dir().unwrap();
    let home = home_dir().unwrap();
    let path = home.join("dev/agora_hedge/main/app");
    get_for_statements(&path).unwrap();
}

#[derive(Debug)]
enum QuerySetState {
    Safe,
    Unsafe,
}

#[derive(Debug)]
enum BindingKind {
    QuerySetCandidate(QuerySetState),
    NoQuerySet,
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

#[derive(Debug)]
struct Context {
    filename: String,
    stack: Vec<ScopeState>,
}

impl Context {
    fn new(filename: String) -> Self {
        let mut stack = Vec::new();
        // Global state
        stack.push(ScopeState::new());
        Self { filename, stack }
    }
    fn current_scope(&self) -> Option<&ScopeState> {
        self.stack.iter().last()
    }

    fn current_scope_mut(&mut self) -> Option<&mut ScopeState> {
        self.stack.iter_mut().last()
    }
}

fn is_candidate(name: &str, ctx: &Context) -> bool {
    if let Some(scope) = ctx.current_scope() {
        scope
            .symbols
            .iter()
            .filter_map(|(n, k)| {
                match k {
                    BindingKind::QuerySetCandidate(state)
                        if matches!(state, QuerySetState::Unsafe) =>
                    {
                        true
                    }
                    _ => false,
                }
                .then_some(n.as_str())
            })
            .collect::<Vec<&str>>()
            .contains(&name)
    } else {
        false
    }
}

fn evaluate_for(for_stmt: &StmtFor, ctx: &mut Context) {
    if let Expr::Name(name) = for_stmt.iter.as_ref()
        && is_candidate(&name.id.as_str(), ctx)
    {
        for expr in for_stmt.body.iter() {
            if find_n1(&expr, &for_stmt.target, ctx)
                && let Expr::Name(name) = for_stmt.target.as_ref()
            {
                println!(
                    "[{}] {} is a N + 1 candidate!",
                    ctx.filename,
                    name.id.as_str()
                );
            }
        }
    }
}

fn walk_expression<'a>(
    expr: &'a Expr,
    loop_name: &ExprName,
    traceback: &mut Vec<&'a Expr>,
) -> bool {
    traceback.push(expr);
    match expr {
        Expr::Lambda(expr_lambda) => walk_expression(&expr_lambda.body, loop_name, traceback),
        Expr::If(expr_if) => {
            walk_expression(&expr_if.body, loop_name, traceback)
                || walk_expression(&expr_if.orelse, loop_name, traceback)
                || walk_expression(&expr_if.test, loop_name, traceback)
        }
        Expr::Dict(expr_dict) => expr_dict
            .iter_values()
            .any(|v| walk_expression(v, loop_name, traceback)),
        Expr::ListComp(expr_list_comp) => expr_list_comp
            .generators
            .iter()
            .any(|g| walk_expression(&g.target, loop_name, traceback)),
        Expr::SetComp(expr_set_comp) => expr_set_comp
            .generators
            .iter()
            .any(|g| walk_expression(&g.target, loop_name, traceback)),
        Expr::DictComp(expr_dict_comp) => expr_dict_comp
            .generators
            .iter()
            .any(|g| walk_expression(&g.target, loop_name, traceback)),
        Expr::Generator(expr_generator) => expr_generator
            .generators
            .iter()
            .any(|g| walk_expression(&g.target, loop_name, traceback)),
        Expr::Call(expr_call) => expr_call
            .arguments
            .args
            .iter()
            .any(|a| walk_expression(a, loop_name, traceback)),
        Expr::Attribute(attr) => walk_expression(&attr.value, loop_name, traceback),
        Expr::Name(expr_name) => {
            expr_name.id.as_str() == loop_name.id.as_str()
                && traceback.iter().any(|t| matches!(t, Expr::Attribute(_)))
        }
        Expr::List(expr_list) => expr_list
            .elts
            .iter()
            .any(|e| walk_expression(e, loop_name, traceback)),
        Expr::Tuple(expr_tuple) => expr_tuple
            .elts
            .iter()
            .any(|e| walk_expression(e, loop_name, traceback)),
        _ => false,
    }
}

fn walk_expressions(stmt: &Stmt, loop_name: &ExprName) -> bool {
    match stmt {
        Stmt::Return(stmt_return) => {
            let Some(ref expr) = stmt_return.value else {
                return false;
            };
            walk_expression(&expr, loop_name, &mut Vec::new())
        }
        Stmt::Assign(stmt_assign) => {
            walk_expression(&stmt_assign.value, loop_name, &mut Vec::new())
        }
        Stmt::AnnAssign(stmt_ann_assign) => stmt_ann_assign
            .value
            .as_ref()
            .and_then(|value| Some(walk_expression(&value, loop_name, &mut Vec::new())))
            .unwrap_or(false),
        Stmt::For(stmt_for) => walk_expression(&stmt_for.iter, loop_name, &mut Vec::new()),
        Stmt::While(stmt_while) => stmt_while
            .body
            .iter()
            .any(|v| walk_expressions(v, loop_name)),
        Stmt::If(stmt_if) => {
            walk_expression(&stmt_if.test, loop_name, &mut Vec::new())
                || stmt_if.body.iter().any(|e| walk_expressions(e, loop_name))
                || stmt_if.elif_else_clauses.iter().any(|e| {
                    e.test
                        .as_ref()
                        .and_then(|t| Some(walk_expression(&t, loop_name, &mut Vec::new())))
                        .unwrap_or(false)
                        || e.body.iter().any(|e| walk_expressions(e, loop_name))
                })
        }
        Stmt::Match(stmt_match) => {
            walk_expression(&stmt_match.subject, loop_name, &mut Vec::new())
                || stmt_match
                    .cases
                    .iter()
                    .any(|c| c.body.iter().any(|b| walk_expressions(b, loop_name)))
        }
        Stmt::Try(stmt_try) => {
            stmt_try.handlers.iter().any(|h| {
                h.as_except_handler()
                    .and_then(|h| Some(h.body.iter().any(|e| walk_expressions(e, loop_name))))
                    .unwrap_or(false)
            }) || stmt_try
                .orelse
                .iter()
                .any(|h| walk_expressions(h, loop_name))
                || stmt_try.body.iter().any(|e| walk_expressions(e, loop_name))
                || stmt_try
                    .finalbody
                    .iter()
                    .any(|e| walk_expressions(e, loop_name))
        }
        Stmt::Assert(stmt_assert) => walk_expression(&stmt_assert.test, loop_name, &mut Vec::new()),
        Stmt::Expr(stmt_expr) => walk_expression(&stmt_expr.value, loop_name, &mut Vec::new()),
        _ => false,
    }
}

fn find_n1(stmt: &Stmt, loop_var: &Expr, ctx: &Context) -> bool {
    let Expr::Name(loop_name) = loop_var else {
        return false;
    };

    for child in children(stmt) {
        for stmt in child {
            if find_n1(stmt, loop_var, ctx) {
                return true;
            }
        }
    }

    walk_expressions(stmt, loop_name)
}

fn evaluate_expr<'a>(expr: &'a Expr, traceback: &mut Vec<&'a Expr>) -> BindingKind {
    traceback.push(expr);

    match expr {
        Expr::Call(call) => evaluate_expr(&call.func, traceback),
        Expr::Attribute(attr) => {
            if attr.attr.id.as_str() == "objects" {
                let mut state = QuerySetState::Unsafe;
                for tr in traceback {
                    if let Expr::Call(call) = tr
                        && let Expr::Attribute(attr) = call.func.as_ref()
                        && ["select_related", "prefetch_related"].contains(&attr.attr.id.as_str())
                    {
                        state = QuerySetState::Safe;
                        break;
                    }
                }
                BindingKind::QuerySetCandidate(state)
            } else {
                evaluate_expr(&attr.value, traceback)
            }
        }
        _ => BindingKind::Unknown,
    }
}

fn evaluate_assignment(assignment: &Stmt, ctx: &mut Context) {
    match assignment {
        Stmt::Assign(assign) => {
            for target in assign.targets.iter() {
                if let Some(name) = target.as_name_expr() {
                    let kind = evaluate_expr(&assign.value, &mut Vec::new());
                    ctx.current_scope_mut()
                        .expect("should exist an scope")
                        .symbols
                        .insert(name.id.to_string(), kind);
                }
            }
        }
        Stmt::AnnAssign(assign) => {
            if let Some(name) = assign.target.as_name_expr()
                && let Some(ref value) = assign.value
            {
                let kind = evaluate_expr(value, &mut Vec::new());
                ctx.current_scope_mut()
                    .expect("should exist an scope")
                    .symbols
                    .insert(name.id.to_string(), kind);
            }
        }
        _ => {}
    }
}

fn maybe_analyze(stmt: &Stmt, ctx: &mut Context) {
    match stmt {
        Stmt::For(for_stmt) => {
            evaluate_for(for_stmt, ctx);
        }
        Stmt::Assign(_) | Stmt::AnnAssign(_) => evaluate_assignment(stmt, ctx),
        _ => {}
    }
}

fn children(stmt: &Stmt) -> Vec<&[Stmt]> {
    match stmt {
        Stmt::For(s) => vec![&s.body, &s.orelse],
        Stmt::FunctionDef(s) => {
            vec![&s.body]
        }
        Stmt::While(s) => vec![&s.body, &s.orelse],
        Stmt::With(s) => vec![&s.body],
        Stmt::ClassDef(s) => {
            vec![&s.body]
        }
        Stmt::If(s) => {
            let mut bodies: Vec<&[Stmt]> = vec![&s.body];
            for clause in &s.elif_else_clauses {
                bodies.push(&clause.body);
            }
            bodies
        }
        Stmt::Match(s) => s.cases.iter().map(|c| c.body.as_slice()).collect(),
        Stmt::Try(s) => {
            let mut bodies: Vec<&[Stmt]> = vec![&s.body, &s.orelse, &s.finalbody];
            for handler in &s.handlers {
                match handler {
                    ruff_python_ast::ExceptHandler::ExceptHandler(h) => {
                        bodies.push(&h.body);
                    }
                }
            }
            bodies
        }
        _ => vec![],
    }
}

fn visit_stmt(stmt: &Stmt, ctx: &mut Context) {
    match stmt {
        Stmt::FunctionDef(_) | Stmt::ClassDef(_) => {
            ctx.stack.push(ScopeState::new());
        }
        _ => {}
    }

    maybe_analyze(stmt, ctx);

    for body in children(stmt) {
        for child in body {
            visit_stmt(child, ctx);
        }
    }

    match stmt {
        Stmt::FunctionDef(_) | Stmt::ClassDef(_) => {
            ctx.stack.pop();
        }
        _ => {}
    }
}

pub fn get_for_statements(dir: &Path) -> Result<(), Box<dyn std::error::Error>> {
    WalkDir::new(dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|entry| entry.file_name().to_string_lossy().ends_with(".py"))
        .try_for_each(|entry| {
            let file_content = fs::read_to_string(entry.path())?;
            let parsed = parse_module(&file_content)?;
            let module = parsed.syntax();

            let filename: String = entry
                .path()
                .strip_prefix(dir)
                .unwrap()
                .to_string_lossy()
                .replace("/", ".")
                .into();

            let mut context = Context::new(filename);

            for stmt in &module.body {
                visit_stmt(stmt, &mut context);
            }

            Ok(())
        })
}
