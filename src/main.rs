use ruff_python_ast::{Expr, Stmt, StmtFor};
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
enum BindingKind {
    QuerySetCandidate,
    NoQuerySet,
    Unknown,
}

#[derive(Debug)]
struct Context {
    filename: String,
    symbols: HashMap<String, BindingKind>,
}

impl Context {
    fn new(filename: String) -> Self {
        Self {
            filename,
            symbols: HashMap::new(),
        }
    }
}

fn evaluate_for(for_stmt: &StmtFor, ctx: &Context) {
    if let Expr::Name(name) = for_stmt.iter.as_ref() {
        println!("{}", name.id.as_str());
    }
}

fn evaluate_expr(expr: &Expr) -> BindingKind {
    match expr {
        Expr::Call(call) => evaluate_expr(&call.func),
        Expr::Attribute(attr) => {
            if attr.attr.id.as_str() == "objects" {
                BindingKind::QuerySetCandidate
            } else {
                evaluate_expr(&attr.value)
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
                    let kind = evaluate_expr(&assign.value);
                    ctx.symbols
                        .entry(name.id.to_string())
                        .or_insert_with(|| kind);
                }
            }
        }
        Stmt::AnnAssign(assign) => {
            if let Some(name) = assign.target.as_name_expr() {
                if let Some(ref value) = assign.value {
                    let kind = evaluate_expr(value);
                    ctx.symbols
                        .entry(name.id.to_string())
                        .or_insert_with(|| kind);
                }
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
        Stmt::FunctionDef(s) => vec![&s.body],
        Stmt::While(s) => vec![&s.body, &s.orelse],
        Stmt::With(s) => vec![&s.body],
        Stmt::ClassDef(s) => vec![&s.body],
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
    maybe_analyze(stmt, ctx);
    for body in children(stmt) {
        for child in body {
            visit_stmt(child, ctx);
        }
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

            dbg!(context);

            Ok(())
        })
}
