use crate::passes::Pass;
use ruff_python_ast::{Expr, ModModule, Stmt, visitor::Visitor};
use std::{
    collections::{HashMap, HashSet},
    marker::PhantomData,
};

#[derive(Debug, Clone)]
pub struct QueryFunction {
    pub name: String,
    pub args: Vec<QueryArg>,
}

#[derive(Debug, Clone)]
pub struct QueryArg {
    pub idx: usize,
    pub var_name: String,
    pub model_name: String,
    pub attr_accesses: HashSet<String>,
}

#[derive(Debug, Clone, Copy)]
struct RawIdx(usize);

#[derive(Debug)]
struct Idx<T> {
    raw: RawIdx,
    _ty: PhantomData<fn() -> T>,
}

impl<T> Clone for Idx<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Idx<T> {}

impl<T> From<usize> for Idx<T> {
    fn from(value: usize) -> Self {
        Self {
            raw: RawIdx(value),
            _ty: PhantomData,
        }
    }
}

impl<T> Idx<T> {
    fn idx(&self) -> usize {
        self.raw.0
    }
}

#[derive(Debug, Clone, Copy)]
struct QueryArgId {
    func_idx: Idx<QueryFunction>,
    arg_idx: Idx<QueryArg>,
}

#[derive(Debug, Clone)]
struct LoopContext<'a> {
    arg: QueryArgId,
    loop_var: &'a str,
}

#[derive(Debug, Clone)]
pub struct QueryFunctionPass<'a> {
    functions: Vec<QueryFunction>,
    model_aliases: HashMap<&'a str, &'a str>,
    active_loops: Vec<LoopContext<'a>>,
}

impl<'a> QueryFunctionPass<'a> {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            model_aliases: HashMap::new(),
            active_loops: Vec::new(),
        }
    }
}

impl<'a> Pass<'a> for QueryFunctionPass<'a> {
    type Output = Vec<QueryFunction>;

    fn run(&mut self, module: &'a ModModule) -> Self::Output {
        for stmt in &module.body {
            self.visit_stmt(stmt);
        }

        std::mem::take(&mut self.functions)
    }
}

impl<'a> Visitor<'a> for QueryFunctionPass<'a> {
    fn visit_stmt(&mut self, stmt: &'a Stmt) {
        match stmt {
            Stmt::ImportFrom(import) => {
                for name in &import.names {
                    if let Some(alias) = &name.asname {
                        self.model_aliases
                            .insert(alias.id.as_str(), name.name.id.as_str());
                    }
                }
                ruff_python_ast::visitor::walk_stmt(self, stmt);
            }

            Stmt::FunctionDef(fd) => {
                let mut query_args = Vec::new();

                for (arg_idx, arg) in fd.parameters.args.iter().enumerate() {
                    if let Some(anot) = arg.parameter.annotation()
                        && let Some(subscript) = anot.as_subscript_expr()
                        && let Expr::Name(name) = subscript.value.as_ref()
                        && name.id.contains("QuerySet")
                        && let Expr::Name(sl) = subscript.slice.as_ref()
                    {
                        let model_name = sl.id.as_str();
                        query_args.push(QueryArg {
                            idx: arg_idx,
                            var_name: arg.name().to_string(),
                            model_name: self
                                .model_aliases
                                .get(model_name)
                                .copied()
                                .unwrap_or(model_name)
                                .to_string(),
                            attr_accesses: HashSet::new(),
                        });
                    }
                }

                if !query_args.is_empty() {
                    self.functions.push(QueryFunction {
                        name: fd.name.id.to_string(),
                        args: query_args,
                    });

                    ruff_python_ast::visitor::walk_stmt(self, stmt);
                }
            }

            Stmt::For(for_stmt) => {
                let mut loop_inserted = false;
                if let Expr::Name(iter_name) = for_stmt.iter.as_ref()
                    && let Expr::Name(target) = for_stmt.target.as_ref()
                {
                    // Find the function and argument that matches this loop's iterator
                    if let Some((f_idx, f)) = self.functions.iter().enumerate().next_back()
                        && let Some((a_idx, _)) = f
                            .args
                            .iter()
                            .enumerate()
                            .find(|(_, a)| a.var_name == iter_name.id.as_str())
                    {
                        self.active_loops.push(LoopContext {
                            arg: QueryArgId {
                                func_idx: Idx::from(f_idx),
                                arg_idx: Idx::from(a_idx),
                            },
                            loop_var: target.id.as_str(),
                        });
                        loop_inserted = true;
                    }
                }

                ruff_python_ast::visitor::walk_stmt(self, stmt);

                if loop_inserted {
                    self.active_loops.pop();
                }
            }
            _ => ruff_python_ast::visitor::walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'a Expr) {
        if let Expr::Attribute(attr) = expr {
            let (base_name, attr_chain) = self.extract_attribute_chain(attr);

            for ctx in self.active_loops.iter() {
                if ctx.loop_var != base_name {
                    continue;
                }

                let arg = &mut self.functions[ctx.arg.func_idx.idx()].args[ctx.arg.arg_idx.idx()];
                for el in attr_chain.iter() {
                    arg.attr_accesses.insert(el.to_string());
                }
            }
        }

        ruff_python_ast::visitor::walk_expr(self, expr);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn run_pass(source: &str) -> Vec<QueryFunction> {
        let parser = Parser::new();
        let parsed = parser.parse_module(source).expect("should parse");
        let mut function_pass = QueryFunctionPass::new();
        function_pass.run(parsed.syntax())
    }

    #[test]
    fn detect_qs_arg() {
        let source = r#"
def foo(qs: QuerySet[Bar]) -> None:
    pass
        "#;
        let functions = run_pass(source);

        assert!(!functions.is_empty());
        let function = functions.first().unwrap();
        assert_eq!(function.name, "foo");
        assert!(!function.args.is_empty());

        let arg = function.args.first().unwrap();
        assert_eq!(arg.var_name, "qs");
        assert_eq!(arg.model_name, "Bar");
    }

    #[test]
    fn detect_multiple_qs_arg() {
        let source = r#"
def foo(qs: QuerySet[Bar], n: int, qs2: QuerySet[User]) -> None:
    pass
        "#;
        let functions = run_pass(source);

        assert!(!functions.is_empty());
        let function = functions.first().unwrap();
        assert_eq!(function.name, "foo");
        assert!(!function.args.is_empty());

        let arg1 = function.args.first().unwrap();
        let arg2 = function.args.iter().last().unwrap();

        assert_eq!(arg1.var_name, "qs");
        assert_eq!(arg1.model_name, "Bar");
        assert_eq!(arg1.idx, 0);
        assert_eq!(arg2.var_name, "qs2");
        assert_eq!(arg2.model_name, "User");
        assert_eq!(arg2.idx, 2);
    }

    #[test]
    fn no_capture_no_qs_arg() {
        let source = r#"
def foo(n: int, s: str) -> None:
    pass
        "#;
        let functions = run_pass(source);

        assert!(functions.is_empty());
    }

    #[test]
    fn resolve_alias() {
        let source = r#"
from bar.models import Bar as BarModel

def foo(qs: QuerySet[BarModel]) -> None:
    pass
        "#;
        let functions = run_pass(source);

        assert!(!functions.is_empty());
        let function = functions.first().unwrap();
        assert_eq!(function.name, "foo");
        assert!(!function.args.is_empty());

        let arg = function.args.first().unwrap();
        assert_eq!(arg.var_name, "qs");
        assert_eq!(arg.model_name, "Bar");
    }

    #[test]
    fn attr_access_detected() {
        let source = r#"
def foo(qs: QuerySet[Bar]) -> None:
    for q in qs:
       print(q.user)
        "#;
        let functions = run_pass(source);

        assert!(!functions.is_empty());
        let function = functions.first().unwrap();
        assert_eq!(function.name, "foo");
        assert!(!function.args.is_empty());

        let arg = function.args.first().unwrap();
        assert_eq!(arg.var_name, "qs");
        assert_eq!(arg.model_name, "Bar");
        assert_eq!(arg.idx, 0);

        assert!(!arg.attr_accesses.is_empty());
        let attr = arg.attr_accesses.iter().next().unwrap();
        assert_eq!(*attr, "user");
    }

    #[test]
    fn attr_nested_access_detected() {
        let source = r#"
def foo(qs: QuerySet[Bar], qs2: QuerySet[Bar2]) -> None:
    for q in qs:
        for q2 in qs2:
            print(q2.user)
        "#;
        let functions = run_pass(source);

        assert!(!functions.is_empty());
        let function = functions.first().unwrap();
        assert_eq!(function.name, "foo");
        assert!(!function.args.is_empty());

        let arg = function.args.iter().last().unwrap();
        assert_eq!(arg.var_name, "qs2");
        assert_eq!(arg.model_name, "Bar2");

        assert!(!arg.attr_accesses.is_empty());
        let attr = arg.attr_accesses.iter().next().unwrap();
        assert_eq!(*attr, "user");
    }
}
