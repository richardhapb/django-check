use crate::passes::Pass;
use ruff_python_ast::{Expr, ModModule, Stmt, visitor::Visitor};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct QueryFunction {
    name: String,
    args: Vec<QueryArg>,
}

#[derive(Debug, Clone)]
pub struct QueryArg {
    var_name: String,
    model_name: String,
}

#[derive(Debug, Clone)]
pub struct QueryFunctionPass {
    functions: Vec<QueryFunction>,
    model_aliases: HashMap<String, String>,
}

impl QueryFunctionPass {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            model_aliases: HashMap::new(),
        }
    }
}

impl<'a> Pass<'a> for QueryFunctionPass {
    type Output = Vec<QueryFunction>;

    fn run(&mut self, module: &'a ModModule) -> Self::Output {
        for stmt in &module.body {
            self.visit_stmt(stmt);
        }

        std::mem::take(&mut self.functions)
    }
}

impl<'a> Visitor<'a> for QueryFunctionPass {
    fn visit_stmt(&mut self, stmt: &'a Stmt) {
        match stmt {
            Stmt::ImportFrom(import) => {
                for name in import.names.iter() {
                    if let Some(alias) = name.asname.as_ref() {
                        self.model_aliases.insert(alias.id.to_string(), name.name.id.to_string());
                    }
                }
            }
            Stmt::FunctionDef(fd) => {
                let mut query_args = Vec::new();

                for arg in fd.parameters.args.iter() {
                    if let Some(anot) = arg.parameter.annotation()
                        && let Some(subscript) = anot.as_subscript_expr()
                        && let Expr::Name(name) = subscript.value.as_ref()
                        && name.id.contains("QuerySet")
                        && let Expr::Name(sl) = subscript.slice.as_ref()
                    {
                        let model_name = sl.id.as_str();

                        query_args.push(QueryArg {
                            var_name: arg.name().to_string(),
                            model_name: self.model_aliases.get(model_name).map_or(model_name, |v| v).to_string(),
                        });
                    }
                }
                if !query_args.is_empty() {
                    self.functions.push(QueryFunction {
                        name: fd.name.id.to_string(),
                        args: query_args,
                    });
                }
            }
            _ => {}
        }
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
        let functions = run_pass(&source);

        assert!(!functions.is_empty());
        let function = functions.iter().next().unwrap();
        assert_eq!(function.name, "foo");
        assert!(!function.args.is_empty());

        let arg = function.args.iter().next().unwrap();
        assert_eq!(arg.var_name, "qs");
        assert_eq!(arg.model_name, "Bar");
    }

    #[test]
    fn detect_multiple_qs_arg() {
        let source = r#"
def foo(qs: QuerySet[Bar], n: int, qs2: QuerySet[User]) -> None:
    pass
        "#;
        let functions = run_pass(&source);

        assert!(!functions.is_empty());
        let function = functions.iter().next().unwrap();
        assert_eq!(function.name, "foo");
        assert!(!function.args.is_empty());

        let arg1 = function.args.iter().next().unwrap();
        let arg2 = function.args.iter().last().unwrap();

        assert_eq!(arg1.var_name, "qs");
        assert_eq!(arg1.model_name, "Bar");
        assert_eq!(arg2.var_name, "qs2");
        assert_eq!(arg2.model_name, "User");
    }

    #[test]
    fn no_capture_no_qs_arg() {
        let source = r#"
def foo(n: int, s: str) -> None:
    pass
        "#;
        let functions = run_pass(&source);

        assert!(functions.is_empty());
    }

    #[test]
    fn resolve_alias() {
        let source = r#"
from bar.models import Bar as BarModel

def foo(qs: QuerySet[BarModel]) -> None:
    pass
        "#;
        let functions = run_pass(&source);

        assert!(!functions.is_empty());
        let function = functions.iter().next().unwrap();
        assert_eq!(function.name, "foo");
        assert!(!function.args.is_empty());

        let arg = function.args.iter().next().unwrap();
        assert_eq!(arg.var_name, "qs");
        assert_eq!(arg.model_name, "Bar");
    }
}
