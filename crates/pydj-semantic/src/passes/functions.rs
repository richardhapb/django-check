use crate::passes::Pass;
use ruff_python_ast::{Expr, ModModule, Stmt, visitor::Visitor};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct QueryFunction {
    name: String,
    args: Vec<QueryArg>,
}

#[derive(Debug, Clone)]
pub struct QueryArg {
    var_name: String,
    model_name: String,
    attr_accesses: HashSet<String>,
}

#[derive(Debug, Clone)]
struct FunctionScope {
    function_vars: Vec<String>,
}

#[derive(Debug, Clone)]
struct LoopContext {
    func_name: String,
    loop_var: String,
    orig_var: String,
}

#[derive(Debug, Clone)]
pub struct QueryFunctionPass {
    functions: Vec<QueryFunction>,
    model_aliases: HashMap<String, String>,
    function_scopes: Vec<FunctionScope>,
    active_loops: Vec<LoopContext>,
}

impl QueryFunctionPass {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            model_aliases: HashMap::new(),
            function_scopes: Vec::new(),
            active_loops: Vec::new(),
        }
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
                        self.model_aliases
                            .insert(alias.id.to_string(), name.name.id.to_string());
                    }
                }
                ruff_python_ast::visitor::walk_stmt(self, stmt);
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
                            model_name: self
                                .model_aliases
                                .get(model_name)
                                .map_or(model_name, |v| v)
                                .to_string(),
                            attr_accesses: HashSet::new(),
                        });
                    }
                }

                if !query_args.is_empty() {
                    self.functions.push(QueryFunction {
                        name: fd.name.id.to_string(),
                        args: query_args.clone(),
                    });

                    self.function_scopes.push(FunctionScope {
                        function_vars: query_args.iter().map(|a| a.var_name.to_string()).collect(),
                    });
                    ruff_python_ast::visitor::walk_stmt(self, stmt);
                    self.function_scopes.pop();
                }
                ruff_python_ast::visitor::walk_stmt(self, stmt);
            }
            Stmt::For(for_stmt) => {
                let mut loop_inserted = false;
                if !self.function_scopes.is_empty()
                    && let Expr::Name(iter_name) = for_stmt.iter.as_ref()
                    && let Expr::Name(target) = for_stmt.target.as_ref()
                {
                    for f in self.functions.iter() {
                        if let Some(var) = f.args.iter().find(|a| a.var_name == iter_name.id) {
                            self.active_loops.push(LoopContext {
                                func_name: f.name.clone(),
                                loop_var: target.id.to_string(),
                                orig_var: var.var_name.clone(),
                            });

                            loop_inserted = true;

                            break;
                        }
                    }

                    ruff_python_ast::visitor::walk_stmt(self, stmt);

                    if loop_inserted {
                        self.active_loops.pop();
                    }
                }
            }
            _ => ruff_python_ast::visitor::walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'a Expr) {
        if let Expr::Attribute(attr) = expr {
            let (base_name, attr_chain) = self.extract_attribute_chain(attr);

            for ctx in &self.active_loops {
                if base_name != ctx.loop_var {
                    continue;
                }

                for el in attr_chain.iter() {
                    if let Some(f) = self.functions.iter_mut().find(|f| f.name == ctx.func_name)
                        && let Some(arg) = f.args.iter_mut().find(|a| a.var_name == ctx.orig_var)
                    {
                        arg.attr_accesses.insert(el.to_string());
                    }
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

    #[test]
    fn attr_access_detected() {
        let source = r#"
def foo(qs: QuerySet[Bar]) -> None:
    for q in qs:
       print(q.user)
        "#;
        let functions = run_pass(&source);

        assert!(!functions.is_empty());
        let function = functions.iter().next().unwrap();
        assert_eq!(function.name, "foo");
        assert!(!function.args.is_empty());

        let arg = function.args.iter().next().unwrap();
        assert_eq!(arg.var_name, "qs");
        assert_eq!(arg.model_name, "Bar");
        assert!(!arg.attr_accesses.is_empty());
        let attr = arg.attr_accesses.iter().next().unwrap();
        assert_eq!(attr, "user");
    }
}
