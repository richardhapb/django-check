//! Analysis passes over Python AST.

pub mod functions;
pub mod model_graph;
pub mod n_plus_one;

use ruff_python_ast::{Expr, ModModule};

/// A single analysis pass over a Python module.
pub trait Pass<'a> {
    type Output;

    /// Run the pass on a parsed module.
    fn run(&mut self, module: &'a ModModule) -> Self::Output;
}

/// Extract the attribute chain from a complete sentence
///
/// Args
///     attr: The attribute where begin to backward for capturing the chain
///
/// Returns
///     tuple with the base element of the sentence and all the posterior expressions
fn extract_attribute_chain(attr: &ruff_python_ast::ExprAttribute) -> (&str, Vec<&str>) {
    let mut chain = vec![attr.attr.id.as_str()];
    let mut current = attr.value.as_ref();

    loop {
        match current {
            Expr::Attribute(attr) => {
                chain.push(attr.attr.id.as_str());
                current = attr.value.as_ref();
            }
            Expr::Call(call) => {
                current = call.func.as_ref();
            }
            Expr::Name(name) => {
                chain.reverse();
                return (name.id.as_str(), chain);
            }
            _ => return ("", Vec::new()),
        }
    }
}
