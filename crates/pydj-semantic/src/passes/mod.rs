//! Analysis passes over Python AST.

pub mod functions;
pub mod model_graph;
pub mod n_plus_one;

use ruff_python_ast::ModModule;

/// A single analysis pass over a Python module.
pub trait Pass<'a> {
    type Output;

    /// Run the pass on a parsed module.
    fn run(&mut self, module: &'a ModModule) -> Self::Output;
}
