#![feature(if_let_guard)]
#![feature(slice_pattern)]
mod diagnostic;
mod error;
mod ir;
mod parser;
mod passes;

pub use diagnostic::NPlusOneDiagnostic;
pub use ir::model::ModelGraph;
pub use parser::Parser;
pub use passes::functions::QueryFunction;
