#![feature(if_let_guard)]
#![allow(dead_code)] // A lot of code unused now
mod diagnostic;
mod ir;
mod parser;
mod passes;

pub use diagnostic::NPlusOneDiagnostic;
pub use ir::model::ModelGraph;
pub use parser::Parser;
pub use passes::functions::QueryFunction;
