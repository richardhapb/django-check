#![feature(if_let_guard)]
#![allow(dead_code)] // A lot of code unused now
mod diagnostic;
mod ir;
mod parser;
mod passes;

use std::env::home_dir;

use crate::parser::Parser;

fn main() {
    let home = home_dir().unwrap();
    let parser = Parser::new();

    if let Err(e) = parser.analyze_directory(&home.join("dev/ddirt/development/app")) {
        eprintln!("Error: {}", e);
    }
}
