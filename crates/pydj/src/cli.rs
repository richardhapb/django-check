use clap::{Parser, Subcommand};

#[derive(Debug, Parser, Clone)]
pub struct Cli {
    #[command(subcommand)]
    pub cmd: Cmd,
}

#[derive(Debug, Subcommand, PartialEq, Clone)]
pub enum Cmd {
    Server,
    Check,
}
