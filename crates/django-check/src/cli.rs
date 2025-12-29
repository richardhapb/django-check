use clap::{Parser, Subcommand};

#[derive(Debug, Parser, Clone)]
#[command(
    name = "django-check",
    version,
    about,
    long_about = "Static N+1 query detection for Django",
    author = "richardhapb"
)]
pub struct Cli {
    #[command(subcommand)]
    pub cmd: Cmd,
}

#[derive(Debug, Subcommand, PartialEq, Clone)]
pub enum Cmd {
    /// Start as a Language Server (normally handled by the IDE)
    Server,
    /// Analyze the current directory tree for N+1 queries
    Check,
}
