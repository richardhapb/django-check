mod cli;

use pydj_semantic::Parser;
use pydj_server::serve;
use std::env::current_dir;

use clap::Parser as ClapParser;
use cli::{Cli, Cmd};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cwd = current_dir().expect("should get the cwd");
    let parser = Parser::new();

    let cli = Cli::parse();
    let model_graph = parser.extract_model_graph(&cwd)?;

    match cli.cmd {
        Cmd::Check => {
            if let Err(e) = parser.analyze_directory(&cwd, &model_graph) {
                eprintln!("Error: {}", e);
            }
        }
        Cmd::Server => {
            serve(&cwd).await;
        }
    }

    Ok(())
}
