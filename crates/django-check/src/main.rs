mod cli;
use std::path::Path;

use django_check_semantic::Parser;
use django_check_server::serve;
use std::env::current_dir;

use clap::Parser as ClapParser;
use cli::{Cli, Cmd};
use tracing_subscriber::{EnvFilter, filter::LevelFilter};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cwd = current_dir().expect("should get the cwd");
    let parser = Parser::new();

    let cli = Cli::parse();

    match cli.cmd {
        Cmd::Check => {
            initialize_logger(false);
            let model_graph = parser.extract_model_graph(&cwd)?;
            let functions = parser.extract_functions(&cwd)?;
            if let Err(e) = parser.analyze_directory(&cwd, &model_graph, &functions) {
                eprintln!("Error: {}", e);
            }
        }
        Cmd::Server => {
            initialize_logger(true);
            serve(&cwd).await;
        }
    }

    Ok(())
}

fn initialize_logger(with_file: bool) {
    let env_filter = EnvFilter::from_default_env();

    if with_file {
        let log_path = Path::new("/tmp/django-check.log");
        let file = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(log_path)
            .expect("open log file");
        tracing_subscriber::fmt()
            .with_max_level(LevelFilter::INFO)
            .with_writer(file)
            .with_ansi(false)
            .with_env_filter(env_filter)
            .init();
    } else {
        tracing_subscriber::fmt()
            .with_max_level(LevelFilter::ERROR)
            .with_env_filter(env_filter)
            .without_time()
            .init();
    }
}
