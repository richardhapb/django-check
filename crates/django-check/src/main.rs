mod cli;
use std::path::{Path, PathBuf};

use django_check_semantic::Parser;
use django_check_server::serve;
use std::env::current_dir;

use clap::Parser as ClapParser;
use cli::{Cli, Cmd};
use tracing_subscriber::{EnvFilter, filter::LevelFilter};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cwd = current_dir().expect("should get the cwd");
    let root = resolve_project_root(&cwd);
    let parser = Parser::new();

    let model_graph = parser.extract_model_graph(&root)?;
    let functions = parser.extract_functions(&root)?;

    let cli = Cli::parse();

    match cli.cmd {
        Cmd::Check => {
            initialize_logger(false);
            if let Err(e) = parser.analyze_directory(&root, &model_graph, &functions) {
                eprintln!("Error: {}", e);
            }
        }
        Cmd::Server => {
            initialize_logger(true);
            serve(&root, model_graph, functions).await;
        }
    }

    Ok(())
}

fn resolve_project_root(cwd: &Path) -> PathBuf {
    // Prefer the current directory when it already looks like a Django project.
    if cwd.join("manage.py").is_file() {
        return cwd.to_path_buf();
    }

    // If we're one level above the app folder, pick that folder automatically.
    let nested_candidates = std::fs::read_dir(cwd)
        .ok()
        .into_iter()
        .flat_map(|entries| entries.filter_map(Result::ok))
        .map(|entry| entry.path())
        .filter(|path| path.is_dir() && path.join("manage.py").is_file())
        .collect::<Vec<_>>();

    if nested_candidates.len() == 1 {
        return nested_candidates[0].clone();
    }

    cwd.to_path_buf()
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

#[cfg(test)]
mod tests {
    use super::resolve_project_root;
    use std::path::Path;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn unique_suffix() -> u128 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock should be after unix epoch")
            .as_nanos()
    }

    #[test]
    fn prefers_current_directory_when_manage_py_exists() {
        let base = std::env::temp_dir().join(format!(
            "djch-root-current-{}-{}",
            std::process::id(),
            unique_suffix()
        ));
        std::fs::create_dir_all(&base).expect("create temp dir");
        std::fs::write(base.join("manage.py"), "").expect("create manage.py");

        assert_eq!(resolve_project_root(&base), base);

        std::fs::remove_dir_all(&base).expect("cleanup temp dir");
    }

    #[test]
    fn picks_single_nested_django_directory() {
        let base = std::env::temp_dir().join(format!(
            "djch-root-nested-{}-{}",
            std::process::id(),
            unique_suffix()
        ));
        let app = base.join("app");
        std::fs::create_dir_all(&app).expect("create app dir");
        std::fs::write(app.join("manage.py"), "").expect("create nested manage.py");

        assert_eq!(resolve_project_root(Path::new(&base)), app);

        std::fs::remove_dir_all(&base).expect("cleanup temp dir");
    }
}
