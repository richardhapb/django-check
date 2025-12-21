use pydj_semantic::Parser;
use std::env::home_dir;

fn main() {
    let home = home_dir().unwrap();
    let parser = Parser::new();

    if let Err(e) = parser.analyze_directory(&home.join("dev/agora_hedge/main/app")) {
        eprintln!("Error: {}", e);
    }
}
