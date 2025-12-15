//! File parsing and analysis orchestration.

use ruff_python_ast::ModModule;
use ruff_python_parser::{Parsed, parse_module};
use std::fs;
use std::path::Path;
use walkdir::WalkDir;

use crate::diagnostic::Diagnostic;
use crate::ir::model::ModelGraph;
use crate::passes::Pass;
use crate::passes::model_graph::ModelGraphPass;
use crate::passes::n_plus_one::NPlusOnePass;

pub struct Parser;

impl Parser {
    pub fn new() -> Self {
        Self
    }

    pub fn parse_module(
        &self,
        source: &str,
    ) -> Result<Parsed<ModModule>, Box<dyn std::error::Error>> {
        Ok(parse_module(source)?)
    }

    /// Run N+1 detection on a directory
    pub fn analyze_n_plus_one(
        &self,
        dir: &Path,
    ) -> Result<Vec<Diagnostic>, Box<dyn std::error::Error>> {
        let mut all_diagnostics = Vec::new();

        let mut global_graph = ModelGraph::new();
        for entry in Self::python_files(dir) {
            let source = fs::read_to_string(entry.path())?;
            let parsed = self.parse_module(&source)?;
            let filename = Self::relative_path(dir, entry.path());

            let mut graph_pass = ModelGraphPass::new(&filename, &source);
            let file_graph = graph_pass.run(parsed.syntax());
            global_graph.merge(file_graph);
        }

        for entry in Self::python_files(dir) {
            let source = fs::read_to_string(entry.path())?;
            let parsed = self.parse_module(&source)?;
            let filename = Self::relative_path(dir, entry.path());

            let mut n1_pass = NPlusOnePass::new(&filename, &source, &global_graph);
            let diagnostics = n1_pass.run(parsed.syntax());
            all_diagnostics.extend(diagnostics);
        }

        Ok(all_diagnostics)
    }

    /// Extract Django model dependency graph from a directory
    pub fn extract_model_graph(
        &self,
        dir: &Path,
    ) -> Result<ModelGraph, Box<dyn std::error::Error>> {
        let mut combined_graph = ModelGraph::new();

        for entry in Self::python_files(dir) {
            let source = fs::read_to_string(entry.path())?;
            let parsed = self.parse_module(&source)?;
            let filename = Self::relative_path(dir, entry.path());

            let mut pass = ModelGraphPass::new(&filename, &source);
            let graph = pass.run(parsed.syntax());
            combined_graph.merge(graph);
        }

        Ok(combined_graph)
    }

    /// Run all analyses and print results (current behavior)
    pub fn analyze_directory(&self, dir: &Path) -> Result<(), Box<dyn std::error::Error>> {
        let diagnostics = self.analyze_n_plus_one(dir)?;

        for diag in &diagnostics {
            println!("{}\n", diag);
        }

        println!("\nTotal N+1 warnings: {}", diagnostics.len());
        Ok(())
    }

    fn python_files(dir: &Path) -> impl Iterator<Item = walkdir::DirEntry> {
        WalkDir::new(dir)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.file_name().to_string_lossy().ends_with(".py"))
    }

    fn relative_path(base: &Path, full: &Path) -> String {
        full.strip_prefix(base)
            .unwrap_or(full)
            .to_string_lossy()
            .to_string()
    }
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}
