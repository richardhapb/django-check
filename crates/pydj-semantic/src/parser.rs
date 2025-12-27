//! File parsing and analysis orchestration.

use ruff_python_ast::ModModule;
use ruff_python_parser::{Parsed, parse_module};
use std::fs;
use std::path::Path;
use walkdir::WalkDir;

use crate::diagnostic::NPlusOneDiagnostic;
use crate::ir::model::ModelGraph;
use crate::passes::{
    Pass,
    functions::{QueryFunction, QueryFunctionPass},
    model_graph::ModelGraphPass,
    n_plus_one::NPlusOnePass,
};

#[derive(Debug, Clone)]
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

    fn analyze_n_plus_one_file(
        &self,
        file: &Path,
        model_graph: &ModelGraph,
        functions: &[QueryFunction],
    ) -> Result<Vec<NPlusOneDiagnostic>, Box<dyn std::error::Error>> {
        let source = fs::read_to_string(file)?;
        self.analyze_source(
            &source,
            file.file_name().unwrap().to_str().unwrap(),
            model_graph,
            functions,
        )
    }

    /// Run N+1 detection on a file or a directory, this function figure out
    /// if the path contains a file or a directory and perform the analysis
    /// in consequence.
    pub fn analyze_n_plus_one(
        &self,
        path: &Path,
        model_graph: &ModelGraph,
        functions: &[QueryFunction],
    ) -> Result<Vec<NPlusOneDiagnostic>, Box<dyn std::error::Error>> {
        let mut all_diagnostics = Vec::new();

        if path.is_dir() {
            for entry in Self::python_files(path) {
                let diagnostics =
                    self.analyze_n_plus_one_file(entry.path(), model_graph, functions)?;
                all_diagnostics.extend(diagnostics);
            }
        } else if path.is_file() {
            all_diagnostics = self.analyze_n_plus_one_file(path, model_graph, functions)?;
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

    pub fn extract_functions(
        &self,
        dir: &Path,
    ) -> Result<Vec<QueryFunction>, Box<dyn std::error::Error>> {
        let mut functions = Vec::new();
        for entry in Self::python_files(dir) {
            let source = fs::read_to_string(entry.path())?;
            let parsed = self.parse_module(&source)?;
            // let filename = Self::relative_path(dir, entry.path());

            let mut pass = QueryFunctionPass::new();
            let result = pass.run(parsed.syntax());
            functions.extend(result);
        }

        Ok(functions)
    }

    /// Run all analyses and print results (current behavior)
    pub fn analyze_directory(
        &self,
        dir: &Path,
        model_graph: &ModelGraph,
        functions: &[QueryFunction],
    ) -> Result<(), Box<dyn std::error::Error>> {
        let diagnostics = self.analyze_n_plus_one(dir, model_graph, functions)?;

        for diag in &diagnostics {
            println!("{}\n", diag);
        }

        println!("\nTotal N+1 warnings: {}", diagnostics.len());
        Ok(())
    }

    /// Run all analyses in a single file and return diagnostics
    pub fn analyze_file(
        &self,
        file: &Path,
        model_graph: &ModelGraph,
        functions: &[QueryFunction],
    ) -> Result<Vec<NPlusOneDiagnostic>, Box<dyn std::error::Error>> {
        Ok(self.analyze_n_plus_one_file(file, model_graph, functions)?)
    }

    /// Run all analyses in a source code and return diagnostics
    pub fn analyze_source(
        &self,
        source: &str,
        file_name: &str,
        model_graph: &ModelGraph,
        functions: &[QueryFunction],
    ) -> Result<Vec<NPlusOneDiagnostic>, Box<dyn std::error::Error>> {
        let parsed = self.parse_module(source)?;

        let mut n1_pass = NPlusOnePass::new(file_name, &source, model_graph, functions);
        Ok(n1_pass.run(parsed.syntax()))
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
