use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use django_check_semantic::{ModelGraph, Parser, QueryFunction};

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use tracing::{debug, error, info, trace};

#[derive(Debug, Clone)]
struct Backend {
    #[allow(dead_code)]
    cwd: PathBuf,
    current_source: Arc<Mutex<String>>,
    client: Client,
    parser: Parser,
    model_graph: Arc<Mutex<ModelGraph>>,
    functions: Vec<QueryFunction>,
}

impl Backend {
    fn reanalyze_graph(&self) -> std::result::Result<(), Box<dyn std::error::Error>> {
        *self.model_graph.lock().unwrap() = self.parser.extract_model_graph(&self.cwd)?;
        Ok(())
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        debug!("method: initialize");
        trace!(?params, "received");

        let result = InitializeResult {
            server_info: Some(build_server_info()),
            capabilities: ServerCapabilities {
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions {
                        identifier: None,
                        inter_file_dependencies: false,
                        workspace_diagnostics: false,
                        work_done_progress_options: WorkDoneProgressOptions::default(),
                    },
                )),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                ..Default::default()
            },
        };

        trace!(?result);

        Ok(result)
    }

    async fn initialized(&self, _params: InitializedParams) {
        debug!("method: Initialized");

        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        debug!("method: textDocument/didOpen");
        *self.current_source.lock().unwrap() = params.text_document.text;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        debug!("method: textDocument/didChange");
        if let Some(cch) = params.content_changes.first() {
            *self.current_source.lock().unwrap() = cch.text.clone();
            if check_if_contains_models(&cch.text) {
                debug!("re-analyzing graph");
                self.reanalyze_graph().unwrap_or_else(|e| {
                    error!(?e, "updating graph");
                });
            }
        }
    }

    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Result<DocumentDiagnosticReportResult> {
        let mut diagnostics = Vec::new();
        let file_path = params
            .text_document
            .uri
            .as_str()
            .strip_prefix("file://")
            .unwrap_or(params.text_document.uri.as_str());

        trace!(?file_path);

        match self.parser.analyze_source(
            &self.current_source.lock().unwrap(),
            file_path
                .strip_prefix(self.cwd.to_str().unwrap_or_default())
                .unwrap_or(file_path),
            &self.model_graph.lock().unwrap(),
            &self.functions,
        ) {
            Ok(diags) => diagnostics = diags,
            Err(e) => {
                debug!(?e);
            }
        }

        debug!(?diagnostics);
        Ok(DocumentDiagnosticReportResult::Report(
            DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                related_documents: None,
                full_document_diagnostic_report: FullDocumentDiagnosticReport {
                    result_id: params.identifier,
                    items: diagnostics.into_iter().map(|d| d.into()).collect(),
                },
            }),
        ))
    }

    async fn shutdown(&self) -> Result<()> {
        info!("Shutting down the server");
        Ok(())
    }
}

fn build_server_info() -> ServerInfo {
    ServerInfo {
        name: "django-check".into(),
        version: Some("0.0.1".into()),
    }
}

/// Basic check if the document is a model document
fn check_if_contains_models(source: &str) -> bool {
    source.contains("(models.Model)") || source.contains("(Model)")
}

pub async fn serve(cwd: &Path, model_graph: ModelGraph, functions: Vec<QueryFunction>) {
    trace!(?model_graph);

    info!("Initializing LSP");

    let parser = Parser::new();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        cwd: cwd.to_path_buf(),
        client,
        parser,
        model_graph: Arc::new(Mutex::new(model_graph)),
        functions,
        current_source: Arc::new(Mutex::new(String::new())),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
