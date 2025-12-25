use std::fs;
use std::path::Path;

use pydj_semantic::{ModelGraph, Parser};

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use tracing::{debug, info, trace};
use tracing_subscriber::{self, EnvFilter};

#[derive(Debug)]
struct Backend {
    client: Client,
    parser: Parser,
    model_graph: ModelGraph,
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
                        workspace_diagnostics: true,
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
        trace!(?params);
        let uri = params
            .text_document
            .uri
            .as_str()
            .strip_prefix("file://")
            .unwrap();
        let path = Path::new(uri);

        trace!(?uri);

        let diagnostics = self.parser.analyze_file(&path, &self.model_graph);
        debug!(?diagnostics);
    }

    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Result<DocumentDiagnosticReportResult> {
        todo!()
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

fn build_server_info() -> ServerInfo {
    ServerInfo {
        name: "pydjavu".into(),
        version: Some("0.0.1".into()),
    }
}

pub async fn serve(cwd: &Path) {
    let log_path = Path::new("/tmp/pydj.log");
    let file = fs::File::create(log_path).expect("create the log file");
    tracing_subscriber::fmt()
        .with_writer(file)
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    info!("Initializing LSP");

    let parser = Parser::new();
    let model_graph = parser.extract_model_graph(cwd).expect("parse model graph");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        parser,
        model_graph,
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
