use crate::ast::parse_script;
use crate::error::ShadyError;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

/// Stores document state for the LSP server
#[derive(Debug, Clone)]
struct DocumentState {
    text: String,
}

/// Backend state for the Shady LSP server
pub struct Backend {
    client: Client,
    documents: Arc<RwLock<HashMap<Url, DocumentState>>>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Parse a document and generate diagnostics
    async fn parse_and_diagnose(&self, uri: &Url, text: &str) {
        let mut diagnostics = Vec::new();

        match parse_script(text) {
            Ok(_ast) => {
                // Parsing succeeded - no diagnostics to report
            }
            Err(err) => {
                // Convert parse error to LSP diagnostic
                let diagnostic = match err {
                    ShadyError::ParseErrorSimple { message, span } => {
                        let start_pos = offset_to_position(text, span.offset());
                        let end_pos = offset_to_position(text, span.offset() + span.len());

                        Diagnostic {
                            range: Range::new(start_pos, end_pos),
                            severity: Some(DiagnosticSeverity::ERROR),
                            source: Some("shady".to_string()),
                            message,
                            ..Default::default()
                        }
                    }
                    other_err => {
                        // For other errors, show at the beginning of the file
                        Diagnostic {
                            range: Range::new(Position::new(0, 0), Position::new(0, 1)),
                            severity: Some(DiagnosticSeverity::ERROR),
                            source: Some("shady".to_string()),
                            message: format!("{:?}", other_err),
                            ..Default::default()
                        }
                    }
                };

                diagnostics.push(diagnostic);
            }
        }

        // Store the document text
        let mut docs = self.documents.write().await;
        docs.insert(
            uri.clone(),
            DocumentState {
                text: text.to_string(),
            },
        );

        // Publish diagnostics to the client
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "shady-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Shady LSP server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;

        self.parse_and_diagnose(&uri, &text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;

        // Since we use FULL sync, we only get the full text
        if let Some(change) = params.content_changes.first() {
            self.parse_and_diagnose(&uri, &change.text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;

        // Clear diagnostics
        self.client
            .publish_diagnostics(uri.clone(), Vec::new(), None)
            .await;

        // Remove from document store
        let mut docs = self.documents.write().await;
        docs.remove(&uri);
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let _position = params.text_document_position_params.position;

        let docs = self.documents.read().await;
        let doc = match docs.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        // Re-parse the document to get the AST
        let ast = match parse_script(&doc.text) {
            Ok(ast) => ast,
            Err(_) => return Ok(None),
        };

        // For now, just show information about the first function
        // In a more complete implementation, we'd check cursor position
        // and show information about the function/variable under the cursor
        if let Some(fn_def) = ast.fn_definitions.first() {
            let hover_text = format!(
                "```shady\n{}\n```",
                fn_def.signature
            );

            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: hover_text,
                }),
                range: None,
            }));
        }

        Ok(None)
    }
}

/// Convert a byte offset to LSP Position (line and character)
fn offset_to_position(text: &str, offset: usize) -> Position {
    let mut line = 0;
    let mut character = 0;

    for (i, ch) in text.chars().enumerate() {
        if i >= offset {
            break;
        }

        if ch == '\n' {
            line += 1;
            character = 0;
        } else {
            character += 1;
        }
    }

    Position::new(line, character)
}

/// Create and run the LSP server
pub async fn run_server() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;
}
