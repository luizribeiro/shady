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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_offset_to_position_single_line() {
        let text = "hello world";

        assert_eq!(offset_to_position(text, 0), Position::new(0, 0));
        assert_eq!(offset_to_position(text, 5), Position::new(0, 5));
        assert_eq!(offset_to_position(text, 11), Position::new(0, 11));
    }

    #[test]
    fn test_offset_to_position_multiple_lines() {
        let text = "line 1\nline 2\nline 3";

        // Start of file
        assert_eq!(offset_to_position(text, 0), Position::new(0, 0));

        // End of first line (before newline)
        assert_eq!(offset_to_position(text, 6), Position::new(0, 6));

        // Start of second line (after first newline)
        assert_eq!(offset_to_position(text, 7), Position::new(1, 0));

        // Middle of second line
        assert_eq!(offset_to_position(text, 10), Position::new(1, 3));

        // Start of third line
        assert_eq!(offset_to_position(text, 14), Position::new(2, 0));

        // End of file
        assert_eq!(offset_to_position(text, 20), Position::new(2, 6));
    }

    #[test]
    fn test_offset_to_position_empty_string() {
        let text = "";
        assert_eq!(offset_to_position(text, 0), Position::new(0, 0));
    }

    #[test]
    fn test_offset_to_position_only_newlines() {
        let text = "\n\n\n";

        assert_eq!(offset_to_position(text, 0), Position::new(0, 0));
        assert_eq!(offset_to_position(text, 1), Position::new(1, 0));
        assert_eq!(offset_to_position(text, 2), Position::new(2, 0));
        assert_eq!(offset_to_position(text, 3), Position::new(3, 0));
    }

    #[test]
    fn test_offset_to_position_with_unicode() {
        // Unicode character "😀" takes 4 bytes but is 1 character
        let text = "hello 😀 world";

        // Before emoji
        assert_eq!(offset_to_position(text, 6), Position::new(0, 6));

        // After emoji (emoji is 1 character position)
        assert_eq!(offset_to_position(text, 7), Position::new(0, 7));

        // After space after emoji
        assert_eq!(offset_to_position(text, 8), Position::new(0, 8));
    }

    #[test]
    fn test_offset_to_position_shady_code() {
        let text = "# Comment\npublic main = echo \"Hello\";\n";

        // Start of file
        assert_eq!(offset_to_position(text, 0), Position::new(0, 0));

        // Start of second line
        assert_eq!(offset_to_position(text, 10), Position::new(1, 0));

        // At "main"
        assert_eq!(offset_to_position(text, 17), Position::new(1, 7));

        // At semicolon
        assert_eq!(offset_to_position(text, 37), Position::new(1, 27));
    }

    #[test]
    fn test_parse_valid_shady_code() {
        let valid_code = "public main = echo \"Hello\";";

        // Should parse without errors
        match parse_script(valid_code) {
            Ok(ast) => {
                assert_eq!(ast.fn_definitions.len(), 1);
                assert_eq!(ast.fn_definitions[0].signature.fn_name, "main");
            }
            Err(e) => panic!("Expected valid code to parse, got error: {:?}", e),
        }
    }

    #[test]
    fn test_parse_invalid_shady_code_missing_semicolon() {
        let invalid_code = "public main = echo \"Hello\"";

        // Should fail to parse
        match parse_script(invalid_code) {
            Ok(_) => panic!("Expected invalid code to fail parsing"),
            Err(e) => {
                // Verify it's a parse error
                assert!(matches!(e, ShadyError::ParseErrorSimple { .. }));
            }
        }
    }

    #[test]
    fn test_parse_invalid_shady_code_bad_syntax() {
        let invalid_code = "public main = = echo";

        // Should fail to parse
        match parse_script(invalid_code) {
            Ok(_) => panic!("Expected invalid code to fail parsing"),
            Err(_) => {
                // Expected to fail
            }
        }
    }

    #[test]
    fn test_parse_multiple_functions() {
        let code = r#"
public main = echo "Hello";
greet $name: str = echo ("Hi, " + $name);
add $a: int $b: int -> int = $a + $b;
"#;

        match parse_script(code) {
            Ok(ast) => {
                assert_eq!(ast.fn_definitions.len(), 3);
                assert_eq!(ast.fn_definitions[0].signature.fn_name, "main");
                assert_eq!(ast.fn_definitions[1].signature.fn_name, "greet");
                assert_eq!(ast.fn_definitions[2].signature.fn_name, "add");
            }
            Err(e) => panic!("Expected valid code to parse, got error: {:?}", e),
        }
    }

    #[test]
    fn test_parse_empty_file() {
        let code = "";

        match parse_script(code) {
            Ok(ast) => {
                assert_eq!(ast.fn_definitions.len(), 0);
            }
            Err(e) => panic!("Expected empty file to parse, got error: {:?}", e),
        }
    }

    #[test]
    fn test_parse_only_comments() {
        let code = "# This is a comment\n# Another comment";

        match parse_script(code) {
            Ok(ast) => {
                assert_eq!(ast.fn_definitions.len(), 0);
            }
            Err(e) => panic!("Expected comments-only file to parse, got error: {:?}", e),
        }
    }

    #[test]
    fn test_diagnostic_position_for_parse_error() {
        let invalid_code = "public main = echo \"Hello\"";

        match parse_script(invalid_code) {
            Err(ShadyError::ParseErrorSimple { message: _, span }) => {
                // Get the position of the error
                let start_pos = offset_to_position(invalid_code, span.offset());
                let end_pos = offset_to_position(invalid_code, span.offset() + span.len());

                // The error should be at or near the end where semicolon is missing
                assert!(start_pos.line == 0);
                assert!(start_pos.character > 0);
                assert!(end_pos.line == 0);

                // Range should be valid
                assert!(end_pos.character >= start_pos.character);
            }
            Err(e) => panic!("Expected ParseErrorSimple, got: {:?}", e),
            Ok(_) => panic!("Expected code to fail parsing"),
        }
    }

    #[test]
    fn test_diagnostic_position_multiline_error() {
        let invalid_code = "public main = echo \"Hello\";\n\ngreet $name: str = echo";

        match parse_script(invalid_code) {
            Err(ShadyError::ParseErrorSimple { message: _, span }) => {
                let start_pos = offset_to_position(invalid_code, span.offset());

                // Error should be on line 2 (0-indexed)
                assert!(start_pos.line >= 2);
            }
            Err(e) => panic!("Expected ParseErrorSimple, got: {:?}", e),
            Ok(_) => panic!("Expected code to fail parsing"),
        }
    }

    // Integration-style tests using mock client would go here
    // For now, we've tested the core helper functions and parsing logic
}
