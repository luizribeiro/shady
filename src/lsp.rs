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
                definition_provider: Some(OneOf::Left(true)),
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

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

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

        // Convert position to byte offset
        let offset = position_to_offset(&doc.text, position);

        // Find the identifier at the cursor position
        if let Some(identifier) = find_identifier_at_position(&doc.text, offset) {
            // Try to find the function definition
            for fn_def in &ast.fn_definitions {
                if fn_def.signature.fn_name == identifier {
                    // Found the definition - convert span to LSP range
                    let start_pos = offset_to_position(&doc.text, 0);
                    let end_pos = offset_to_position(&doc.text, identifier.len());

                    let location = Location {
                        uri: uri.clone(),
                        range: Range::new(start_pos, end_pos),
                    };

                    return Ok(Some(GotoDefinitionResponse::Scalar(location)));
                }
            }

            // Also check if the cursor is on a function call in the expression
            if let Some(location) = find_function_call_definition(&ast, &doc.text, uri, &identifier) {
                return Ok(Some(GotoDefinitionResponse::Scalar(location)));
            }
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

/// Convert LSP Position to byte offset
fn position_to_offset(text: &str, position: Position) -> usize {
    let mut offset = 0;
    let mut current_line = 0;
    let mut current_char = 0;

    for ch in text.chars() {
        if current_line == position.line as usize && current_char == position.character as usize {
            return offset;
        }

        if ch == '\n' {
            current_line += 1;
            current_char = 0;
        } else {
            current_char += 1;
        }

        offset += ch.len_utf8();
    }

    offset
}

/// Find the identifier at a given byte offset
fn find_identifier_at_position(text: &str, offset: usize) -> Option<String> {
    // Find the start and end of the identifier at this position
    let chars: Vec<char> = text.chars().collect();
    let mut char_offset = 0;
    let mut target_index = None;

    // Find the character index corresponding to the byte offset
    for (i, &ch) in chars.iter().enumerate() {
        if char_offset >= offset {
            target_index = Some(i);
            break;
        }
        char_offset += ch.len_utf8();
    }

    let target_index = target_index?;

    // Check if we're on a valid identifier character
    if target_index >= chars.len() || !is_identifier_char(chars[target_index]) {
        return None;
    }

    // Find the start of the identifier
    let mut start = target_index;
    while start > 0 && is_identifier_char(chars[start - 1]) {
        start -= 1;
    }

    // Find the end of the identifier
    let mut end = target_index;
    while end < chars.len() && is_identifier_char(chars[end]) {
        end += 1;
    }

    Some(chars[start..end].iter().collect())
}

/// Check if a character is valid in an identifier
fn is_identifier_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

/// Find the definition location for a function call
fn find_function_call_definition(
    ast: &crate::ast::ProgramAST,
    text: &str,
    uri: &Url,
    fn_name: &str,
) -> Option<Location> {
    use crate::ast;

    // Find the function definition in the AST
    let _fn_def = ast::get_fn_by_name(ast, fn_name)?;

    // We need to find where in the text this function is defined
    // Search for the function name in the definition context
    // For simplicity, we'll search for "fn_name =" or "fn_name $"
    let search_patterns = [
        format!("public {}", fn_name),
        format!("infix {}", fn_name),
        fn_name.to_string(),
    ];

    for (line_num, line) in text.lines().enumerate() {
        for pattern in &search_patterns {
            if let Some(col) = line.find(pattern) {
                // Check if this is actually a function definition (has '=' after parameters)
                if line.contains('=') {
                    // Extract just the function name position
                    let fn_name_col = if pattern.starts_with("public") || pattern.starts_with("infix") {
                        col + pattern.len() - fn_name.len()
                    } else {
                        col
                    };

                    let start_pos = Position::new(line_num as u32, fn_name_col as u32);
                    let end_pos = Position::new(line_num as u32, (fn_name_col + fn_name.len()) as u32);

                    return Some(Location {
                        uri: uri.clone(),
                        range: Range::new(start_pos, end_pos),
                    });
                }
            }
        }
    }

    None
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

    // Tests for Go to Definition functionality

    #[test]
    fn test_position_to_offset_single_line() {
        let text = "hello world";

        assert_eq!(position_to_offset(text, Position::new(0, 0)), 0);
        assert_eq!(position_to_offset(text, Position::new(0, 5)), 5);
        assert_eq!(position_to_offset(text, Position::new(0, 11)), 11);
    }

    #[test]
    fn test_position_to_offset_multiple_lines() {
        let text = "line 1\nline 2\nline 3";

        // Start of file
        assert_eq!(position_to_offset(text, Position::new(0, 0)), 0);

        // End of first line (before newline)
        assert_eq!(position_to_offset(text, Position::new(0, 6)), 6);

        // Start of second line (after first newline)
        assert_eq!(position_to_offset(text, Position::new(1, 0)), 7);

        // Middle of second line
        assert_eq!(position_to_offset(text, Position::new(1, 3)), 10);

        // Start of third line
        assert_eq!(position_to_offset(text, Position::new(2, 0)), 14);
    }

    #[test]
    fn test_position_to_offset_roundtrip() {
        let text = "public main = echo \"Hello\";\ngreet $name: str = echo $name;";

        // Test various positions
        let positions = vec![
            Position::new(0, 0),
            Position::new(0, 7),
            Position::new(0, 12),
            Position::new(1, 0),
            Position::new(1, 6),
            Position::new(1, 14),
        ];

        for pos in positions {
            let offset = position_to_offset(text, pos);
            let converted_back = offset_to_position(text, offset);
            assert_eq!(
                converted_back, pos,
                "Roundtrip failed for position {:?}",
                pos
            );
        }
    }

    #[test]
    fn test_find_identifier_at_position() {
        let text = "public main = echo \"Hello\";";

        // Test finding "public"
        assert_eq!(find_identifier_at_position(text, 0), Some("public".to_string()));
        assert_eq!(find_identifier_at_position(text, 3), Some("public".to_string()));

        // Test finding "main"
        assert_eq!(find_identifier_at_position(text, 7), Some("main".to_string()));
        assert_eq!(find_identifier_at_position(text, 10), Some("main".to_string()));

        // Test finding "echo"
        assert_eq!(find_identifier_at_position(text, 14), Some("echo".to_string()));
        assert_eq!(find_identifier_at_position(text, 17), Some("echo".to_string()));

        // Test finding nothing on whitespace
        assert_eq!(find_identifier_at_position(text, 6), None);

        // Test finding nothing on special characters
        assert_eq!(find_identifier_at_position(text, 12), None); // '='
    }

    #[test]
    fn test_find_identifier_with_underscores() {
        let text = "my_function = other_func 42;";

        // Test finding "my_function"
        assert_eq!(find_identifier_at_position(text, 0), Some("my_function".to_string()));
        assert_eq!(find_identifier_at_position(text, 5), Some("my_function".to_string()));
        assert_eq!(find_identifier_at_position(text, 10), Some("my_function".to_string()));

        // Test finding "other_func"
        assert_eq!(find_identifier_at_position(text, 14), Some("other_func".to_string()));
        assert_eq!(find_identifier_at_position(text, 23), Some("other_func".to_string()));
    }

    #[test]
    fn test_find_identifier_multiline() {
        let text = "public main = echo \"Hello\";\ngreet $name: str = echo $name;";

        // Find "main" on first line
        let main_offset = position_to_offset(text, Position::new(0, 7));
        assert_eq!(find_identifier_at_position(text, main_offset), Some("main".to_string()));

        // Find "greet" on second line
        let greet_offset = position_to_offset(text, Position::new(1, 0));
        assert_eq!(find_identifier_at_position(text, greet_offset), Some("greet".to_string()));

        // Find "name" on second line (first occurrence)
        let name1_offset = position_to_offset(text, Position::new(1, 7));
        assert_eq!(find_identifier_at_position(text, name1_offset), Some("name".to_string()));
    }

    #[test]
    fn test_is_identifier_char() {
        assert!(is_identifier_char('a'));
        assert!(is_identifier_char('Z'));
        assert!(is_identifier_char('0'));
        assert!(is_identifier_char('_'));

        assert!(!is_identifier_char(' '));
        assert!(!is_identifier_char('='));
        assert!(!is_identifier_char('$'));
        assert!(!is_identifier_char(';'));
        assert!(!is_identifier_char('('));
        assert!(!is_identifier_char(')'));
    }

    #[test]
    fn test_goto_definition_finds_function() {
        let code = r#"
public main = echo "Hello";
greet $name: str = echo $name;
add $a: int $b: int -> int = $a + $b;
"#;

        let ast = parse_script(code).unwrap();

        // Test finding "greet" function
        let url = Url::parse("file:///test.shady").unwrap();
        let location = find_function_call_definition(&ast, code, &url, "greet");
        assert!(location.is_some());

        let loc = location.unwrap();
        assert_eq!(loc.range.start.line, 2);
        assert!(loc.range.start.character < 10);

        // Test finding "add" function
        let location = find_function_call_definition(&ast, code, &url, "add");
        assert!(location.is_some());

        // Test not finding non-existent function
        let location = find_function_call_definition(&ast, code, &url, "nonexistent");
        assert!(location.is_none());
    }
}
