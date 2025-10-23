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
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![" ".to_string(), "$".to_string()]),
                    ..Default::default()
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec![" ".to_string(), "$".to_string()]),
                    retrigger_characters: None,
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                }),
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
            let hover_text = format!("```shady\n{}\n```", fn_def.signature);

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
            if let Some(location) = find_function_call_definition(&ast, &doc.text, uri, &identifier)
            {
                return Ok(Some(GotoDefinitionResponse::Scalar(location)));
            }
        }

        Ok(None)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let _position = params.text_document_position.position;

        let docs = self.documents.read().await;
        let doc = match docs.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        // Re-parse the document to get the AST
        let ast = match parse_script(&doc.text) {
            Ok(ast) => ast,
            Err(_) => {
                // Even if parsing fails, we can still provide basic completions
                return Ok(Some(CompletionResponse::Array(get_builtin_completions())));
            }
        };

        // Get all available completions
        let mut completions = Vec::new();

        // Add user-defined functions
        for fn_def in &ast.fn_definitions {
            let detail = if fn_def.signature.parameters.is_empty() {
                format!(
                    "{} -> {}",
                    fn_def.signature.fn_name, fn_def.signature.return_type
                )
            } else {
                fn_def.signature.to_string()
            };

            completions.push(CompletionItem {
                label: fn_def.signature.fn_name.clone(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(detail),
                documentation: Some(Documentation::String(format!(
                    "User-defined function{}",
                    if fn_def.signature.is_public {
                        " (public)"
                    } else {
                        ""
                    }
                ))),
                ..Default::default()
            });
        }

        // Add builtin functions
        completions.extend(get_builtin_completions());

        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
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
            Err(_) => {
                // Even if parsing fails, we can still provide builtin signatures
                return Ok(None);
            }
        };

        // Find the function call at the cursor position
        let _offset = position_to_offset(&doc.text, position);

        // Get the line up to the cursor to find what function is being called
        let line_text = doc.text.lines().nth(position.line as usize).unwrap_or("");
        let line_up_to_cursor =
            &line_text[..position.character.min(line_text.len() as u32) as usize];

        // Try to extract the function name before the cursor
        if let Some(fn_name) = extract_function_name_at_cursor(line_up_to_cursor) {
            // Look for user-defined function
            for fn_def in &ast.fn_definitions {
                if fn_def.signature.fn_name == fn_name {
                    let signature_info = create_signature_info(&fn_def.signature);
                    return Ok(Some(SignatureHelp {
                        signatures: vec![signature_info],
                        active_signature: Some(0),
                        active_parameter: None,
                    }));
                }
            }

            // Look for builtin function
            if let Some(signature_info) = get_builtin_signature(&fn_name) {
                return Ok(Some(SignatureHelp {
                    signatures: vec![signature_info],
                    active_signature: Some(0),
                    active_parameter: None,
                }));
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
                    let fn_name_col =
                        if pattern.starts_with("public") || pattern.starts_with("infix") {
                            col + pattern.len() - fn_name.len()
                        } else {
                            col
                        };

                    let start_pos = Position::new(line_num as u32, fn_name_col as u32);
                    let end_pos =
                        Position::new(line_num as u32, (fn_name_col + fn_name.len()) as u32);

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

/// Extract the function name being called at the cursor position
fn extract_function_name_at_cursor(line_up_to_cursor: &str) -> Option<String> {
    // In Shady, function calls look like: "function_name $arg1 $arg2"
    // We want to find the function name (first word after '=' if present, or first word on line)

    let trimmed = line_up_to_cursor.trim();

    // Find the position after '=' if present
    let start_pos = if let Some(eq_pos) = trimmed.rfind('=') {
        eq_pos + 1
    } else {
        0
    };

    let relevant_part = &trimmed[start_pos..].trim_start();

    // Get the first word (the function name)
    let words: Vec<&str> = relevant_part.split_whitespace().collect();

    if let Some(first_word) = words.first() {
        // Remove any leading/trailing non-identifier characters (like $ or parentheses)
        let clean_word = first_word
            .trim_start_matches(|c: char| !c.is_alphanumeric() && c != '_')
            .trim_end_matches(|c: char| !c.is_alphanumeric() && c != '_');
        if !clean_word.is_empty() && clean_word.chars().all(|c| c.is_alphanumeric() || c == '_') {
            return Some(clean_word.to_string());
        }
    }

    None
}

/// Create signature information from a function signature
fn create_signature_info(signature: &crate::ast::FnSignature) -> SignatureInformation {
    let mut label = signature.fn_name.clone();
    let mut parameters = Vec::new();

    if !signature.parameters.is_empty() {
        label.push(' ');
        for (i, param) in signature.parameters.iter().enumerate() {
            if i > 0 {
                label.push(' ');
            }
            let param_label = format!("${}: {}", param.name, param.typ);
            let start = label.len();
            label.push_str(&param_label);
            let end = label.len();

            parameters.push(ParameterInformation {
                label: ParameterLabel::LabelOffsets([start as u32, end as u32]),
                documentation: None,
            });
        }
    }

    // Add return type if it's not Any (always outside the parameters block)
    // Note: Can't use != because Type::Any == everything in the PartialEq impl
    if !matches!(signature.return_type, crate::types::Type::Any) {
        label.push_str(&format!(" -> {}", signature.return_type));
    }

    SignatureInformation {
        label,
        documentation: None,
        parameters: if parameters.is_empty() {
            None
        } else {
            Some(parameters)
        },
        active_parameter: None,
    }
}

/// Get signature information for a builtin function
fn get_builtin_signature(fn_name: &str) -> Option<SignatureInformation> {
    let (label, params) = match fn_name {
        "exec" => ("exec $proc: proc -> int", vec!["$proc: proc"]),
        "seq" => ("seq $procs: [proc] -> int", vec!["$procs: [proc]"]),
        "stdout" => ("stdout $proc: proc -> str", vec!["$proc: proc"]),
        "print" => ("print $s: str -> int", vec!["$s: str"]),
        "lines" => ("lines $input: str|proc -> [str]", vec!["$input: str|proc"]),
        "to_string" => ("to_string $i: int -> str", vec!["$i: int"]),
        "first" => ("first $list: [any] -> any", vec!["$list: [any]"]),
        "add_all" => ("add_all $list: [int] -> int", vec!["$list: [int]"]),
        "env" => (
            "env $var_name: str $default: str -> str",
            vec!["$var_name: str", "$default: str"],
        ),
        "os" => ("os -> str", vec![]),
        "echo" => ("echo $msg: str -> proc", vec!["$msg: str"]),
        "cat" => ("cat $file: str -> proc", vec!["$file: str"]),
        _ => return None,
    };

    let mut parameters = Vec::new();
    let mut current_pos = fn_name.len() + 1; // Account for function name and space

    for param in params {
        let start = current_pos;
        let end = start + param.len();
        parameters.push(ParameterInformation {
            label: ParameterLabel::LabelOffsets([start as u32, end as u32]),
            documentation: None,
        });
        current_pos = end + 1; // Account for space between parameters
    }

    Some(SignatureInformation {
        label: label.to_string(),
        documentation: None,
        parameters: if parameters.is_empty() {
            None
        } else {
            Some(parameters)
        },
        active_parameter: None,
    })
}

/// Get completion items for builtin functions
fn get_builtin_completions() -> Vec<CompletionItem> {
    vec![
        CompletionItem {
            label: "echo".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("echo $msg: str -> proc".to_string()),
            documentation: Some(Documentation::String(
                "Print a message to stdout (external command)".to_string(),
            )),
            ..Default::default()
        },
        CompletionItem {
            label: "print".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("print $s: str -> int".to_string()),
            documentation: Some(Documentation::String(
                "Print a string and return 0".to_string(),
            )),
            ..Default::default()
        },
        CompletionItem {
            label: "exec".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("exec $proc: proc -> int".to_string()),
            documentation: Some(Documentation::String(
                "Execute a process and return its exit code".to_string(),
            )),
            ..Default::default()
        },
        CompletionItem {
            label: "seq".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("seq $procs: [proc] -> int".to_string()),
            documentation: Some(Documentation::String(
                "Execute a sequence of processes and return the last exit code".to_string(),
            )),
            ..Default::default()
        },
        CompletionItem {
            label: "stdout".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("stdout $proc: proc -> str".to_string()),
            documentation: Some(Documentation::String(
                "Capture stdout from a process as a string".to_string(),
            )),
            ..Default::default()
        },
        CompletionItem {
            label: "lines".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("lines $s: str -> [str] or lines $proc: proc -> [str]".to_string()),
            documentation: Some(Documentation::String(
                "Split a string or process output into lines".to_string(),
            )),
            ..Default::default()
        },
        CompletionItem {
            label: "to_string".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("to_string $i: int -> str".to_string()),
            documentation: Some(Documentation::String(
                "Convert an integer to a string".to_string(),
            )),
            ..Default::default()
        },
        CompletionItem {
            label: "first".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("first $list: [any] -> any".to_string()),
            documentation: Some(Documentation::String(
                "Get the first element of a list".to_string(),
            )),
            ..Default::default()
        },
        CompletionItem {
            label: "add_all".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("add_all $list: [int] -> int".to_string()),
            documentation: Some(Documentation::String(
                "Sum all integers in a list".to_string(),
            )),
            ..Default::default()
        },
        CompletionItem {
            label: "env".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("env $var_name: str $default: str -> str".to_string()),
            documentation: Some(Documentation::String(
                "Get an environment variable with a default value".to_string(),
            )),
            ..Default::default()
        },
        CompletionItem {
            label: "os".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("os -> str".to_string()),
            documentation: Some(Documentation::String(
                "Get the operating system name".to_string(),
            )),
            ..Default::default()
        },
        CompletionItem {
            label: "cat".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("cat $file: str -> proc".to_string()),
            documentation: Some(Documentation::String(
                "Read and display file contents (external command)".to_string(),
            )),
            ..Default::default()
        },
    ]
}

/// Create and run the LSP server
pub async fn run_server() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
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
        assert_eq!(
            find_identifier_at_position(text, 0),
            Some("public".to_string())
        );
        assert_eq!(
            find_identifier_at_position(text, 3),
            Some("public".to_string())
        );

        // Test finding "main"
        assert_eq!(
            find_identifier_at_position(text, 7),
            Some("main".to_string())
        );
        assert_eq!(
            find_identifier_at_position(text, 10),
            Some("main".to_string())
        );

        // Test finding "echo"
        assert_eq!(
            find_identifier_at_position(text, 14),
            Some("echo".to_string())
        );
        assert_eq!(
            find_identifier_at_position(text, 17),
            Some("echo".to_string())
        );

        // Test finding nothing on whitespace
        assert_eq!(find_identifier_at_position(text, 6), None);

        // Test finding nothing on special characters
        assert_eq!(find_identifier_at_position(text, 12), None); // '='
    }

    #[test]
    fn test_find_identifier_with_underscores() {
        let text = "my_function = other_func 42;";

        // Test finding "my_function"
        assert_eq!(
            find_identifier_at_position(text, 0),
            Some("my_function".to_string())
        );
        assert_eq!(
            find_identifier_at_position(text, 5),
            Some("my_function".to_string())
        );
        assert_eq!(
            find_identifier_at_position(text, 10),
            Some("my_function".to_string())
        );

        // Test finding "other_func"
        assert_eq!(
            find_identifier_at_position(text, 14),
            Some("other_func".to_string())
        );
        assert_eq!(
            find_identifier_at_position(text, 23),
            Some("other_func".to_string())
        );
    }

    #[test]
    fn test_find_identifier_multiline() {
        let text = "public main = echo \"Hello\";\ngreet $name: str = echo $name;";

        // Find "main" on first line
        let main_offset = position_to_offset(text, Position::new(0, 7));
        assert_eq!(
            find_identifier_at_position(text, main_offset),
            Some("main".to_string())
        );

        // Find "greet" on second line
        let greet_offset = position_to_offset(text, Position::new(1, 0));
        assert_eq!(
            find_identifier_at_position(text, greet_offset),
            Some("greet".to_string())
        );

        // Find "name" on second line (first occurrence)
        let name1_offset = position_to_offset(text, Position::new(1, 7));
        assert_eq!(
            find_identifier_at_position(text, name1_offset),
            Some("name".to_string())
        );
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

    // Tests for Autocompletion functionality

    #[test]
    fn test_get_builtin_completions() {
        let completions = get_builtin_completions();

        // Should have at least the core builtin functions
        assert!(completions.len() >= 10);

        // Check for some specific builtins
        let labels: Vec<String> = completions.iter().map(|c| c.label.clone()).collect();
        assert!(labels.contains(&"exec".to_string()));
        assert!(labels.contains(&"stdout".to_string()));
        assert!(labels.contains(&"print".to_string()));
        assert!(labels.contains(&"first".to_string()));
        assert!(labels.contains(&"env".to_string()));

        // Check that completions have proper metadata
        for completion in &completions {
            assert!(completion.kind == Some(CompletionItemKind::FUNCTION));
            assert!(completion.detail.is_some());
            assert!(completion.documentation.is_some());
        }
    }

    #[test]
    fn test_completion_includes_user_functions() {
        let code = r#"
public main = echo "Hello";
greet $name: str = echo $name;
add $a: int $b: int -> int = $a + $b;
"#;

        let ast = parse_script(code).unwrap();

        // Simulate what the completion handler does
        let mut completions = Vec::new();

        // Add user-defined functions
        for fn_def in &ast.fn_definitions {
            let detail = if fn_def.signature.parameters.is_empty() {
                format!(
                    "{} -> {}",
                    fn_def.signature.fn_name, fn_def.signature.return_type
                )
            } else {
                fn_def.signature.to_string()
            };

            completions.push(CompletionItem {
                label: fn_def.signature.fn_name.clone(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(detail),
                documentation: Some(Documentation::String(format!(
                    "User-defined function{}",
                    if fn_def.signature.is_public {
                        " (public)"
                    } else {
                        ""
                    }
                ))),
                ..Default::default()
            });
        }

        // Verify user functions are included
        assert_eq!(completions.len(), 3);

        let labels: Vec<String> = completions.iter().map(|c| c.label.clone()).collect();
        assert!(labels.contains(&"main".to_string()));
        assert!(labels.contains(&"greet".to_string()));
        assert!(labels.contains(&"add".to_string()));

        // Check that main is marked as public
        let main_completion = completions.iter().find(|c| c.label == "main").unwrap();
        match &main_completion.documentation {
            Some(Documentation::String(s)) => assert!(s.contains("public")),
            _ => panic!("Expected string documentation"),
        }
    }

    #[test]
    fn test_completion_with_invalid_code() {
        let code = "public main = echo"; // Invalid - missing semicolon

        // Should still provide builtin completions even if parsing fails
        match parse_script(code) {
            Err(_) => {
                let completions = get_builtin_completions();
                assert!(!completions.is_empty());
            }
            Ok(_) => panic!("Expected parsing to fail"),
        }
    }

    #[test]
    fn test_completion_item_details() {
        let completions = get_builtin_completions();

        // Check specific completion details
        let exec_completion = completions.iter().find(|c| c.label == "exec").unwrap();
        assert_eq!(
            exec_completion.detail,
            Some("exec $proc: proc -> int".to_string())
        );
        assert!(exec_completion.documentation.is_some());

        let stdout_completion = completions.iter().find(|c| c.label == "stdout").unwrap();
        assert_eq!(
            stdout_completion.detail,
            Some("stdout $proc: proc -> str".to_string())
        );

        let first_completion = completions.iter().find(|c| c.label == "first").unwrap();
        assert_eq!(
            first_completion.detail,
            Some("first $list: [any] -> any".to_string())
        );
    }

    #[test]
    fn test_completion_function_signatures() {
        let code = r#"
simple = 42;
with_params $x: int $y: str = echo $y;
with_return -> int = 100;
"#;

        let ast = parse_script(code).unwrap();

        let mut completions = Vec::new();
        for fn_def in &ast.fn_definitions {
            let detail = if fn_def.signature.parameters.is_empty() {
                format!(
                    "{} -> {}",
                    fn_def.signature.fn_name, fn_def.signature.return_type
                )
            } else {
                fn_def.signature.to_string()
            };
            completions.push((fn_def.signature.fn_name.clone(), detail));
        }

        // Check that we format details correctly
        let simple_detail = completions
            .iter()
            .find(|(name, _)| name == "simple")
            .unwrap();
        assert!(simple_detail.1.contains("simple"));

        let with_params_detail = completions
            .iter()
            .find(|(name, _)| name == "with_params")
            .unwrap();
        assert!(with_params_detail.1.contains("with_params"));
        assert!(with_params_detail.1.contains("int"));
        assert!(with_params_detail.1.contains("str"));
    }

    // Tests for Signature Help functionality

    #[test]
    fn test_extract_function_name_at_cursor() {
        // Simple function call
        assert_eq!(
            extract_function_name_at_cursor("echo "),
            Some("echo".to_string())
        );
        assert_eq!(
            extract_function_name_at_cursor("echo $"),
            Some("echo".to_string())
        );

        // Function call in expression
        assert_eq!(
            extract_function_name_at_cursor("public main = echo "),
            Some("echo".to_string())
        );
        assert_eq!(
            extract_function_name_at_cursor("result = stdout "),
            Some("stdout".to_string())
        );

        // Function with parameters already typed
        assert_eq!(
            extract_function_name_at_cursor("env "),
            Some("env".to_string())
        );
        assert_eq!(
            extract_function_name_at_cursor("env $var "),
            Some("env".to_string())
        );

        // No function
        assert_eq!(extract_function_name_at_cursor("   "), None);
        assert_eq!(extract_function_name_at_cursor(""), None);
    }

    #[test]
    fn test_get_builtin_signature() {
        // Test exec signature
        let sig = get_builtin_signature("exec").unwrap();
        assert_eq!(sig.label, "exec $proc: proc -> int");
        assert!(sig.parameters.is_some());
        assert_eq!(sig.parameters.as_ref().unwrap().len(), 1);

        // Test env signature (multiple parameters)
        let sig = get_builtin_signature("env").unwrap();
        assert_eq!(sig.label, "env $var_name: str $default: str -> str");
        assert!(sig.parameters.is_some());
        assert_eq!(sig.parameters.as_ref().unwrap().len(), 2);

        // Test os signature (no parameters)
        let sig = get_builtin_signature("os").unwrap();
        assert_eq!(sig.label, "os -> str");
        assert!(sig.parameters.is_none());

        // Test non-existent function
        assert!(get_builtin_signature("nonexistent").is_none());
    }

    #[test]
    fn test_create_signature_info() {
        use crate::ast::{FnSignature, ParamSpec, Parameter};
        use crate::types::Type;

        // Simple function with no parameters
        let sig = FnSignature {
            is_public: true,
            is_infix: false,
            fn_name: "simple".to_string(),
            parameters: vec![],
            return_type: Type::Int,
        };
        let info = create_signature_info(&sig);
        assert_eq!(info.label, "simple -> int");
        assert!(info.parameters.is_none()); // No parameters, so should be None

        // Function with parameters
        let sig = FnSignature {
            is_public: false,
            is_infix: false,
            fn_name: "add".to_string(),
            parameters: vec![
                Parameter {
                    name: "a".to_string(),
                    typ: Type::Int,
                    spec: ParamSpec::default(),
                },
                Parameter {
                    name: "b".to_string(),
                    typ: Type::Int,
                    spec: ParamSpec::default(),
                },
            ],
            return_type: Type::Int,
        };
        let info = create_signature_info(&sig);
        assert!(info.label.contains("add"));
        assert!(info.label.contains("$a: int"));
        assert!(info.label.contains("$b: int"));
        assert!(info.label.contains("-> int"));
        assert_eq!(info.parameters.unwrap().len(), 2);
    }

    #[test]
    fn test_create_signature_info_parameter_offsets() {
        use crate::ast::{FnSignature, ParamSpec, Parameter};
        use crate::types::Type;

        let sig = FnSignature {
            is_public: false,
            is_infix: false,
            fn_name: "greet".to_string(),
            parameters: vec![Parameter {
                name: "name".to_string(),
                typ: Type::Str,
                spec: ParamSpec::default(),
            }],
            return_type: Type::Any,
        };

        let info = create_signature_info(&sig);
        let params = info.parameters.unwrap();
        assert_eq!(params.len(), 1);

        // Check that parameter offset is correct
        match &params[0].label {
            ParameterLabel::LabelOffsets([start, end]) => {
                let param_text = &info.label[*start as usize..*end as usize];
                assert_eq!(param_text, "$name: str");
            }
            _ => panic!("Expected label offsets"),
        }
    }

    #[test]
    fn test_signature_help_for_user_function() {
        let code = r#"
public main = echo "Hello";
greet $name: str = echo $name;
add $a: int $b: int -> int = $a + $b;
"#;

        let ast = parse_script(code).unwrap();

        // Find the "add" function
        let add_fn = ast
            .fn_definitions
            .iter()
            .find(|f| f.signature.fn_name == "add")
            .unwrap();
        let sig_info = create_signature_info(&add_fn.signature);

        assert!(sig_info.label.contains("add"));
        assert!(sig_info.label.contains("$a: int"));
        assert!(sig_info.label.contains("$b: int"));
        assert!(sig_info.label.contains("-> int"));
        assert_eq!(sig_info.parameters.unwrap().len(), 2);
    }

    #[test]
    fn test_extract_function_name_edge_cases() {
        // Function with underscore
        assert_eq!(
            extract_function_name_at_cursor("my_function "),
            Some("my_function".to_string())
        );

        // After equals sign
        assert_eq!(
            extract_function_name_at_cursor("x = echo "),
            Some("echo".to_string())
        );

        // Multiple spaces
        assert_eq!(
            extract_function_name_at_cursor("    echo    "),
            Some("echo".to_string())
        );

        // With parentheses (shouldn't happen in Shady but let's be safe)
        assert_eq!(
            extract_function_name_at_cursor("result = stdout("),
            Some("stdout".to_string())
        );
    }
}
