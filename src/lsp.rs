use crate::ast::{parse_script_tolerant, ParseResult};
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
    parse_result: Arc<ParseResult>,
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

        // Use tolerant parsing to collect ALL errors, not just the first one
        match parse_script_tolerant(text) {
            Ok((parse_result, errors)) => {
                // Convert all parse errors to LSP diagnostics
                for err in errors {
                    let diagnostic = match err {
                        ShadyError::ParseErrorSimple { message, span } => {
                            let start_pos = ts_point_to_position(
                                parse_result
                                    .root_node()
                                    .descendant_for_byte_range(span.offset(), span.offset())
                                    .map(|n| n.start_position())
                                    .unwrap_or(tree_sitter::Point { row: 0, column: 0 }),
                            );
                            let end_pos = ts_point_to_position(
                                parse_result
                                    .root_node()
                                    .descendant_for_byte_range(
                                        span.offset() + span.len(),
                                        span.offset() + span.len(),
                                    )
                                    .map(|n| n.end_position())
                                    .unwrap_or(tree_sitter::Point { row: 0, column: 0 }),
                            );

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

                // Store the parse result
                let mut docs = self.documents.write().await;
                docs.insert(
                    uri.clone(),
                    DocumentState {
                        parse_result: Arc::new(parse_result),
                    },
                );
            }
            Err(err) => {
                // Tolerant parsing failed completely - report the error
                let diagnostic = match err {
                    ShadyError::ParseErrorSimple { message, span } => Diagnostic {
                        range: Range::new(
                            Position::new(0, span.offset() as u32),
                            Position::new(0, (span.offset() + span.len()) as u32),
                        ),
                        severity: Some(DiagnosticSeverity::ERROR),
                        source: Some("shady".to_string()),
                        message,
                        ..Default::default()
                    },
                    other_err => Diagnostic {
                        range: Range::new(Position::new(0, 0), Position::new(0, 1)),
                        severity: Some(DiagnosticSeverity::ERROR),
                        source: Some("shady".to_string()),
                        message: format!("{:?}", other_err),
                        ..Default::default()
                    },
                };

                diagnostics.push(diagnostic);
            }
        }

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
        let position = params.text_document_position_params.position;

        let docs = self.documents.read().await;
        let doc = match docs.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        let offset = lsp_position_to_offset(&doc.parse_result.source, position);

        // Find function containing cursor
        if let Some(func_sig) = doc.parse_result.function_at_offset(offset) {
            let hover_text = format!("```shady\n{}\n```", func_sig);

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

        let offset = lsp_position_to_offset(&doc.parse_result.source, position);

        // Find the node at cursor position
        let node = match doc.parse_result.node_at_offset(offset) {
            Some(n) => n,
            None => return Ok(None),
        };

        // Check if we're on a function name (fn_call or fn_name)
        let identifier_text = if node.kind() == "fn_name" || node.kind() == "token" {
            node.utf8_text(doc.parse_result.source_bytes()).ok()
        } else {
            None
        };

        if let Some(fn_name) = identifier_text {
            // Find the function definition node
            if let Some(def_location) = find_function_definition(&doc.parse_result, fn_name, uri) {
                return Ok(Some(GotoDefinitionResponse::Scalar(def_location)));
            }
        }

        Ok(None)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let docs = self.documents.read().await;
        let doc = match docs.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        let offset = lsp_position_to_offset(&doc.parse_result.source, position);

        // Check if we're completing a variable (after '$')
        if is_completing_variable(&doc.parse_result.source, position) {
            // Find the function containing the cursor
            if let Some(func_sig) = doc.parse_result.function_at_offset(offset) {
                let mut completions = Vec::new();

                // Add all parameters as completion options
                for param in &func_sig.parameters {
                    completions.push(CompletionItem {
                        label: format!("${}", param.name),
                        kind: Some(CompletionItemKind::VARIABLE),
                        detail: Some(format!("{}", param.typ)),
                        documentation: Some(Documentation::String(format!(
                            "Parameter of function '{}'",
                            func_sig.fn_name
                        ))),
                        // Remove the '$' when inserting so user doesn't get '$$'
                        insert_text: Some(param.name.clone()),
                        ..Default::default()
                    });
                }

                return Ok(Some(CompletionResponse::Array(completions)));
            }
        }

        // Get all available function completions
        let mut completions = Vec::new();

        // Add user-defined functions
        for fn_sig in &doc.parse_result.function_signatures {
            let detail = if fn_sig.parameters.is_empty() {
                format!("{} -> {}", fn_sig.fn_name, fn_sig.return_type)
            } else {
                fn_sig.to_string()
            };

            completions.push(CompletionItem {
                label: fn_sig.fn_name.clone(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(detail),
                documentation: Some(Documentation::String(format!(
                    "User-defined function{}",
                    if fn_sig.is_public { " (public)" } else { "" }
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

        // Get the line up to the cursor to find what function is being called
        let line_text = doc
            .parse_result
            .source
            .lines()
            .nth(position.line as usize)
            .unwrap_or("");
        let line_up_to_cursor =
            &line_text[..position.character.min(line_text.len() as u32) as usize];

        // Try to extract the function name before the cursor
        if let Some(fn_name) = extract_function_name_at_cursor(line_up_to_cursor) {
            // Look for user-defined function
            for fn_sig in &doc.parse_result.function_signatures {
                if fn_sig.fn_name == fn_name {
                    let signature_info = create_signature_info(fn_sig);
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

/// Convert tree-sitter Point to LSP Position
fn ts_point_to_position(point: tree_sitter::Point) -> Position {
    Position::new(point.row as u32, point.column as u32)
}

/// Convert LSP Position to byte offset
fn lsp_position_to_offset(text: &str, position: Position) -> usize {
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

/// Find the definition location for a function
fn find_function_definition(
    parse_result: &ParseResult,
    fn_name: &str,
    uri: &Url,
) -> Option<Location> {
    // Walk the tree to find fn_definition nodes
    let root = parse_result.root_node();
    let mut cursor = root.walk();

    for child in root.named_children(&mut cursor) {
        if child.kind() == "fn_definition" {
            if let Some(name_node) = child.child_by_field_name("name") {
                if let Ok(name) = name_node.utf8_text(parse_result.source_bytes()) {
                    if name == fn_name {
                        // Found it! Return the location
                        let start_pos = ts_point_to_position(name_node.start_position());
                        let end_pos = ts_point_to_position(name_node.end_position());

                        return Some(Location {
                            uri: uri.clone(),
                            range: Range::new(start_pos, end_pos),
                        });
                    }
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
    } else if let Some(paren_pos) = trimmed.rfind('(') {
        paren_pos + 1
    } else {
        0
    };

    let relevant_part = &trimmed[start_pos..].trim_start();

    // Get the first word (the function name)
    let words: Vec<&str> = relevant_part.split_whitespace().collect();

    if let Some(first_word) = words.first() {
        // Remove any leading/trailing non-identifier characters
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
fn create_signature_info(signature: &crate::ast::LspSignature) -> SignatureInformation {
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

    // Add return type if it's not Any
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

/// Check if the user is trying to complete a variable (typing after '$')
fn is_completing_variable(text: &str, position: Position) -> bool {
    let line = match text.lines().nth(position.line as usize) {
        Some(l) => l,
        None => return false,
    };

    // Get the character position, ensuring we don't go out of bounds
    let char_pos = position.character as usize;
    if char_pos > line.len() {
        return false;
    }

    // If at position 0, can't be completing a variable
    if char_pos == 0 {
        return false;
    }

    // Check if the previous character is '$' or if we're in the middle of a variable
    let line_up_to_cursor = &line[..char_pos];

    // Look for the last '$' in the line up to cursor
    if let Some(dollar_pos) = line_up_to_cursor.rfind('$') {
        // Check if there's only valid identifier characters between '$' and cursor
        let after_dollar = &line_up_to_cursor[dollar_pos + 1..];
        after_dollar
            .chars()
            .all(|c| c.is_alphanumeric() || c == '_')
    } else {
        false
    }
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
    use crate::ast::parse_script_tolerant;

    #[test]
    fn test_is_completing_variable_after_dollar() {
        // Cursor right after $
        assert!(is_completing_variable("echo $", Position::new(0, 6)));
    }

    #[test]
    fn test_is_completing_variable_middle_of_name() {
        // Cursor in the middle of $nam|e
        assert!(is_completing_variable("echo $name", Position::new(0, 8)));
    }

    #[test]
    fn test_is_completing_variable_no_dollar() {
        // No $ present
        assert!(!is_completing_variable("echo hello", Position::new(0, 6)));
    }

    #[test]
    fn test_is_completing_variable_after_space() {
        // $ followed by space (still valid)
        assert!(is_completing_variable("echo $ ", Position::new(0, 6)));
    }

    #[test]
    fn test_is_completing_variable_with_underscore() {
        // Variable name with underscore
        assert!(is_completing_variable("echo $my_var", Position::new(0, 10)));
    }

    #[test]
    fn test_is_completing_variable_at_line_start() {
        // Variable at start of line
        assert!(is_completing_variable("$var", Position::new(0, 2)));
    }

    #[test]
    fn test_is_completing_variable_incomplete_code() {
        // Incomplete code with variable - the key use case!
        assert!(is_completing_variable(
            "doit $x: str = seq [\n  echo $",
            Position::new(1, 8)
        ));
    }

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
        use crate::ast::{LspParam, LspSignature};
        use crate::types::Type;

        // Simple function with no parameters
        let sig = LspSignature {
            is_public: true,
            is_infix: false,
            fn_name: "simple".to_string(),
            parameters: vec![],
            return_type: Type::Int,
        };
        let info = create_signature_info(&sig);
        assert_eq!(info.label, "simple -> int");
        assert!(info.parameters.is_none());

        // Function with parameters
        let sig = LspSignature {
            is_public: false,
            is_infix: false,
            fn_name: "add".to_string(),
            parameters: vec![
                LspParam {
                    name: "a".to_string(),
                    typ: Type::Int,
                },
                LspParam {
                    name: "b".to_string(),
                    typ: Type::Int,
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
    fn test_function_at_offset() {
        let code = "greet $name: str = echo $name;";
        let (parse_result, _) = parse_script_tolerant(code).unwrap();

        // Position inside "echo" - should find "greet" function
        let offset = code.find("echo").unwrap();
        let func_sig = parse_result.function_at_offset(offset);

        assert!(func_sig.is_some());
        assert_eq!(func_sig.unwrap().fn_name, "greet");
    }

    #[test]
    fn test_function_at_offset_multiple_functions() {
        let code = "f1 = 1;\nf2 $x: int = $x;\nf3 = 3;";
        let (parse_result, _) = parse_script_tolerant(code).unwrap();

        // Position in f2's body
        let offset = code.find("f2 $x").unwrap() + 10; // Inside the body
        let func_sig = parse_result.function_at_offset(offset);

        assert!(func_sig.is_some());
        assert_eq!(func_sig.unwrap().fn_name, "f2");
    }

    #[test]
    fn test_function_at_offset_incomplete_code() {
        let code = "doit $x: str = seq [\n  echo $x";
        let (parse_result, _errors) = parse_script_tolerant(code).unwrap();

        // Try to find function even in incomplete code
        if !parse_result.function_signatures.is_empty() {
            let offset = code.len() - 2; // Near the end
            let func_sig = parse_result.function_at_offset(offset);
            // May or may not find it depending on Tree-sitter's error recovery
            // The important thing is it doesn't panic
            let _ = func_sig;
        }
    }

    #[test]
    fn test_get_builtin_completions() {
        let completions = get_builtin_completions();

        // Should have all the core builtin functions
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
}

#[cfg(test)]
mod debug_tests {
    use super::*;
    use crate::ast::parse_script_tolerant;

    #[test]
    fn test_completion_incomplete_code_debug() {
        let code = "public main $something: str = seq [\n  (echo $so";
        let (parse_result, errors) = parse_script_tolerant(code).unwrap();

        println!(
            "Functions found: {}",
            parse_result.function_signatures.len()
        );
        for sig in &parse_result.function_signatures {
            println!(
                "  Function: {} with {} parameters",
                sig.fn_name,
                sig.parameters.len()
            );
            for param in &sig.parameters {
                println!("    - ${}: {}", param.name, param.typ);
            }
        }

        println!("Parse errors: {}", errors.len());

        // Check is_completing_variable
        let position = Position::new(1, 11); // After "$so"
        let completing = is_completing_variable(code, position);
        println!(
            "is_completing_variable at position {:?}: {}",
            position, completing
        );
        assert!(completing, "Should detect variable completion");

        // Check function_at_offset
        let offset = code.len() - 1; // Near the end
        println!("Looking for function at offset {}", offset);
        match parse_result.function_at_offset(offset) {
            Some(func) => {
                println!("✅ Found function: {}", func.fn_name);
                assert_eq!(func.fn_name, "main");
                assert_eq!(func.parameters.len(), 1);
                assert_eq!(func.parameters[0].name, "something");
            }
            None => {
                println!("❌ No function found at offset!");
                panic!("Should find 'main' function");
            }
        }
    }
}
