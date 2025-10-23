/// Formatter for Shady code
///
/// This module provides auto-formatting capabilities for Shady scripts,
/// ensuring consistent code style across the codebase.
use crate::ast::ParseResult;

/// Configuration for the formatter
#[derive(Debug, Clone)]
pub struct FormatterConfig {
    /// Number of spaces for indentation
    pub indent_size: usize,
    /// Maximum line length before considering wrapping
    pub max_line_length: usize,
}

impl Default for FormatterConfig {
    fn default() -> Self {
        Self {
            indent_size: 2,
            max_line_length: 100,
        }
    }
}

/// Format a Shady script
pub fn format_script(source: &str, parse_result: &ParseResult) -> String {
    format_script_with_config(source, parse_result, &FormatterConfig::default())
}

/// Format a Shady script with custom configuration
pub fn format_script_with_config(
    source: &str,
    parse_result: &ParseResult,
    config: &FormatterConfig,
) -> String {
    let mut formatter = Formatter::new(source, parse_result, config);
    formatter.format()
}

struct Formatter<'a> {
    source_bytes: &'a [u8],
    parse_result: &'a ParseResult,
    config: &'a FormatterConfig,
    output: String,
    current_indent: usize,
}

impl<'a> Formatter<'a> {
    fn new(source: &'a str, parse_result: &'a ParseResult, config: &'a FormatterConfig) -> Self {
        Self {
            source_bytes: source.as_bytes(),
            parse_result,
            config,
            output: String::new(),
            current_indent: 0,
        }
    }

    fn format(&mut self) -> String {
        let root = self.parse_result.root_node();
        let mut cursor = root.walk();
        let mut prev_kind: Option<&str> = None;

        for child in root.named_children(&mut cursor) {
            if child.kind() == "comment" {
                // Add blank line before comment if previous was a function
                if let Some("fn_definition") = prev_kind {
                    self.output.push('\n');
                }
                // Format comment
                self.format_comment(&child);
                self.output.push('\n');
                prev_kind = Some("comment");
            } else if child.kind() == "fn_definition" {
                // Add blank line before function if previous was a function
                if let Some("fn_definition") = prev_kind {
                    self.output.push('\n');
                }
                // Format function definition
                self.format_function_definition(&child);
                self.output.push('\n');
                prev_kind = Some("fn_definition");
            }
        }

        // Ensure file ends with a newline
        if !self.output.ends_with('\n') {
            self.output.push('\n');
        }

        self.output.clone()
    }

    fn indent(&mut self) {
        self.current_indent += self.config.indent_size;
    }

    fn dedent(&mut self) {
        self.current_indent = self.current_indent.saturating_sub(self.config.indent_size);
    }

    fn write_indent(&mut self) {
        self.output.push_str(&" ".repeat(self.current_indent));
    }

    fn current_line_length(&self) -> usize {
        self.output
            .lines()
            .last()
            .map(|line| line.len())
            .unwrap_or(0)
    }

    fn would_exceed_max_length(&self, additional: usize) -> bool {
        self.current_line_length() + additional > self.config.max_line_length
    }

    fn format_comment(&mut self, node: &tree_sitter::Node) {
        if let Ok(text) = node.utf8_text(self.source_bytes) {
            self.output.push_str(text);
        }
    }

    fn format_function_definition(&mut self, node: &tree_sitter::Node) {
        let mut cursor = node.walk();

        // Check for 'public' keyword
        if let Some(_public) = node.child_by_field_name("public") {
            self.output.push_str("public ");
        }

        // Check for 'infix' keyword
        if let Some(_infix) = node.child_by_field_name("infix") {
            self.output.push_str("infix ");
        }

        // Function name
        if let Some(name) = node.child_by_field_name("name") {
            if let Ok(text) = name.utf8_text(self.source_bytes) {
                self.output.push_str(text);
            }
        }

        // Parameters
        for child in node.children_by_field_name("parameters", &mut cursor) {
            self.output.push(' ');
            self.format_parameter(&child);
        }

        // Return type
        if let Some(return_type) = node.child_by_field_name("return_type") {
            self.output.push_str(" -> ");
            self.format_type(&return_type);
        }

        // Body - check if it would exceed line length
        if let Some(body) = node.child_by_field_name("body") {
            // Get the body text to estimate length
            if let Ok(body_text) = body.utf8_text(self.source_bytes) {
                let body_length = body_text.len();
                // Check if " = <body>;" would exceed max line length
                if self.would_exceed_max_length(3 + body_length + 1) {
                    // Wrap to new line with indentation
                    self.output.push_str(" =\n");
                    self.indent();
                    self.write_indent();
                    self.format_expression(&body, false);
                    self.output.push(';');
                    self.dedent();
                    return;
                }
            }

            // Fits on one line
            self.output.push_str(" = ");
            self.format_expression(&body, false);
        }

        self.output.push(';');
    }

    fn format_parameter(&mut self, node: &tree_sitter::Node) {
        // Variable name
        if let Some(name) = node.child_by_field_name("name") {
            if let Ok(text) = name.utf8_text(self.source_bytes) {
                self.output.push_str(text);
            }
        }

        // Type annotation
        if let Some(typ) = node.child_by_field_name("type") {
            self.output.push_str(": ");
            self.format_type(&typ);
        }

        // Parameter spec (default value and options)
        if let Some(spec) = node.child_by_field_name("spec") {
            self.output.push_str(" (");
            if let Ok(text) = spec.utf8_text(self.source_bytes) {
                self.output.push_str(text);
            }
            self.output.push(')');
        }
    }

    fn format_type(&mut self, node: &tree_sitter::Node) {
        if let Ok(text) = node.utf8_text(self.source_bytes) {
            self.output.push_str(text);
        }
    }

    fn format_expression(&mut self, node: &tree_sitter::Node, is_nested: bool) {
        match node.kind() {
            "value" | "int" | "str" | "bool" | "token" | "fn_name" => {
                if let Ok(text) = node.utf8_text(self.source_bytes) {
                    self.output.push_str(text);
                }
            }
            "variable" => {
                if let Ok(text) = node.utf8_text(self.source_bytes) {
                    self.output.push_str(text);
                }
            }
            "list" => {
                self.format_list(node);
            }
            "block_expr" => {
                self.format_block(node);
            }
            "if_expr" => {
                self.format_if_expr(node, is_nested);
            }
            "fn_call" => {
                self.format_fn_call(node);
            }
            "binary_expr" => {
                self.format_binary_expr(node, is_nested);
            }
            "prefix_expr" => {
                self.format_prefix_expr(node);
            }
            "lambda_expr" => {
                self.format_lambda_expr(node);
            }
            "fn_arg" => {
                // fn_arg is a wrapper - unwrap and format the content
                // Walk through all children and format them
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    if !child.is_named() {
                        // Skip unnamed nodes like parentheses
                        continue;
                    }
                    self.format_expression(&child, is_nested);
                }
            }
            "unquoted_str_arg" => {
                if let Ok(text) = node.utf8_text(self.source_bytes) {
                    self.output.push_str(text);
                }
            }
            _ => {
                // For unknown nodes, try to format their children
                if node.named_child_count() > 0 {
                    let mut cursor = node.walk();
                    for child in node.named_children(&mut cursor) {
                        self.format_expression(&child, is_nested);
                    }
                } else if let Ok(text) = node.utf8_text(self.source_bytes) {
                    // Fallback: just output the text as-is
                    self.output.push_str(text);
                }
            }
        }
    }

    fn format_list(&mut self, node: &tree_sitter::Node) {
        self.output.push('[');

        let mut cursor = node.walk();
        let mut first = true;

        for child in node.named_children(&mut cursor) {
            if !first {
                self.output.push_str("; ");
            }
            first = false;
            self.format_expression(&child, true);
        }

        self.output.push(']');
    }

    fn format_block(&mut self, node: &tree_sitter::Node) {
        self.output.push('{');

        let mut cursor = node.walk();
        let children: Vec<_> = node.named_children(&mut cursor).collect();

        if !children.is_empty() {
            self.output.push('\n');
            self.indent();

            for child in &children {
                self.write_indent();
                self.format_expression(child, false);
                self.output.push_str(";\n");
            }

            self.dedent();
            self.write_indent();
        }

        self.output.push('}');
    }

    fn format_if_expr(&mut self, node: &tree_sitter::Node, is_nested: bool) {
        // Try formatting as single line first to check length
        let mut temp_formatter = Formatter::new(
            std::str::from_utf8(self.source_bytes).unwrap(),
            self.parse_result,
            self.config,
        );
        temp_formatter.current_indent = self.current_indent;
        temp_formatter.format_if_expr_single_line(node, is_nested);
        let single_line = temp_formatter.output;

        // Check if single-line version would exceed max length
        let would_fit =
            self.current_line_length() + single_line.len() <= self.config.max_line_length;

        if would_fit {
            // Use single-line format
            self.output.push_str(&single_line);
        } else {
            // Use multi-line format
            self.format_if_expr_multiline(node, is_nested);
        }
    }

    fn format_if_expr_single_line(&mut self, node: &tree_sitter::Node, is_nested: bool) {
        if is_nested {
            self.output.push('(');
        }

        self.output.push_str("if (");

        if let Some(condition) = node.child_by_field_name("condition") {
            self.format_expression(&condition, false);
        }

        self.output.push_str(") ");

        if let Some(when_true) = node.child_by_field_name("when_true") {
            self.format_expression(&when_true, false);
        }

        self.output.push_str(" else ");

        if let Some(when_false) = node.child_by_field_name("when_false") {
            self.format_expression(&when_false, false);
        }

        if is_nested {
            self.output.push(')');
        }
    }

    fn format_if_expr_multiline(&mut self, node: &tree_sitter::Node, is_nested: bool) {
        if is_nested {
            self.output.push('(');
        }

        self.output.push_str("if (");

        if let Some(condition) = node.child_by_field_name("condition") {
            self.format_expression(&condition, false);
        }

        self.output.push_str(")\n");
        self.indent();
        self.write_indent();

        if let Some(when_true) = node.child_by_field_name("when_true") {
            self.format_expression(&when_true, false);
        }

        self.output.push_str("\n");
        self.dedent();
        self.write_indent();
        self.output.push_str("else\n");
        self.indent();
        self.write_indent();

        if let Some(when_false) = node.child_by_field_name("when_false") {
            self.format_expression(&when_false, false);
        }

        self.dedent();

        if is_nested {
            self.output.push(')');
        }
    }

    fn format_fn_call(&mut self, node: &tree_sitter::Node) {
        // Get function name
        if let Some(name_node) = node.child_by_field_name("name") {
            if let Ok(name) = name_node.utf8_text(self.source_bytes) {
                self.output.push_str(name);

                let mut cursor = node.walk();
                let args: Vec<_> = node
                    .children_by_field_name("argument", &mut cursor)
                    .collect();

                // Check if this is 'seq' with a list argument
                if name == "seq" && args.len() == 1 {
                    // Look for list inside the argument and format it specially
                    if self.format_seq_if_list(&args[0]) {
                        return;
                    }
                }

                // Regular function call - format all arguments with proper spacing
                for arg in args {
                    self.output.push(' ');
                    self.format_argument(&arg);
                }
            }
        }
    }

    fn format_seq_if_list(&mut self, node: &tree_sitter::Node) -> bool {
        // Try to find and format a list node for seq
        if node.kind() == "list" {
            self.output.push(' ');
            self.format_seq_list(node);
            return true;
        }
        // Check if it's an fn_arg wrapper
        if node.kind() == "fn_arg" {
            for i in 0..node.child_count() {
                if let Some(child) = node.child(i) {
                    if child.kind() == "list" {
                        self.output.push(' ');
                        self.format_seq_list(&child);
                        return true;
                    }
                }
            }
        }
        false
    }

    fn format_argument(&mut self, node: &tree_sitter::Node) {
        // Handle parenthesized expressions
        if node.kind() == "fn_arg" {
            // Check if this contains a parenthesized expression
            let mut has_parens = false;
            let mut inner_expr = None;

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                match child.kind() {
                    "(" => {
                        has_parens = true;
                    }
                    ")" => {
                        // Already noted by has_parens
                    }
                    _ if child.is_named() => {
                        inner_expr = Some(child);
                    }
                    _ => {}
                }
            }

            if !has_parens {
                // No parens, just format the inner expression
                if let Some(expr) = inner_expr {
                    self.format_expression(&expr, false);
                }
            } else if let Some(expr) = inner_expr {
                // Had parens, format the expression inside them
                // Don't pass is_nested=true since we're already adding explicit parens
                self.output.push('(');
                self.format_expression(&expr, false);
                self.output.push(')');
            }
        } else {
            // Not wrapped in fn_arg, format directly
            self.format_expression(node, false);
        }
    }

    fn format_seq_list(&mut self, node: &tree_sitter::Node) {
        self.output.push('[');

        let mut cursor = node.walk();
        let children: Vec<_> = node.named_children(&mut cursor).collect();

        if !children.is_empty() {
            self.output.push('\n');
            self.indent();

            for child in &children {
                self.write_indent();
                self.format_expression(child, false);
                self.output.push_str(";\n");
            }

            self.dedent();
            self.write_indent();
        }

        self.output.push(']');
    }

    fn format_binary_expr(&mut self, node: &tree_sitter::Node, is_nested: bool) {
        if is_nested {
            self.output.push('(');
        }

        if let Some(left) = node.child_by_field_name("left") {
            // Only add parens if the child is also a binary expr
            let needs_parens = left.kind() == "binary_expr";
            self.format_expression(&left, needs_parens);
        }

        if let Some(operator) = node.child_by_field_name("operator") {
            if let Ok(op_text) = operator.utf8_text(self.source_bytes) {
                self.output.push(' ');
                self.output.push_str(op_text);
                self.output.push(' ');
            }
        }

        if let Some(right) = node.child_by_field_name("right") {
            // Only add parens if the child is also a binary expr
            let needs_parens = right.kind() == "binary_expr";
            self.format_expression(&right, needs_parens);
        }

        if is_nested {
            self.output.push(')');
        }
    }

    fn format_prefix_expr(&mut self, node: &tree_sitter::Node) {
        if let Some(operator) = node.child_by_field_name("operator") {
            if let Ok(op_text) = operator.utf8_text(self.source_bytes) {
                self.output.push_str(op_text);
            }
        }

        if let Some(operand) = node.child_by_field_name("operand") {
            self.format_expression(&operand, true);
        }
    }

    fn format_lambda_expr(&mut self, node: &tree_sitter::Node) {
        self.output.push_str("lambda");

        let mut cursor = node.walk();
        for param in node.children_by_field_name("parameter", &mut cursor) {
            self.output.push(' ');
            self.format_parameter(&param);
        }

        self.output.push_str(" -> ");

        // Return type (optional)
        if let Some(return_type) = node.child_by_field_name("return_type") {
            self.format_type(&return_type);
            self.output.push_str(" = ");
        }

        if let Some(body) = node.child_by_field_name("body") {
            // Don't add extra parens around lambda body
            self.format_expression(&body, false);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::parse_script_tolerant;

    #[test]
    fn test_format_simple_function() {
        let code = "public  main=print \"hello\";";
        let (parse_result, _) = parse_script_tolerant(code).unwrap();
        let formatted = format_script(code, &parse_result);
        assert_eq!(formatted, "public main = print \"hello\";\n");
    }

    #[test]
    fn test_format_function_with_params() {
        let code = "greet $name: str = echo $name;";
        let (parse_result, _) = parse_script_tolerant(code).unwrap();
        let formatted = format_script(code, &parse_result);
        assert_eq!(formatted, "greet $name: str = echo $name;\n");
    }

    #[test]
    fn test_format_seq_block() {
        let code = "public main = seq [echo \"a\"; echo \"b\";];";
        let (parse_result, _) = parse_script_tolerant(code).unwrap();
        let formatted = format_script(code, &parse_result);
        assert!(formatted.contains("seq ["));
        assert!(formatted.contains("  echo \"a\";"));
        assert!(formatted.contains("  echo \"b\";"));
    }

    #[test]
    fn test_format_if_expr() {
        let code = "check $x: int = if ($x > 10) \"big\" else \"small\";";
        let (parse_result, _) = parse_script_tolerant(code).unwrap();
        let formatted = format_script(code, &parse_result);
        assert!(formatted.contains("if ("));
        assert!(formatted.contains(") "));
        assert!(formatted.contains(" else "));
    }

    #[test]
    fn test_format_lambda() {
        let code = "public double-all $nums: [int] = map (lambda $x -> $x * 2) $nums;";
        let (parse_result, _) = parse_script_tolerant(code).unwrap();
        let formatted = format_script(code, &parse_result);
        assert!(formatted.contains("lambda $x ->"));
        assert!(formatted.contains("map"));
    }

    #[test]
    fn test_format_block_multiline() {
        let code = "public main $name: str -> proc =    {echo \"Hello {$name}!\";};";
        let (parse_result, _) = parse_script_tolerant(code).unwrap();
        let formatted = format_script(code, &parse_result);
        // Should have proper multi-line formatting
        assert!(formatted.contains("{\n"));
        assert!(formatted.contains("  echo"));
        assert!(formatted.contains(";\n}"));
    }
}
