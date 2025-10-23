use crate::error::{Result, ShadyError};
use crate::parser;
use crate::types::{Type, Value};
use miette::SourceSpan;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use tree_sitter::{Node, Parser as TSParser};

/// Represents a span of source code for error reporting
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub offset: usize,
    pub length: usize,
}

impl Span {
    #[cfg(test)]
    pub fn new(offset: usize, length: usize) -> Self {
        Self { offset, length }
    }

    /// Create a Span from a tree-sitter Node
    pub fn from_node(node: &Node) -> Self {
        Self {
            offset: node.start_byte(),
            length: node.end_byte() - node.start_byte(),
        }
    }

    /// Convert to miette's SourceSpan for error reporting
    #[allow(dead_code)]
    pub fn to_source_span(&self) -> SourceSpan {
        SourceSpan::new(self.offset.into(), self.length)
    }
}

#[derive(Debug)]
pub struct ProgramAST {
    pub fn_definitions: Vec<FnDefinition>,
}

#[derive(Debug, Eq)]
pub struct Parameter {
    pub name: String,
    pub typ: Type,
    pub spec: ParamSpec,
}

impl PartialEq for Parameter {
    fn eq(&self, other: &Self) -> bool {
        self.typ == other.typ
    }
}

#[derive(Debug, Eq)]
pub struct FnSignature {
    pub is_public: bool,
    pub is_infix: bool,
    pub fn_name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
}

impl Hash for FnSignature {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.fn_name.hash(state);
        self.is_infix.hash(state);
    }
}

impl PartialEq for FnSignature {
    fn eq(&self, other: &Self) -> bool {
        self.fn_name == other.fn_name
            && self.is_infix == other.is_infix
            && self.parameters == other.parameters
    }
}

impl Display for FnSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut result = String::new();
        if self.is_public {
            result.push_str("public ");
        }
        if self.is_infix {
            result.push_str("infix ");
        }
        result.push_str(&self.fn_name);
        result.push(' ');
        for (i, param) in self.parameters.iter().enumerate() {
            result.push('$');
            result.push_str(if param.name.is_empty() {
                "_"
            } else {
                &param.name
            });
            result.push_str(": ");
            result.push_str(&param.typ.to_string());
            if i < self.parameters.len() - 1 {
                result.push_str(", ");
            }
        }
        write!(f, "{}", result)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct FnDefinition {
    pub signature: FnSignature,
    pub expr: Expr,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Value(Value, Span),
    Variable(String, Span),
    Call {
        fn_name: String,
        arguments: Vec<Expr>,
        is_infix: bool,
        span: Span,
    },
    If {
        condition: Box<Expr>,
        when_true: Box<Expr>,
        when_false: Box<Expr>,
        span: Span,
    },
    List {
        elements: Vec<Expr>,
        span: Span,
    },
}

impl Expr {
    /// Get the source span for this expression
    #[allow(dead_code)]
    pub fn span(&self) -> &Span {
        match self {
            Expr::Value(_, span) => span,
            Expr::Variable(_, span) => span,
            Expr::Call { span, .. } => span,
            Expr::If { span, .. } => span,
            Expr::List { span, .. } => span,
        }
    }
}

/// Helper to get node text from source
fn node_text<'a>(node: &Node, source: &'a [u8]) -> &'a str {
    node.utf8_text(source).unwrap()
}

/// Helper to get a child node by field name
fn child_by_field<'a>(node: &'a Node, field_name: &str) -> Option<Node<'a>> {
    node.child_by_field_name(field_name)
}

/// Helper to get named children of a node
fn named_children<'a>(node: &'a Node<'a>) -> Vec<Node<'a>> {
    let mut cursor = node.walk();
    node.named_children(&mut cursor).collect()
}

fn parse_type(node: Node, _source: &[u8]) -> Type {
    match node.kind() {
        "type_int" => Type::Int,
        "type_str" => Type::Str,
        "type_bool" => Type::Bool,
        "type_list" => {
            let element_type = child_by_field(&node, "element_type")
                .expect("type_list must have element_type field");
            Type::List(Box::new(parse_type(element_type, _source)))
        }
        "typ" => {
            // typ is a wrapper node, get the actual type child
            let type_child = node.named_child(0).expect("typ node must have a child");
            parse_type(type_child, _source)
        }
        _ => unreachable!("Unknown type: {}", node.kind()),
    }
}

fn parse_if(node: Node, source: &[u8]) -> Expr {
    let span = Span::from_node(&node);
    let condition = child_by_field(&node, "condition").expect("if_expr must have condition");
    let when_true = child_by_field(&node, "when_true").expect("if_expr must have when_true");
    let when_false = child_by_field(&node, "when_false").expect("if_expr must have when_false");

    Expr::If {
        condition: Box::new(parse_expr(condition, source)),
        when_true: Box::new(parse_expr(when_true, source)),
        when_false: Box::new(parse_expr(when_false, source)),
        span,
    }
}

fn parse_call(node: Node, source: &[u8]) -> Expr {
    let span = Span::from_node(&node);
    let fn_name = child_by_field(&node, "name")
        .map(|n| node_text(&n, source).to_string())
        .expect("fn_call must have name field");

    let mut arguments: Vec<Expr> = Vec::new();

    // Get all children with field name "argument"
    let mut cursor = node.walk();
    for child in node.children_by_field_name("argument", &mut cursor) {
        arguments.push(parse_fn_arg(child, source));
    }

    Expr::Call {
        fn_name,
        arguments,
        is_infix: false,
        span,
    }
}

fn parse_fn_arg(node: Node, source: &[u8]) -> Expr {
    match node.kind() {
        "value" | "int" | "str" | "bool" => {
            let span = Span::from_node(&node);
            Expr::Value(parse_value(node, source), span)
        }
        "unquoted_str_arg" => {
            let span = Span::from_node(&node);
            Expr::Value(Value::Str(node_text(&node, source).to_string()), span)
        }
        "variable" => parse_variable(node, source),
        "list" => parse_list(node, source),
        _ => parse_expr(node, source),
    }
}

fn parse_value(node: Node, source: &[u8]) -> Value {
    // For composite value nodes, get the actual value child
    let value_node = if node.kind() == "value" {
        node.named_child(0).expect("value node must have a child")
    } else {
        node
    };

    match value_node.kind() {
        "int" => Value::Int(
            node_text(&value_node, source)
                .parse()
                .expect("int parse error"),
        ),
        "str" => {
            let text = node_text(&value_node, source);
            Value::Str(snailquote::unescape(text).expect("str parse error"))
        }
        "bool" => Value::Bool(
            node_text(&value_node, source)
                .parse()
                .expect("bool parse error"),
        ),
        _ => unreachable!("unknown value type: {}", value_node.kind()),
    }
}

fn parse_variable(node: Node, source: &[u8]) -> Expr {
    let span = Span::from_node(&node);
    let text = node_text(&node, source);
    // Variable text includes the $, so skip it
    let var_name = text[1..].to_string();
    Expr::Variable(var_name, span)
}

fn parse_list(node: Node, source: &[u8]) -> Expr {
    let span = Span::from_node(&node);
    let elements: Vec<Expr> = named_children(&node)
        .into_iter()
        .filter(|child| child.kind() != ";" && child.kind() != "[" && child.kind() != "]")
        .map(|child| parse_expr(child, source))
        .collect();
    Expr::List { elements, span }
}

fn parse_expr(node: Node, source: &[u8]) -> Expr {
    match node.kind() {
        // Primary expressions
        "int" | "str" | "bool" => {
            let span = Span::from_node(&node);
            Expr::Value(parse_value(node, source), span)
        }
        "value" => {
            let span = Span::from_node(&node);
            Expr::Value(parse_value(node, source), span)
        }
        "variable" => parse_variable(node, source),
        "fn_call" => parse_call(node, source),
        "list" => parse_list(node, source),
        "if_expr" => parse_if(node, source),

        // Binary expressions
        "binary_expr" => {
            let span = Span::from_node(&node);
            let left = child_by_field(&node, "left").expect("binary_expr must have left");
            let right = child_by_field(&node, "right").expect("binary_expr must have right");
            let operator =
                child_by_field(&node, "operator").expect("binary_expr must have operator");

            let fn_name = if operator.kind() == "custom_infix_op" {
                // Custom infix operator like `foo` - extract the token between backticks
                let op_text = node_text(&operator, source);
                op_text[1..op_text.len() - 1].to_string()
            } else {
                node_text(&operator, source).to_string()
            };

            Expr::Call {
                fn_name,
                arguments: vec![parse_expr(left, source), parse_expr(right, source)],
                is_infix: true,
                span,
            }
        }

        // Prefix expressions
        "prefix_expr" => {
            let span = Span::from_node(&node);
            let operator =
                child_by_field(&node, "operator").expect("prefix_expr must have operator");
            let operand = child_by_field(&node, "operand").expect("prefix_expr must have operand");

            Expr::Call {
                fn_name: node_text(&operator, source).to_string(),
                arguments: vec![parse_expr(operand, source)],
                is_infix: false,
                span,
            }
        }

        // Handle unquoted string arguments (which can appear as expressions in some contexts)
        "unquoted_str_arg" => {
            let span = Span::from_node(&node);
            Expr::Value(Value::Str(node_text(&node, source).to_string()), span)
        }

        // Fallthrough for other expression types
        _ => {
            // Try to get the first named child for wrapper nodes
            if let Some(child) = node.named_child(0) {
                parse_expr(child, source)
            } else {
                unreachable!("unknown expression type: {}", node.kind())
            }
        }
    }
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct ParamSpec {
    pub is_option: bool,
    pub short: Option<char>,
    pub default_value: Option<Value>,
}

fn parse_param_spec(node: Node, source: &[u8]) -> ParamSpec {
    let mut is_option = false;
    let mut short = None;

    let default_value_node =
        child_by_field(&node, "default_value").expect("param_spec must have default_value");
    let default_value = Some(parse_value(default_value_node, source));

    // Parse param_option children
    for child in named_children(&node) {
        if child.kind() == "param_option" {
            let option_name = child_by_field(&child, "option_name")
                .map(|n| node_text(&n, source))
                .expect("param_option must have option_name");

            match option_name {
                "option" => is_option = true,
                "short" => {
                    if let Some(value_node) = child_by_field(&child, "option_value") {
                        let value_text = node_text(&value_node, source);
                        // Value is a quoted char like 'x', so extract the char at position 1
                        short = Some(value_text.chars().nth(1).unwrap());
                    }
                }
                _ => {}
            }
        }
    }

    ParamSpec {
        is_option,
        short,
        default_value,
    }
}

fn parse_fn_definition(node: Node, source: &[u8]) -> Result<FnDefinition> {
    let is_public = child_by_field(&node, "public").is_some();
    let is_infix = child_by_field(&node, "infix").is_some();

    let fn_name = child_by_field(&node, "name")
        .map(|n| node_text(&n, source).to_string())
        .expect("fn_definition must have name field");

    let return_type = child_by_field(&node, "return_type")
        .map(|n| parse_type(n, source))
        .unwrap_or(Type::Any);

    let body = child_by_field(&node, "body").expect("fn_definition must have body");
    let expr = parse_expr(body, source);

    // Parse parameters - they're individual field nodes, not a single container
    let mut parameters: Vec<Parameter> = Vec::new();
    let mut cursor = node.walk();
    for param_node in node.children_by_field_name("parameters", &mut cursor) {
        parameters.push(parse_parameter(param_node, source)?);
    }

    // Validate parameter ordering: required parameters cannot come after optional ones
    let mut found_optional = false;
    for param in &parameters {
        if param.spec.default_value.is_some() {
            found_optional = true;
        } else if found_optional {
            return Err(ShadyError::RequiredAfterOptional(param.name.clone()));
        }
    }

    // Validate default value types match parameter types
    for param in &parameters {
        if let Some(default_value) = &param.spec.default_value {
            let default_type = default_value.get_type();
            if default_type != param.typ {
                return Err(ShadyError::DefaultTypeMismatch {
                    param: param.name.clone(),
                    expected: param.typ.clone(),
                    actual: default_type,
                });
            }
        }
    }

    Ok(FnDefinition {
        signature: FnSignature {
            is_public,
            is_infix,
            fn_name,
            parameters,
            return_type,
        },
        expr,
    })
}

fn parse_parameter(node: Node, source: &[u8]) -> Result<Parameter> {
    let var_node = child_by_field(&node, "name").expect("parameter must have name");
    let var_text = node_text(&var_node, source);
    // Variable includes $, so skip it
    let name = var_text[1..].to_string();

    let typ = child_by_field(&node, "type")
        .map(|n| parse_type(n, source))
        .unwrap_or(Type::Str); // default to string

    let spec = child_by_field(&node, "spec")
        .map(|n| parse_param_spec(n, source))
        .unwrap_or_default();

    Ok(Parameter { name, typ, spec })
}

fn parse_program(node: Node, source: &[u8]) -> Result<ProgramAST> {
    let mut fn_definitions: Vec<FnDefinition> = vec![];

    for child in named_children(&node) {
        if child.kind() == "fn_definition" {
            fn_definitions.push(parse_fn_definition(child, source)?);
        }
    }

    Ok(ProgramAST { fn_definitions })
}

/// Recursively find the first ERROR or MISSING node in the parse tree
fn find_error_node<'a>(node: Node<'a>) -> Option<Node<'a>> {
    if node.kind() == "ERROR" || node.is_missing() {
        return Some(node);
    }

    // Recursively check children using cursor
    let mut cursor = node.walk();
    if cursor.goto_first_child() {
        loop {
            let child_node = cursor.node();
            if let Some(error_node) = find_error_node(child_node) {
                return Some(error_node);
            }
            if !cursor.goto_next_sibling() {
                break;
            }
        }
    }

    None
}

/// Recursively find all ERROR or MISSING nodes in the parse tree
fn find_all_error_nodes<'a>(node: Node<'a>) -> Vec<Node<'a>> {
    let mut errors = Vec::new();

    if node.kind() == "ERROR" || node.is_missing() {
        errors.push(node);
    }

    // Recursively check children using cursor
    let mut cursor = node.walk();
    if cursor.goto_first_child() {
        loop {
            errors.extend(find_all_error_nodes(cursor.node()));
            if !cursor.goto_next_sibling() {
                break;
            }
        }
    }

    errors
}

/// Parse a script with error tolerance for LSP features
/// Returns the AST (potentially partial) and a list of parse errors
pub fn parse_script_tolerant(text: &str) -> Result<(ProgramAST, Vec<ShadyError>)> {
    let mut parser = TSParser::new();
    parser
        .set_language(parser::language())
        .map_err(|e| ShadyError::ParseErrorSimple {
            message: format!("Failed to set tree-sitter language: {}", e),
            span: SourceSpan::from(0..0),
        })?;

    let tree = parser
        .parse(text, None)
        .ok_or_else(|| ShadyError::ParseErrorSimple {
            message: "Failed to parse script".to_string(),
            span: SourceSpan::from(0..0),
        })?;

    let root_node = tree.root_node();
    let mut errors = Vec::new();

    // Collect all parse errors but don't fail
    if root_node.has_error() {
        let error_nodes = find_all_error_nodes(root_node);
        for error_node in error_nodes {
            errors.push(ShadyError::ParseErrorSimple {
                message: "Syntax error in script".to_string(),
                span: SourceSpan::from(error_node.start_byte()..error_node.end_byte()),
            });
        }
    }

    if root_node.kind() != "program" {
        return Err(ShadyError::ParseErrorSimple {
            message: format!("expected program node, got {}", root_node.kind()),
            span: SourceSpan::from(0..0),
        });
    }

    // Parse as much as we can from the (potentially partial) tree
    let ast = parse_program(root_node, text.as_bytes())?;

    Ok((ast, errors))
}

pub fn parse_script(text: &str) -> Result<ProgramAST> {
    let mut parser = TSParser::new();
    parser
        .set_language(parser::language())
        .map_err(|e| ShadyError::ParseErrorSimple {
            message: format!("Failed to set tree-sitter language: {}", e),
            span: SourceSpan::from(0..0),
        })?;

    let tree = parser
        .parse(text, None)
        .ok_or_else(|| ShadyError::ParseErrorSimple {
            message: "Failed to parse script".to_string(),
            span: SourceSpan::from(0..0),
        })?;

    let root_node = tree.root_node();

    // Check for parse errors
    if root_node.has_error() {
        // Find the first ERROR node to get a more specific error location
        let error_node = find_error_node(root_node);
        let span = if let Some(err_node) = error_node {
            SourceSpan::from(err_node.start_byte()..err_node.end_byte())
        } else {
            SourceSpan::from(root_node.start_byte()..root_node.end_byte())
        };

        return Err(ShadyError::ParseErrorSimple {
            message: "Syntax error in script".to_string(),
            span,
        });
    }

    if root_node.kind() != "program" {
        return Err(ShadyError::ParseErrorSimple {
            message: format!("expected program node, got {}", root_node.kind()),
            span: SourceSpan::from(0..0),
        });
    }

    parse_program(root_node, text.as_bytes())
}

pub fn get_fn_by_name<'a>(program: &'a ProgramAST, fn_name: &str) -> Option<&'a FnDefinition> {
    program
        .fn_definitions
        .iter()
        .find(|fn_def| fn_def.signature.fn_name == fn_name)
}

#[cfg(test)]
mod tests {
    use super::*;

    // Helper function to create a dummy span for tests
    fn dummy_span() -> Span {
        Span::new(0, 0)
    }

    // Test helper functions for creating Exprs with dummy spans
    impl Expr {
        fn test_value(value: Value) -> Self {
            Expr::Value(value, dummy_span())
        }

        // Helper to compare Exprs ignoring spans
        fn eq_ignore_spans(&self, other: &Self) -> bool {
            match (self, other) {
                (Expr::Value(v1, _), Expr::Value(v2, _)) => v1 == v2,
                (Expr::Variable(n1, _), Expr::Variable(n2, _)) => n1 == n2,
                (
                    Expr::Call {
                        fn_name: n1,
                        arguments: a1,
                        is_infix: i1,
                        ..
                    },
                    Expr::Call {
                        fn_name: n2,
                        arguments: a2,
                        is_infix: i2,
                        ..
                    },
                ) => {
                    n1 == n2
                        && i1 == i2
                        && a1.len() == a2.len()
                        && a1
                            .iter()
                            .zip(a2.iter())
                            .all(|(e1, e2)| e1.eq_ignore_spans(e2))
                }
                (
                    Expr::If {
                        condition: c1,
                        when_true: t1,
                        when_false: f1,
                        ..
                    },
                    Expr::If {
                        condition: c2,
                        when_true: t2,
                        when_false: f2,
                        ..
                    },
                ) => c1.eq_ignore_spans(c2) && t1.eq_ignore_spans(t2) && f1.eq_ignore_spans(f2),
                (Expr::List { elements: e1, .. }, Expr::List { elements: e2, .. }) => {
                    e1.len() == e2.len()
                        && e1.iter().zip(e2.iter()).all(|(a, b)| a.eq_ignore_spans(b))
                }
                _ => false,
            }
        }
    }

    #[test]
    fn parse_function_definition() {
        let program = parse_script("ans = 42;").unwrap();
        let expected = FnDefinition {
            signature: FnSignature {
                is_public: false,
                is_infix: false,
                fn_name: "ans".to_string(),
                parameters: vec![],
                return_type: Type::Any,
            },
            expr: Expr::test_value(Value::Int(42)),
        };
        // Compare everything except spans
        assert_eq!(program.fn_definitions[0].signature, expected.signature);
        assert!(program.fn_definitions[0]
            .expr
            .eq_ignore_spans(&expected.expr));
    }

    #[test]
    fn parse_function_signature() {
        assert_eq!(
            parse_script("public ans -> int = 42;")
                .unwrap()
                .fn_definitions[0]
                .signature,
            FnSignature {
                is_public: true,
                is_infix: false,
                fn_name: "ans".to_string(),
                parameters: vec![],
                return_type: Type::Int,
            },
        );
        assert_eq!(
            parse_script("ans = 42;").unwrap().fn_definitions[0].signature,
            FnSignature {
                is_public: false,
                is_infix: false,
                fn_name: "ans".to_string(),
                parameters: vec![],
                return_type: Type::Any,
            },
        );
        assert_eq!(
            parse_script("print $msg = echo $msg;")
                .unwrap()
                .fn_definitions[0]
                .signature,
            FnSignature {
                is_public: false,
                is_infix: false,
                fn_name: "print".to_string(),
                parameters: vec![Parameter {
                    name: "msg".to_string(),
                    typ: Type::Str,
                    spec: ParamSpec::default(),
                }],
                return_type: Type::Any,
            },
        );
        assert_eq!(
            parse_script("add $a: int $b: int = $a + $b;")
                .unwrap()
                .fn_definitions[0]
                .signature,
            FnSignature {
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
                return_type: Type::Any,
            },
        );
        assert_eq!(
            parse_script("add $a: int $b: int (42) = $a + $b;")
                .unwrap()
                .fn_definitions[0]
                .signature,
            FnSignature {
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
                        spec: ParamSpec {
                            default_value: Some(Value::Int(42)),
                            ..ParamSpec::default()
                        },
                    },
                ],
                return_type: Type::Any,
            },
        );
        assert_eq!(
            parse_script("get $a: int (42, option) = $a;")
                .unwrap()
                .fn_definitions[0]
                .signature,
            FnSignature {
                is_public: false,
                is_infix: false,
                fn_name: "get".to_string(),
                parameters: vec![Parameter {
                    name: "a".to_string(),
                    typ: Type::Int,
                    spec: ParamSpec {
                        default_value: Some(Value::Int(42)),
                        is_option: true,
                        ..ParamSpec::default()
                    },
                },],
                return_type: Type::Any,
            },
        );
        assert_eq!(
            parse_script("add $a: [int] $b: [[int]] -> [int] = [1; 2];")
                .unwrap()
                .fn_definitions[0]
                .signature,
            FnSignature {
                is_public: false,
                is_infix: false,
                fn_name: "add".to_string(),
                parameters: vec![
                    Parameter {
                        name: "a".to_string(),
                        typ: Type::List(Box::new(Type::Int)),
                        spec: ParamSpec::default(),
                    },
                    Parameter {
                        name: "b".to_string(),
                        typ: Type::List(Box::new(Type::List(Box::new(Type::Int)))),
                        spec: ParamSpec::default(),
                    },
                ],
                return_type: Type::List(Box::new(Type::Int)),
            },
        );
    }

    macro_rules! parse_expr_tests {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    let program = parse_script(input).unwrap();
                    let expr = &program.fn_definitions[0].expr;
                    assert!(expr.eq_ignore_spans(&expected),
                            "Parsed expression doesn't match expected.\nParsed: {:#?}\nExpected: {:#?}",
                            expr, expected);
                }
            )*
        }
    }

    parse_expr_tests! {
        parse_int: ("main = 1;", Expr::Value(Value::Int(1), dummy_span())),
        parse_list: ("main = [1; 2];", Expr::List {
            elements: vec![
                Expr::Value(Value::Int(1), dummy_span()),
                Expr::Value(Value::Int(2), dummy_span()),
            ],
            span: dummy_span(),
        }),
        parse_list_of_expressions: ("main = [(1+2); (3*4)];", Expr::List {
            elements: vec![
                Expr::Call {
                    fn_name: "+".to_string(),
                    arguments: vec![
                        Expr::Value(Value::Int(1), dummy_span()),
                        Expr::Value(Value::Int(2), dummy_span()),
                    ],
                    is_infix: true,
                    span: dummy_span(),
                },
                Expr::Call {
                    fn_name: "*".to_string(),
                    arguments: vec![
                        Expr::Value(Value::Int(3), dummy_span()),
                        Expr::Value(Value::Int(4), dummy_span()),
                    ],
                    is_infix: true,
                    span: dummy_span(),
                },
            ],
            span: dummy_span(),
        }),
        parse_true: ("main = true;", Expr::Value(Value::Bool(true), dummy_span())),
        parse_false: ("main = false;", Expr::Value(Value::Bool(false), dummy_span())),
        parse_str: ("main = \"hello\";", Expr::Value(Value::Str("hello".to_string()), dummy_span())),
        parse_str_with_escape_characters: (
            "main = \"hello\\\"\";",
            Expr::Value(Value::Str("hello\"".to_string()), dummy_span()),
        ),
        parse_multiline_str_with_escape_characters: (
            r#"main = "hello
\"
";"#,
            Expr::Value(Value::Str("hello\n\"\n".to_string()), dummy_span()),
        ),
        parse_add: ("main = 1 + 2;", Expr::Call { fn_name: "+".to_string(), is_infix: true, arguments: vec![Expr::Value(Value::Int(1), dummy_span()), Expr::Value(Value::Int(2), dummy_span())], span: dummy_span() }),
        parse_sub: ("main = 1 - 2;", Expr::Call { fn_name: "-".to_string(), is_infix: true, arguments: vec![Expr::Value(Value::Int(1), dummy_span()), Expr::Value(Value::Int(2), dummy_span())], span: dummy_span() }),
        parse_mul: ("main = 1 * 2;", Expr::Call { fn_name: "*".to_string(), is_infix: true, arguments: vec![Expr::Value(Value::Int(1), dummy_span()), Expr::Value(Value::Int(2), dummy_span())], span: dummy_span() }),
        parse_div: ("main = 1 / 2;", Expr::Call { fn_name: "/".to_string(), is_infix: true, arguments: vec![Expr::Value(Value::Int(1), dummy_span()), Expr::Value(Value::Int(2), dummy_span())], span: dummy_span() }),
        parse_pow: ("main = 1 ^ 2;", Expr::Call { fn_name: "^".to_string(), is_infix: true, arguments: vec![Expr::Value(Value::Int(1), dummy_span()), Expr::Value(Value::Int(2), dummy_span())], span: dummy_span() }),
        parse_eq: ("main = 1 == 1;", Expr::Call { fn_name: "==".to_string(), is_infix: true, arguments: vec![Expr::Value(Value::Int(1), dummy_span()), Expr::Value(Value::Int(1), dummy_span())], span: dummy_span() }),
        parse_neq: ("main = 1 != 2;", Expr::Call { fn_name: "!=".to_string(), is_infix: true, arguments: vec![Expr::Value(Value::Int(1), dummy_span()), Expr::Value(Value::Int(2), dummy_span())], span: dummy_span() }),
        parse_call: ("main = add 1 2;", Expr::Call {
            fn_name: "add".to_string(),
            arguments: vec![Expr::Value(Value::Int(1), dummy_span()), Expr::Value(Value::Int(2), dummy_span())],
            is_infix: false,
            span: dummy_span(),
        }),
        parse_call_with_unquoted_str_arg: ("main = add hello;", Expr::Call {
            fn_name: "add".to_string(),
            arguments: vec![Expr::Value(Value::Str("hello".to_string()), dummy_span())],
            is_infix: false,
            span: dummy_span(),
        }),
        parse_call_with_directory_as_arg: ("main = ls ./share/lib;", Expr::Call {
            fn_name: "ls".to_string(),
            arguments: vec![Expr::Value(Value::Str("./share/lib".to_string()), dummy_span())],
            is_infix: false,
            span: dummy_span(),
        }),
        parse_call_with_home_dir_as_arg: ("main = ls ~/.config/;", Expr::Call {
            fn_name: "ls".to_string(),
            arguments: vec![Expr::Value(Value::Str("~/.config/".to_string()), dummy_span())],
            is_infix: false,
            span: dummy_span(),
        }),
        parse_call_with_variable: ("main = add $a;", Expr::Call {
            fn_name: "add".to_string(),
            arguments: vec![Expr::Variable("a".to_string(), dummy_span())],
            is_infix: false,
            span: dummy_span(),
        }),
        parse_if_with_seq: (
            r#"
                main = if ($isdog) seq [
                    echo "dog";
                ] else seq [
                    echo "cat";
                ];
            "#,
            Expr::If {
                condition: Box::new(Expr::Variable("isdog".to_string(), dummy_span())),
                when_true: Box::new(Expr::Call {
                    fn_name: "seq".to_string(),
                    is_infix: false,
                    arguments: vec![Expr::List {
                        elements: vec![
                            Expr::Call {
                                fn_name: "echo".to_string(),
                                arguments: vec![Expr::Value(Value::Str("dog".to_string()), dummy_span())],
                                is_infix: false,
                                span: dummy_span(),
                            },
                        ],
                        span: dummy_span(),
                    }],
                    span: dummy_span(),
                }),
                when_false: Box::new(Expr::Call {
                        fn_name: "seq".to_string(),
                        is_infix: false,
                        arguments: vec![Expr::List {
                            elements: vec![
                                Expr::Call {
                                    fn_name: "echo".to_string(),
                                    arguments: vec![Expr::Value(Value::Str("cat".to_string()), dummy_span())],
                                    is_infix: false,
                                    span: dummy_span(),
                                },
                            ],
                            span: dummy_span(),
                        }],
                        span: dummy_span(),
                    }
                ),
                span: dummy_span(),
            },
        ),
        parse_if_without_seq: (
            r#"
                main = if ($isdog) echo "dog";
                    else echo "cat";
            "#,
            Expr::If {
                condition: Box::new(Expr::Variable("isdog".to_string(), dummy_span())),
                when_true: Box::new(
                    Expr::Call {
                        fn_name: "echo".to_string(),
                        arguments: vec![Expr::Value(Value::Str("dog".to_string()), dummy_span())],
                        is_infix: false,
                        span: dummy_span(),
                    },
                ),
                when_false: Box::new(
                    Expr::Call {
                        fn_name: "echo".to_string(),
                        arguments: vec![Expr::Value(Value::Str("cat".to_string()), dummy_span())],
                        is_infix: false,
                        span: dummy_span(),
                    },
                ),
                span: dummy_span(),
            },
        ),
        parse_if_without_seq_and_without_semicolon: (
            r#"
                main = if ($isdog) echo "dog"
                    else echo "cat";
            "#,
            Expr::If {
                condition: Box::new(Expr::Variable("isdog".to_string(), dummy_span())),
                when_true: Box::new(
                    Expr::Call {
                        fn_name: "echo".to_string(),
                        arguments: vec![Expr::Value(Value::Str("dog".to_string()), dummy_span())],
                        is_infix: false,
                        span: dummy_span(),
                    },
                ),
                when_false: Box::new(
                    Expr::Call {
                        fn_name: "echo".to_string(),
                        arguments: vec![Expr::Value(Value::Str("cat".to_string()), dummy_span())],
                        is_infix: false,
                        span: dummy_span(),
                    },
                ),
                span: dummy_span(),
            },
        ),
        parse_string_concat_with_echo: (
            r#"public main = exec (echo ("a" + (stdout (echo -n "b")) + "c"));"#,
            Expr::Call {
                fn_name: "exec".to_string(),
                is_infix: false,
                arguments: vec![Expr::Call {
                    fn_name: "echo".to_string(),
                    is_infix: false,
                    arguments: vec![Expr::Call {
                        fn_name: "+".to_string(),
                        is_infix: true,
                        arguments: vec![
                            Expr::Call {
                                fn_name: "+".to_string(),
                                is_infix: true,
                                arguments: vec![
                                    Expr::Value(Value::Str("a".to_string()), dummy_span()),
                                    Expr::Call {
                                        fn_name: "stdout".to_string(),
                                        is_infix: false,
                                        arguments: vec![Expr::Call {
                                            fn_name: "echo".to_string(),
                                            is_infix: false,
                                            arguments: vec![
                                                Expr::Value(Value::Str("-n".to_string()), dummy_span()),
                                                Expr::Value(Value::Str("b".to_string()), dummy_span()),
                                            ],
                                            span: dummy_span(),
                                        }],
                                        span: dummy_span(),
                                    },
                                ],
                                span: dummy_span(),
                            },
                            Expr::Value(Value::Str("c".to_string()), dummy_span()),
                        ],
                        span: dummy_span(),
                    }],
                    span: dummy_span(),
                }],
                span: dummy_span(),
            },
        ),
    }

    // Parameter validation tests
    #[test]
    fn test_required_param_after_optional_fails() {
        let result = parse_script("bad $a: int (42) $b: int = $a + $b;");
        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::RequiredAfterOptional(param_name) => {
                assert_eq!(param_name, "b");
            }
            e => panic!("Expected RequiredAfterOptional error, got {:?}", e),
        }
    }

    #[test]
    fn test_default_type_mismatch_fails() {
        let result = parse_script("bad $a: int (\"hello\") = $a;");
        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::DefaultTypeMismatch {
                param,
                expected,
                actual,
            } => {
                assert_eq!(param, "a");
                assert_eq!(expected, Type::Int);
                assert_eq!(actual, Type::Str);
            }
            e => panic!("Expected DefaultTypeMismatch error, got {:?}", e),
        }
    }

    #[test]
    fn test_bool_default_on_int_param_fails() {
        let result = parse_script("bad $a: int (true) = $a;");
        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::DefaultTypeMismatch {
                param,
                expected,
                actual,
            } => {
                assert_eq!(param, "a");
                assert_eq!(expected, Type::Int);
                assert_eq!(actual, Type::Bool);
            }
            e => panic!("Expected DefaultTypeMismatch error, got {:?}", e),
        }
    }

    #[test]
    fn test_valid_parameter_ordering_succeeds() {
        // All required parameters first
        let result = parse_script("good $a: int $b: int = $a + $b;");
        assert!(result.is_ok());

        // All optional parameters
        let result = parse_script("good $a: int (1) $b: int (2) = $a + $b;");
        assert!(result.is_ok());

        // Required then optional
        let result = parse_script("good $a: int $b: int (42) = $a + $b;");
        assert!(result.is_ok());
    }

    #[test]
    fn test_multiple_required_after_optional_fails() {
        let result = parse_script("bad $a: int (1) $b: int $c: int = $a + $b + $c;");
        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::RequiredAfterOptional(param_name) => {
                assert_eq!(param_name, "b");
            }
            e => panic!("Expected RequiredAfterOptional error, got {:?}", e),
        }
    }

    #[test]
    fn test_correct_default_value_types_succeed() {
        // Int default on int param
        let result = parse_script("good $a: int (42) = $a;");
        assert!(result.is_ok());

        // String default on str param
        let result = parse_script("good $a: str (\"hello\") = $a;");
        assert!(result.is_ok());

        // Bool default on bool param
        let result = parse_script("good $a: bool (true) = $a;");
        assert!(result.is_ok());
    }

    #[test]
    fn test_complex_invalid_ordering() {
        // Three params: required, optional, required - should fail on third param
        let result = parse_script("bad $x: int $y: int (10) $z: str = $x + $y;");
        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::RequiredAfterOptional(param_name) => {
                assert_eq!(param_name, "z");
            }
            e => panic!("Expected RequiredAfterOptional error, got {:?}", e),
        }
    }

    // Comment parsing tests
    #[test]
    fn test_multiline_comment_before_function() {
        // This was the bug in examples/example.shady
        let code = r#"/*
 * This is a multiline comment
 */
public main = 42;"#;
        let result = parse_script(code);
        assert!(
            result.is_ok(),
            "Multiline comment before function should parse successfully, got error: {:?}",
            result.err()
        );
        let program = result.unwrap();
        assert_eq!(program.fn_definitions.len(), 1);
        assert_eq!(program.fn_definitions[0].signature.fn_name, "main");
    }

    #[test]
    fn test_single_line_comment_before_function() {
        let code = "# This is a comment\npublic main = 42;";
        let result = parse_script(code);
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.fn_definitions.len(), 1);
        assert_eq!(program.fn_definitions[0].signature.fn_name, "main");
    }

    #[test]
    fn test_comments_between_functions() {
        let code = r#"
foo = 1;
# Comment between functions
bar = 2;
/* Another comment */
baz = 3;
"#;
        let result = parse_script(code);
        assert!(
            result.is_ok(),
            "Comments between functions should parse successfully, got error: {:?}",
            result.err()
        );
        let program = result.unwrap();
        assert_eq!(program.fn_definitions.len(), 3);
        assert_eq!(program.fn_definitions[0].signature.fn_name, "foo");
        assert_eq!(program.fn_definitions[1].signature.fn_name, "bar");
        assert_eq!(program.fn_definitions[2].signature.fn_name, "baz");
    }

    #[test]
    fn test_comment_at_end_of_file() {
        let code = "main = 42;\n# Trailing comment";
        let result = parse_script(code);
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.fn_definitions.len(), 1);
        assert_eq!(program.fn_definitions[0].signature.fn_name, "main");
    }

    #[test]
    fn test_multiple_comments_before_function() {
        let code = r#"
# Comment 1
# Comment 2
/* Multiline
   comment */
# Comment 3
public main = 42;
"#;
        let result = parse_script(code);
        assert!(
            result.is_ok(),
            "Multiple comments before function should parse successfully, got error: {:?}",
            result.err()
        );
        let program = result.unwrap();
        assert_eq!(program.fn_definitions.len(), 1);
        assert_eq!(program.fn_definitions[0].signature.fn_name, "main");
    }

    // Tests for tolerant parsing
    #[test]
    fn test_parse_tolerant_with_valid_code() {
        let code = "main = 42;";
        let result = parse_script_tolerant(code);
        assert!(result.is_ok());
        let (ast, errors) = result.unwrap();
        assert_eq!(ast.fn_definitions.len(), 1);
        assert_eq!(errors.len(), 0); // No errors for valid code
    }

    #[test]
    fn test_parse_tolerant_missing_semicolon() {
        let code = "main = 42";
        let result = parse_script_tolerant(code);
        assert!(result.is_ok());
        let (_ast, errors) = result.unwrap();
        assert!(!errors.is_empty()); // Should have errors
    }

    #[test]
    fn test_parse_tolerant_incomplete_list() {
        let code = "doit $x: str = seq [\n  echo $x";
        let result = parse_script_tolerant(code);
        assert!(result.is_ok());
        let (_ast, errors) = result.unwrap();
        // Should have errors for incomplete code (missing ] and ;)
        assert!(!errors.is_empty());
        // Note: with severely incomplete code, Tree-sitter may not be able to
        // construct valid function definitions, which is expected behavior
    }

    #[test]
    fn test_parse_tolerant_multiple_errors() {
        let code = "f1 = 1\nf2 = 2\nf3 = 3";
        let result = parse_script_tolerant(code);
        assert!(result.is_ok());
        let (_ast, errors) = result.unwrap();
        // Should collect errors (missing semicolons)
        assert!(errors.len() >= 1);
        // Note: Tree-sitter may or may not recover enough to create AST nodes
    }

    #[test]
    fn test_parse_tolerant_nested_incomplete() {
        let code = "doit $x: str = if ($x) seq [\n  echo";
        let result = parse_script_tolerant(code);
        assert!(result.is_ok());
        let (_ast, errors) = result.unwrap();
        // Should have errors for incomplete code
        assert!(!errors.is_empty());
    }

    #[test]
    fn test_parse_tolerant_partial_recovery() {
        // This test uses code that Tree-sitter CAN recover from
        let code = "main = 42;\nincomplete = echo";
        let result = parse_script_tolerant(code);
        assert!(result.is_ok());
        let (ast, errors) = result.unwrap();
        // Should successfully parse the first complete function
        assert!(ast.fn_definitions.len() >= 1);
        assert_eq!(ast.fn_definitions[0].signature.fn_name, "main");
        // Should have errors for the second incomplete function
        assert!(!errors.is_empty());
    }
}
