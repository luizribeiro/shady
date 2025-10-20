use crate::error::{Result, ShadyError};
use crate::types::{Type, Value};
use miette::SourceSpan;
use pest::iterators::Pair;
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest::Parser;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

#[derive(Parser)]
#[grammar = "shady.pest"]
pub struct ShadyParser;

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

    /// Create a Span from a Pest span
    pub fn from_pest(span: pest::Span) -> Self {
        Self {
            offset: span.start(),
            length: span.end() - span.start(),
        }
    }

    /// Convert to miette's SourceSpan for error reporting
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

fn parse_type(pair: Pair<Rule>) -> Type {
    match pair.as_rule() {
        Rule::type_int => Type::Int,
        Rule::type_str => Type::Str,
        Rule::type_bool => Type::Bool,
        Rule::type_list => Type::List(Box::new(parse_type(pair.into_inner().next().unwrap()))),
        _ => unreachable!(),
    }
}

fn parse_if(pair: Pair<Rule>) -> Expr {
    let span = Span::from_pest(pair.as_span());
    let pairs: Vec<Pair<Rule>> = pair.into_inner().collect();
    match &pairs[..] {
        [a, b, c] => Expr::If {
            condition: Box::new(parse_expr(a.clone())),
            when_true: Box::new(parse_expr(b.clone())),
            when_false: Box::new(parse_expr(c.clone())),
            span,
        },
        _ => unreachable!(),
    }
}

fn parse_call(pair: Pair<Rule>) -> Expr {
    let span = Span::from_pest(pair.as_span());
    let mut fn_name: Option<String> = None;
    let mut arguments: Vec<Expr> = Vec::new();

    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::fn_name => fn_name = Some(inner_pair.as_str().to_string()),
            Rule::expr => arguments.push(parse_expr(inner_pair)),
            x if is_value(x) => {
                let value_span = Span::from_pest(inner_pair.as_span());
                arguments.push(Expr::Value(parse_value(inner_pair), value_span))
            }
            Rule::unquoted_str_arg => {
                let arg_span = Span::from_pest(inner_pair.as_span());
                arguments.push(Expr::Value(
                    Value::Str(inner_pair.as_str().to_string()),
                    arg_span,
                ))
            }
            _ => unreachable!("unknown rule type: {:?}", inner_pair.as_rule()),
        }
    }

    Expr::Call {
        fn_name: fn_name.expect("Rule::fn_name not found"),
        arguments,
        is_infix: false,
        span,
    }
}

fn is_value(rule: Rule) -> bool {
    matches!(rule, Rule::int | Rule::str | Rule::bool)
}

fn is_type(rule: Rule) -> bool {
    matches!(
        rule,
        Rule::type_int | Rule::type_str | Rule::type_bool | Rule::type_list
    )
}

fn parse_value(pair: Pair<Rule>) -> Value {
    match pair.as_rule() {
        Rule::int => Value::Int(pair.as_str().parse().expect("int parse error")),
        Rule::str => Value::Str(snailquote::unescape(pair.as_str()).expect("str parse error")),
        Rule::bool => Value::Bool(pair.as_str().parse().expect("bool parse error")),
        _ => unreachable!("unknown rule type: {:?}", pair.as_rule()),
    }
}

fn parse_expr(pair: Pair<Rule>) -> Expr {
    let pratt = PrattParser::new()
        .op(Op::infix(Rule::or, Assoc::Left))
        .op(Op::infix(Rule::and, Assoc::Left))
        .op(Op::infix(Rule::comparison_infix_op, Assoc::Left))
        .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::sub, Assoc::Left))
        .op(Op::infix(Rule::mul, Assoc::Left) | Op::infix(Rule::div, Assoc::Left))
        .op(Op::infix(Rule::infix_op, Assoc::Left))
        .op(Op::infix(Rule::pow, Assoc::Right))
        .op(Op::prefix(Rule::prefix_op));

    pratt
        .map_primary(|primary| {
            let span = Span::from_pest(primary.as_span());
            match primary.as_rule() {
                Rule::fn_call => parse_call(primary),
                Rule::expr => parse_expr(primary),
                Rule::if_expr => parse_if(primary),
                x if is_value(x) => Expr::Value(parse_value(primary), span),
                Rule::variable => Expr::Variable(primary.as_str()[1..].to_string(), span),
                Rule::list => {
                    let elements: Vec<Expr> = primary
                        .into_inner()
                        .map(|pair| match pair.as_rule() {
                            Rule::expr => parse_expr(pair),
                            _ => unreachable!(),
                        })
                        .collect();
                    Expr::List { elements, span }
                }
                _ => unreachable!("unknown rule type: {:?}", primary.as_rule()),
            }
        })
        .map_prefix(|op, rhs| {
            let span = Span::from_pest(op.as_span());
            Expr::Call {
                fn_name: op.as_str().to_string(),
                arguments: vec![rhs],
                is_infix: false,
                span,
            }
        })
        .map_infix(|lhs, op, rhs| {
            let span = Span::from_pest(op.as_span());
            Expr::Call {
                fn_name: op.as_str().to_string(),
                arguments: vec![lhs, rhs],
                is_infix: true,
                span,
            }
        })
        .parse(pair.into_inner())
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct ParamSpec {
    pub is_option: bool,
    pub short: Option<char>,
    pub default_value: Option<Value>,
}

fn parse_param_spec(pair: Pair<Rule>) -> ParamSpec {
    let mut is_option = false;
    let mut short = None;
    let default_value;

    let inner: Vec<Pair<Rule>> = pair.into_inner().collect();
    match &inner[..] {
        [raw_default_value, params @ ..] => {
            default_value = Some(parse_value(raw_default_value.clone()));
            for param in params {
                let param_inner: Vec<Pair<Rule>> = param.clone().into_inner().collect();
                match &param_inner[..] {
                    [token, value] => match token.as_str() {
                        "short" => short = Some(value.as_str().chars().nth(1).unwrap()),
                        _ => unreachable!(),
                    },
                    [token] => match token.as_str() {
                        "option" => is_option = true,
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
        }
        _ => unreachable!(),
    }

    ParamSpec {
        is_option,
        short,
        default_value,
    }
}

fn parse_fn_definition(pair: Pair<Rule>) -> Result<FnDefinition> {
    let mut is_public = false;
    let mut is_infix = false;
    let mut fn_name: Option<String> = None;
    let mut parameters: Vec<Parameter> = vec![];
    let mut expr: Option<Expr> = None;
    let mut return_type: Type = Type::Any;

    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::public => is_public = true,
            Rule::infix => is_infix = true,
            Rule::fn_name => fn_name = Some(pair.as_str().to_string()),
            Rule::parameter => {
                let inner: Vec<Pair<Rule>> = pair.into_inner().collect();
                let parameter = match &inner[..] {
                    [var_name, typ, spec] => Parameter {
                        name: var_name.as_str()[1..].to_string(),
                        typ: parse_type(typ.clone()),
                        spec: parse_param_spec(spec.clone()),
                    },
                    [var_name, typ] => Parameter {
                        name: var_name.as_str()[1..].to_string(),
                        typ: parse_type(typ.clone()),
                        spec: ParamSpec::default(),
                    },
                    [var_name] => Parameter {
                        name: var_name.as_str()[1..].to_string(),
                        // default to string
                        typ: Type::Str,
                        spec: ParamSpec::default(),
                    },
                    _ => unreachable!(),
                };
                parameters.push(parameter)
            }
            x if is_type(x) => return_type = parse_type(pair),
            Rule::expr => expr = Some(parse_expr(pair)),
            _ => unreachable!(),
        };
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
            fn_name: fn_name.expect("Rule::fn_name not found while parsing function"),
            parameters,
            return_type,
        },
        expr: expr.expect("Rule::expr not found while parsing function"),
    })
}

fn parse_program(pair: Pair<Rule>) -> Result<ProgramAST> {
    let mut fn_definitions: Vec<FnDefinition> = vec![];
    let pairs = pair.into_inner();

    for pair in pairs {
        match pair.as_rule() {
            Rule::fn_definition => fn_definitions.push(parse_fn_definition(pair)?),
            Rule::EOI => (),
            _ => unreachable!(),
        }
    }

    Ok(ProgramAST { fn_definitions })
}

pub fn parse_script(text: &str) -> Result<ProgramAST> {
    let mut pairs = ShadyParser::parse(Rule::program, text).map_err(|e| {
        // Extract span from Pest error
        let span = match e.location {
            pest::error::InputLocation::Pos(pos) => SourceSpan::from(pos..pos + 1),
            pest::error::InputLocation::Span((start, end)) => SourceSpan::from(start..end),
        };

        // Extract the error variant message
        let message = match &e.variant {
            pest::error::ErrorVariant::ParsingError {
                positives,
                negatives,
            } => {
                let mut msg = String::from("unexpected token");
                if !positives.is_empty() {
                    msg.push_str(&format!(", expected: {:?}", positives));
                }
                if !negatives.is_empty() {
                    msg.push_str(&format!(", but found: {:?}", negatives));
                }
                msg
            }
            pest::error::ErrorVariant::CustomError { message } => message.clone(),
        };

        ShadyError::ParseErrorSimple { message, span }
    })?;
    let pair = pairs.next().ok_or_else(|| ShadyError::ParseErrorSimple {
        message: "no pairs returned by parser".to_string(),
        span: SourceSpan::from(0..0),
    })?;

    if pair.as_rule() != Rule::program {
        return Err(ShadyError::ParseErrorSimple {
            message: format!("expected program rule, got {:?}", pair.as_rule()),
            span: SourceSpan::from(0..0),
        });
    }

    parse_program(pair)
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

        fn test_variable(name: &str) -> Self {
            Expr::Variable(name.to_string(), dummy_span())
        }

        fn test_call(fn_name: &str, arguments: Vec<Expr>, is_infix: bool) -> Self {
            Expr::Call {
                fn_name: fn_name.to_string(),
                arguments,
                is_infix,
                span: dummy_span(),
            }
        }

        fn test_if(condition: Expr, when_true: Expr, when_false: Expr) -> Self {
            Expr::If {
                condition: Box::new(condition),
                when_true: Box::new(when_true),
                when_false: Box::new(when_false),
                span: dummy_span(),
            }
        }

        fn test_list(elements: Vec<Expr>) -> Self {
            Expr::List {
                elements,
                span: dummy_span(),
            }
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
}
