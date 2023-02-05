use crate::types::{Type, Value};
use pest::iterators::Pair;
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest::Parser;
use std::fmt::{Display, Formatter};
use std::fs;
use std::hash::{Hash, Hasher};

#[derive(Parser)]
#[grammar = "shady.pest"]
pub struct ShadyParser;

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
    Value(Value),
    Variable(String),
    Call {
        fn_name: String,
        arguments: Vec<Expr>,
        is_infix: bool,
    },
    If {
        condition: Box<Expr>,
        when_true: Box<Expr>,
        when_false: Box<Expr>,
    },
    List {
        elements: Vec<Expr>,
    },
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
    let pairs: Vec<Pair<Rule>> = pair.into_inner().collect();
    match &pairs[..] {
        [a, b, c] => Expr::If {
            condition: Box::new(parse_expr(a.clone())),
            when_true: Box::new(parse_expr(b.clone())),
            when_false: Box::new(parse_expr(c.clone())),
        },
        _ => unreachable!(),
    }
}

fn parse_call(pair: Pair<Rule>) -> Expr {
    let mut fn_name: Option<String> = None;
    let mut arguments: Vec<Expr> = Vec::new();

    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::fn_name => fn_name = Some(pair.as_str().to_string()),
            Rule::expr => arguments.push(parse_expr(pair)),
            x if is_value(x) => arguments.push(Expr::Value(parse_value(pair))),
            Rule::unquoted_str_arg => {
                arguments.push(Expr::Value(Value::Str(pair.as_str().to_string())))
            }
            _ => unreachable!("unknown rule type: {:?}", pair.as_rule()),
        }
    }

    Expr::Call {
        fn_name: fn_name.expect("Rule::fn_name not found"),
        arguments,
        is_infix: false,
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
        Rule::str => {
            let mut s = pair.as_str().to_string();
            s.remove(0);
            s.pop();
            Value::Str(s)
        }
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
        .map_primary(|primary| match primary.as_rule() {
            Rule::fn_call => parse_call(primary),
            Rule::expr => parse_expr(primary),
            Rule::if_expr => parse_if(primary),
            x if is_value(x) => Expr::Value(parse_value(primary)),
            Rule::variable => Expr::Variable(primary.as_str()[1..].to_string()),
            Rule::list => {
                let elements: Vec<Expr> = primary
                    .into_inner()
                    .map(|pair| match pair.as_rule() {
                        Rule::expr => parse_expr(pair),
                        _ => unreachable!(),
                    })
                    .collect();
                Expr::List { elements }
            }
            _ => unreachable!("unknown rule type: {:?}", primary.as_rule()),
        })
        .map_prefix(|op, rhs| Expr::Call {
            fn_name: op.as_str().to_string(),
            arguments: vec![rhs],
            is_infix: false,
        })
        .map_infix(|lhs, op, rhs| Expr::Call {
            fn_name: op.as_str().to_string(),
            arguments: vec![lhs, rhs],
            is_infix: true,
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

fn parse_fn_definition(pair: Pair<Rule>) -> FnDefinition {
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
                    // TODO: error out when adding a required parameter after an optional one
                    // TODO: error out when default value's type doesn't match parameter's type
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

    FnDefinition {
        signature: FnSignature {
            is_public,
            is_infix,
            fn_name: fn_name.expect("Rule::fn_name not found while parsing function"),
            parameters,
            return_type,
        },
        expr: expr.expect("Rule::expr not found while parsing function"),
    }
}

fn parse_program(pair: Pair<Rule>) -> ProgramAST {
    let mut fn_definitions: Vec<FnDefinition> = vec![];
    let pairs = pair.into_inner();

    for pair in pairs {
        match pair.as_rule() {
            Rule::fn_definition => fn_definitions.push(parse_fn_definition(pair)),
            Rule::EOI => (),
            _ => unreachable!(),
        }
    }

    ProgramAST { fn_definitions }
}

pub fn parse_script(text: &str) -> ProgramAST {
    let mut pairs = ShadyParser::parse(Rule::program, text).expect("parse error");
    let pair = pairs.next().expect("no pairs returned by parser");
    assert!(pair.as_rule() == Rule::program);
    parse_program(pair)
}

pub fn parse_file(filename: &str) -> ProgramAST {
    let unparsed_file =
        fs::read_to_string(filename).unwrap_or_else(|_| panic!("file {filename} not found"));
    parse_script(&unparsed_file)
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

    #[test]
    fn parse_function_definition() {
        let program = parse_script("ans = 42;");
        assert_eq!(
            program.fn_definitions[0],
            FnDefinition {
                signature: FnSignature {
                    is_public: false,
                    is_infix: false,
                    fn_name: "ans".to_string(),
                    parameters: vec![],
                    return_type: Type::Any,
                },
                expr: Expr::Value(Value::Int(42)),
            },
        );
    }

    #[test]
    fn parse_function_signature() {
        assert_eq!(
            parse_script("public ans -> int = 42;").fn_definitions[0].signature,
            FnSignature {
                is_public: true,
                is_infix: false,
                fn_name: "ans".to_string(),
                parameters: vec![],
                return_type: Type::Int,
            },
        );
        assert_eq!(
            parse_script("ans = 42;").fn_definitions[0].signature,
            FnSignature {
                is_public: false,
                is_infix: false,
                fn_name: "ans".to_string(),
                parameters: vec![],
                return_type: Type::Any,
            },
        );
        assert_eq!(
            parse_script("print $msg = echo $msg;").fn_definitions[0].signature,
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
            parse_script("add $a: int $b: int = $a + $b;").fn_definitions[0].signature,
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
            parse_script("add $a: int $b: int (42) = $a + $b;").fn_definitions[0].signature,
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
            parse_script("get $a: int (42, option) = $a;").fn_definitions[0].signature,
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
            parse_script("add $a: [int] $b: [[int]] -> [int] = [1; 2];").fn_definitions[0]
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
                    let program = parse_script(input);
                    let expr = &program.fn_definitions[0].expr;
                    assert_eq!(expr, &expected);
                }
            )*
        }
    }

    parse_expr_tests! {
        parse_int: ("main = 1;", Expr::Value(Value::Int(1))),
        parse_list: ("main = [1; 2];", Expr::List {
            elements: vec![
                Expr::Value(Value::Int(1)),
                Expr::Value(Value::Int(2)),
            ],
        }),
        parse_list_of_expressions: ("main = [(1+2); (3*4)];", Expr::List {
            elements: vec![
                Expr::Call {
                    fn_name: "+".to_string(),
                    arguments: vec![
                        Expr::Value(Value::Int(1)),
                        Expr::Value(Value::Int(2)),
                    ],
                    is_infix: true,
                },
                Expr::Call {
                    fn_name: "*".to_string(),
                    arguments: vec![
                        Expr::Value(Value::Int(3)),
                        Expr::Value(Value::Int(4)),
                    ],
                    is_infix: true,
                },
            ],
        }),
        parse_true: ("main = true;", Expr::Value(Value::Bool(true))),
        parse_false: ("main = false;", Expr::Value(Value::Bool(false))),
        parse_str: ("main = \"hello\";", Expr::Value(Value::Str("hello".to_string()))),
        parse_add: ("main = 1 + 2;", Expr::Call { fn_name: "+".to_string(), is_infix: true, arguments: vec![Expr::Value(Value::Int(1)), Expr::Value(Value::Int(2))] }),
        parse_sub: ("main = 1 - 2;", Expr::Call { fn_name: "-".to_string(), is_infix: true, arguments: vec![Expr::Value(Value::Int(1)), Expr::Value(Value::Int(2))] }),
        parse_mul: ("main = 1 * 2;", Expr::Call { fn_name: "*".to_string(), is_infix: true, arguments: vec![Expr::Value(Value::Int(1)), Expr::Value(Value::Int(2))] }),
        parse_div: ("main = 1 / 2;", Expr::Call { fn_name: "/".to_string(), is_infix: true, arguments: vec![Expr::Value(Value::Int(1)), Expr::Value(Value::Int(2))] }),
        parse_pow: ("main = 1 ^ 2;", Expr::Call { fn_name: "^".to_string(), is_infix: true, arguments: vec![Expr::Value(Value::Int(1)), Expr::Value(Value::Int(2))] }),
        parse_eq: ("main = 1 == 1;", Expr::Call { fn_name: "==".to_string(), is_infix: true, arguments: vec![Expr::Value(Value::Int(1)), Expr::Value(Value::Int(1))] }),
        parse_neq: ("main = 1 != 2;", Expr::Call { fn_name: "!=".to_string(), is_infix: true, arguments: vec![Expr::Value(Value::Int(1)), Expr::Value(Value::Int(2))] }),
        parse_call: ("main = add 1 2;", Expr::Call {
            fn_name: "add".to_string(),
            arguments: vec![Expr::Value(Value::Int(1)), Expr::Value(Value::Int(2))],
            is_infix: false,
        }),
        parse_call_with_unquoted_str_arg: ("main = add hello;", Expr::Call {
            fn_name: "add".to_string(),
            arguments: vec![Expr::Value(Value::Str("hello".to_string()))],
            is_infix: false,
        }),
        parse_call_with_directory_as_arg: ("main = ls ./share/lib;", Expr::Call {
            fn_name: "ls".to_string(),
            arguments: vec![Expr::Value(Value::Str("./share/lib".to_string()))],
            is_infix: false,
        }),
        parse_call_with_home_dir_as_arg: ("main = ls ~/.config/;", Expr::Call {
            fn_name: "ls".to_string(),
            arguments: vec![Expr::Value(Value::Str("~/.config/".to_string()))],
            is_infix: false,
        }),
        parse_call_with_variable: ("main = add $a;", Expr::Call {
            fn_name: "add".to_string(),
            arguments: vec![Expr::Variable("a".to_string())],
            is_infix: false,
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
                condition: Box::new(Expr::Variable("isdog".to_string())),
                when_true: Box::new(Expr::Call {
                    fn_name: "seq".to_string(),
                    is_infix: false,
                    arguments: vec![Expr::List {
                        elements: vec![
                            Expr::Call {
                                fn_name: "echo".to_string(),
                                arguments: vec![Expr::Value(Value::Str("dog".to_string()))],
                                is_infix: false,
                            },
                        ],
                    }],
                }),
                when_false: Box::new(Expr::Call {
                        fn_name: "seq".to_string(),
                        is_infix: false,
                        arguments: vec![Expr::List {
                            elements: vec![
                                Expr::Call {
                                    fn_name: "echo".to_string(),
                                    arguments: vec![Expr::Value(Value::Str("cat".to_string()))],
                                    is_infix: false,
                                },
                            ],
                        }],
                    }
                ),
            },
        ),
        parse_if_without_seq: (
            r#"
                main = if ($isdog) echo "dog";
                    else echo "cat";
            "#,
            Expr::If {
                condition: Box::new(Expr::Variable("isdog".to_string())),
                when_true: Box::new(
                    Expr::Call {
                        fn_name: "echo".to_string(),
                        arguments: vec![Expr::Value(Value::Str("dog".to_string()))],
                        is_infix: false,
                    },
                ),
                when_false: Box::new(
                    Expr::Call {
                        fn_name: "echo".to_string(),
                        arguments: vec![Expr::Value(Value::Str("cat".to_string()))],
                        is_infix: false,
                    },
                ),
            },
        ),
        parse_if_without_seq_and_without_semicolon: (
            r#"
                main = if ($isdog) echo "dog"
                    else echo "cat";
            "#,
            Expr::If {
                condition: Box::new(Expr::Variable("isdog".to_string())),
                when_true: Box::new(
                    Expr::Call {
                        fn_name: "echo".to_string(),
                        arguments: vec![Expr::Value(Value::Str("dog".to_string()))],
                        is_infix: false,
                    },
                ),
                when_false: Box::new(
                    Expr::Call {
                        fn_name: "echo".to_string(),
                        arguments: vec![Expr::Value(Value::Str("cat".to_string()))],
                        is_infix: false,
                    },
                ),
            },
        ),
    }
}
