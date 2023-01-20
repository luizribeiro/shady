use pest::iterators::Pair;
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest::Parser;
use std::fs;

#[derive(Parser)]
#[grammar = "shady.pest"]
pub struct ShadyParser;

#[derive(Debug)]
pub struct ProgramAST {
    pub fn_definitions: Vec<FnDefinition>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    Str,
    Bool,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Parameter {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FnSignature {
    pub is_public: bool,
    pub is_infix: bool,
    pub fn_name: String,
    pub parameters: Vec<Parameter>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FnDefinition {
    pub signature: FnSignature,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Int(i64),
    String(String),
    Bool(bool),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Value(Value),
    Variable(String),
    Call {
        fn_name: String,
        arguments: Vec<Expr>,
    },
    Block {
        statements: Vec<Expr>,
    },
    If {
        condition: Box<Expr>,
        when_true: Box<Expr>,
        when_false: Box<Expr>,
    },
}

fn parse_type(pair: Pair<Rule>) -> Type {
    match pair.as_rule() {
        Rule::type_int => Type::Int,
        Rule::type_str => Type::Str,
        Rule::type_bool => Type::Bool,
        _ => unreachable!(),
    }
}

fn parse_block(pair: Pair<Rule>) -> Expr {
    let mut statements = Vec::new();
    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::expr => statements.push(parse_expr(pair)),
            _ => unreachable!(),
        }
    }
    Expr::Block { statements }
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
                arguments.push(Expr::Value(Value::String(pair.as_str().to_string())))
            }
            _ => unreachable!("unknown rule type: {:?}", pair.as_rule()),
        }
    }

    Expr::Call {
        fn_name: fn_name.unwrap(),
        arguments,
    }
}

fn is_value(rule: Rule) -> bool {
    match rule {
        Rule::int => true,
        Rule::str => true,
        Rule::bool => true,
        _ => false,
    }
}

fn parse_value(pair: Pair<Rule>) -> Value {
    match pair.as_rule() {
        Rule::int => Value::Int(pair.as_str().parse().unwrap()),
        Rule::str => {
            let mut s = pair.as_str().to_string();
            s.remove(0);
            s.pop();
            Value::String(s)
        }
        Rule::bool => Value::Bool(pair.as_str().parse().unwrap()),
        _ => unreachable!(),
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
            Rule::call => parse_call(primary),
            Rule::expr => parse_expr(primary),
            Rule::block => parse_block(primary),
            Rule::if_expr => parse_if(primary),
            x if is_value(x) => Expr::Value(parse_value(primary)),
            Rule::variable => Expr::Variable(primary.as_str()[1..].to_string()),
            _ => unreachable!("unknown rule type: {:?}", primary.as_rule()),
        })
        .map_prefix(|op, rhs| Expr::Call {
            fn_name: op.as_str().to_string(),
            arguments: vec![rhs],
        })
        .map_infix(|lhs, op, rhs| Expr::Call {
            fn_name: op.as_str().to_string(),
            arguments: vec![lhs, rhs],
        })
        .parse(pair.into_inner())
}

fn parse_fn_definition(pair: Pair<Rule>) -> FnDefinition {
    let mut is_public = false;
    let mut is_infix = false;
    let mut fn_name: Option<String> = None;
    let mut parameters: Vec<Parameter> = vec![];
    let mut expr: Option<Expr> = None;

    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::public => is_public = true,
            Rule::infix => is_infix = true,
            Rule::fn_name => fn_name = Some(pair.as_str().to_string()),
            Rule::parameter => {
                let mut inner = pair.into_inner();
                let var_name = inner.next().unwrap();
                let typ = match inner.next() {
                    // default to string
                    None => Type::Str,
                    Some(typ) => parse_type(typ),
                };
                parameters.push(Parameter {
                    name: var_name.as_str()[1..].to_string(),
                    typ,
                });
            }
            Rule::expr => expr = Some(parse_expr(pair)),
            _ => unreachable!(),
        };
    }

    FnDefinition {
        signature: FnSignature {
            is_public,
            is_infix,
            fn_name: fn_name.unwrap(),
            parameters,
        },
        expr: expr.unwrap(),
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
    let mut pairs = ShadyParser::parse(Rule::program, &text).unwrap();
    let pair = pairs.next().unwrap();
    assert!(pair.as_rule() == Rule::program);
    parse_program(pair)
}

pub fn parse_file(filename: &str) -> ProgramAST {
    let unparsed_file = fs::read_to_string(&filename).unwrap();
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
                },
                expr: Expr::Value(Value::Int(42)),
            },
        );
    }

    #[test]
    fn parse_function_signature() {
        assert_eq!(
            parse_script("public ans = 42;").fn_definitions[0].signature,
            FnSignature {
                is_public: true,
                is_infix: false,
                fn_name: "ans".to_string(),
                parameters: vec![],
            },
        );
        assert_eq!(
            parse_script("ans = 42;").fn_definitions[0].signature,
            FnSignature {
                is_public: false,
                is_infix: false,
                fn_name: "ans".to_string(),
                parameters: vec![],
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
                },],
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
                    },
                    Parameter {
                        name: "b".to_string(),
                        typ: Type::Int,
                    },
                ],
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
        parse_true: ("main = true;", Expr::Value(Value::Bool(true))),
        parse_false: ("main = false;", Expr::Value(Value::Bool(false))),
        parse_str: ("main = \"hello\";", Expr::Value(Value::String("hello".to_string()))),
        parse_add: ("main = 1 + 2;", Expr::Call { fn_name: "+".to_string(), arguments: vec![Expr::Value(Value::Int(1)), Expr::Value(Value::Int(2))] }),
        parse_sub: ("main = 1 - 2;", Expr::Call { fn_name: "-".to_string(), arguments: vec![Expr::Value(Value::Int(1)), Expr::Value(Value::Int(2))] }),
        parse_mul: ("main = 1 * 2;", Expr::Call { fn_name: "*".to_string(), arguments: vec![Expr::Value(Value::Int(1)), Expr::Value(Value::Int(2))] }),
        parse_div: ("main = 1 / 2;", Expr::Call { fn_name: "/".to_string(), arguments: vec![Expr::Value(Value::Int(1)), Expr::Value(Value::Int(2))] }),
        parse_pow: ("main = 1 ^ 2;", Expr::Call { fn_name: "^".to_string(), arguments: vec![Expr::Value(Value::Int(1)), Expr::Value(Value::Int(2))] }),
        parse_eq: ("main = 1 == 1;", Expr::Call { fn_name: "==".to_string(), arguments: vec![Expr::Value(Value::Int(1)), Expr::Value(Value::Int(1))] }),
        parse_neq: ("main = 1 != 2;", Expr::Call { fn_name: "!=".to_string(), arguments: vec![Expr::Value(Value::Int(1)), Expr::Value(Value::Int(2))] }),
        parse_call: ("main = add 1 2;", Expr::Call {
            fn_name: "add".to_string(),
            arguments: vec![Expr::Value(Value::Int(1)), Expr::Value(Value::Int(2))],
        }),
        parse_call_with_unquoted_str_arg: ("main = add hello;", Expr::Call {
            fn_name: "add".to_string(),
            arguments: vec![Expr::Value(Value::String("hello".to_string()))],
        }),
        parse_call_with_directory_as_arg: ("main = ls ./share/lib;", Expr::Call {
            fn_name: "ls".to_string(),
            arguments: vec![Expr::Value(Value::String("./share/lib".to_string()))],
        }),
        parse_call_with_home_dir_as_arg: ("main = ls ~/.config/;", Expr::Call {
            fn_name: "ls".to_string(),
            arguments: vec![Expr::Value(Value::String("~/.config/".to_string()))],
        }),
        parse_call_with_variable: ("main = add $a;", Expr::Call {
            fn_name: "add".to_string(),
            arguments: vec![Expr::Variable("a".to_string())],
        }),
        parse_block: (
            r#"main = { 1 * 2; echo "hi"; };"#,
            Expr::Block {
                statements: vec![
                    Expr::Call {
                        fn_name: "*".to_string(),
                        arguments: vec![
                            Expr::Value(Value::Int(1)),
                            Expr::Value(Value::Int(2)),
                        ],
                    },
                    Expr::Call {
                        fn_name: "echo".to_string(),
                        arguments: vec![
                            Expr::Value(Value::String("hi".to_string())),
                        ],
                    },
                ],
            }
        ),
        parse_if_with_block: (
            r#"
                main = if ($isdog) {
                    echo "dog";
                } else {
                    echo "cat";
                };
            "#,
            Expr::If {
                condition: Box::new(Expr::Variable("isdog".to_string())),
                when_true: Box::new(Expr::Block {
                    statements: vec![Expr::Call {
                        fn_name: "echo".to_string(),
                        arguments: vec![Expr::Value(Value::String("dog".to_string()))],
                    }],
                }),
                when_false: Box::new(Expr::Block {
                    statements: vec![Expr::Call {
                        fn_name: "echo".to_string(),
                        arguments: vec![Expr::Value(Value::String("cat".to_string()))],
                    }],
                }),
            },
        ),
        parse_if_without_block: (
            r#"
                main = if ($isdog) echo "dog";
                    else echo "cat";
            "#,
            Expr::If {
                condition: Box::new(Expr::Variable("isdog".to_string())),
                when_true: Box::new(
                    Expr::Call {
                        fn_name: "echo".to_string(),
                        arguments: vec![Expr::Value(Value::String("dog".to_string()))],
                    },
                ),
                when_false: Box::new(
                    Expr::Call {
                        fn_name: "echo".to_string(),
                        arguments: vec![Expr::Value(Value::String("cat".to_string()))],
                    },
                ),
            },
        ),
        parse_if_without_block_and_without_semicolon: (
            r#"
                main = if ($isdog) echo "dog"
                    else echo "cat";
            "#,
            Expr::If {
                condition: Box::new(Expr::Variable("isdog".to_string())),
                when_true: Box::new(
                    Expr::Call {
                        fn_name: "echo".to_string(),
                        arguments: vec![Expr::Value(Value::String("dog".to_string()))],
                    },
                ),
                when_false: Box::new(
                    Expr::Call {
                        fn_name: "echo".to_string(),
                        arguments: vec![Expr::Value(Value::String("cat".to_string()))],
                    },
                ),
            },
        ),
    }
}
