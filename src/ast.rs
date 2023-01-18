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

#[derive(Debug)]
pub enum Type {
    Int,
    Str,
}

#[derive(Debug)]
pub struct Parameter {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug)]
pub struct FnSignature {
    pub is_public: bool,
    pub is_infix: bool,
    pub fn_name: String,
    pub parameters: Vec<Parameter>,
}

#[derive(Debug)]
pub struct FnDefinition {
    pub signature: FnSignature,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    String(String),
}

#[derive(Debug)]
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
}

fn parse_type(pair: Pair<Rule>) -> Type {
    match pair.as_rule() {
        Rule::type_int => Type::Int,
        Rule::type_str => Type::Str,
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

fn parse_call(pair: Pair<Rule>) -> Expr {
    let mut fn_name: Option<String> = None;
    let mut arguments: Vec<Expr> = Vec::new();

    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::fn_name => fn_name = Some(pair.as_str().to_string()),
            Rule::expr => arguments.push(parse_expr(pair)),
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

fn parse_expr(pair: Pair<Rule>) -> Expr {
    let pratt = PrattParser::new()
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
            Rule::int => Expr::Value(Value::Int(primary.as_str().parse().unwrap())),
            Rule::str => {
                let raw_str = primary.as_str();
                let unquoted_str = &raw_str[1..raw_str.len() - 1];
                Expr::Value(Value::String(unquoted_str.to_string()))
            }
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

pub fn parse_script(filename: &str) -> ProgramAST {
    let unparsed_file = fs::read_to_string(&filename).unwrap();
    let mut pairs = ShadyParser::parse(Rule::program, &unparsed_file).unwrap();
    let pair = pairs.next().unwrap();
    assert!(pair.as_rule() == Rule::program);
    parse_program(pair)
}

pub fn get_fn_by_name<'a>(program: &'a ProgramAST, fn_name: &str) -> Option<&'a FnDefinition> {
    program
        .fn_definitions
        .iter()
        .find(|fn_def| fn_def.signature.fn_name == fn_name)
}
