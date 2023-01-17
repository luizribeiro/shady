#![allow(dead_code)]
extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::iterators::Pair;
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest::Parser;
use std::fs;

#[derive(Parser)]
#[grammar = "shady.pest"]
pub struct ShadyParser;

#[derive(Debug)]
struct ProgramAST {
    fn_definitions: Vec<FnDefinition>,
}

#[derive(Debug)]
struct FnSignature {
    is_public: bool,
    is_infix: bool,
    fn_name: String,
    parameters: Vec<String>,
}

#[derive(Debug)]
struct FnDefinition {
    signature: FnSignature,
    expr: Expr,
}

#[derive(Debug)]
enum Value {
    Int(i64),
    Bool(bool),
    String(String),
}

#[derive(Debug)]
enum Expr {
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
        .op(Op::infix(Rule::infix_op, Assoc::Left))
        .op(Op::prefix(Rule::prefix_op));

    pratt
        .map_primary(|primary| match primary.as_rule() {
            Rule::call => parse_call(primary),
            Rule::expr => parse_expr(primary),
            Rule::block => Expr::Block { statements: vec![] },
            Rule::int => Expr::Value(Value::Int(primary.as_str().parse().unwrap())),
            Rule::variable => Expr::Variable(primary.as_str().to_string()),
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
    let mut parameters: Vec<String> = vec![];
    let mut expr: Option<Expr> = None;

    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::public => is_public = true,
            Rule::infix => is_infix = true,
            Rule::fn_name => fn_name = Some(pair.as_str().to_string()),
            Rule::parameter => parameters.push(pair.as_str().to_string()),
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

fn print_usage(context: ShadyContext) {
    println!("Usage: shady {}", context.args.filename);
    for fun in context.program.fn_definitions {
        let signature = &fun.signature;
        if !signature.is_public {
            continue;
        }
        print!("{}", signature.fn_name);
        for param in &signature.parameters {
            print!(" <{}>", param[1..].to_uppercase());
        }
        println!();
    }
}

use clap::Parser as ClapParser;

#[derive(ClapParser, Debug)]
#[command(author, version, about, long_about = None)]
struct ShadyArgs {
    #[arg(short, long)]
    ast: bool,

    filename: String,
}

struct ShadyContext {
    args: ShadyArgs,
    program: ProgramAST,
}

fn main() {
    let args = ShadyArgs::parse();

    let unparsed_file = fs::read_to_string(&args.filename).unwrap();
    let mut pairs = ShadyParser::parse(Rule::program, &unparsed_file).unwrap();
    let pair = pairs.next().unwrap();
    assert!(pair.as_rule() == Rule::program);
    let program = parse_program(pair);
    let context = ShadyContext { args, program };

    if context.args.ast {
        println!("{:#?}", context.program);
        return;
    }

    print_usage(context);
}
