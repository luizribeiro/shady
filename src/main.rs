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
struct Program {
    decls: Vec<Decl>,
}

#[derive(Debug)]
struct Decl {
    is_public: bool,
    is_infix: bool,
    fn_name: String,
    parameters: Vec<String>,
}

fn parse_call(pair: Pair<Rule>) {
    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::fn_name => println!("calling {:?}", pair.as_str()),
            Rule::expr => parse_expr(pair),
            _ => println!("    {:?}: {:?}", pair.as_rule(), pair.as_str()),
        }
    }
}

fn parse_expr(pair: Pair<Rule>) {
    let pratt = PrattParser::new()
        .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::sub, Assoc::Left))
        .op(Op::infix(Rule::mul, Assoc::Left) | Op::infix(Rule::div, Assoc::Left))
        .op(Op::infix(Rule::pow, Assoc::Right))
        .op(Op::postfix(Rule::fac))
        .op(Op::prefix(Rule::neg));

    pratt
        .map_primary(|primary| match primary.as_rule() {
            Rule::int => println!("int: {:?}", primary.as_str()),
            Rule::expr => parse_expr(primary),
            Rule::fn_call => parse_call(primary),
            _ => println!("primary: {:?} {:?}", primary.as_rule(), primary.as_str()),
        })
        .map_prefix(|op, _rhs| match op.as_rule() {
            _ => println!("prefix: {:?} {:?}", op.as_rule(), op.as_str()),
        })
        .map_postfix(|_lhs, op| match op.as_rule() {
            _ => println!("postfix: {:?} {:?}", op.as_rule(), op.as_str()),
        })
        .map_infix(|_lhs, op, _rhs| match op.as_rule() {
            _ => println!("infix: {:?} {:?}", op.as_rule(), op.as_str()),
        })
        .parse(pair.into_inner());
}

fn parse_decl(pair: Pair<Rule>) -> Decl {
    let mut is_public = false;
    let mut is_infix = false;
    let mut fn_name: Option<String> = None;
    let mut parameters: Vec<String> = vec![];

    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::public => is_public = true,
            Rule::infix => is_infix = true,
            Rule::fn_name => fn_name = Some(pair.as_str().to_string()),
            Rule::parameter => parameters.push(pair.as_str().to_string()),
            Rule::expr => parse_expr(pair),
            _ => unreachable!(),
        };
    }

    Decl {
        is_public,
        is_infix,
        fn_name: fn_name.unwrap(),
        parameters,
    }
}

fn parse_program(pair: Pair<Rule>) -> Program {
    let mut decls: Vec<Decl> = vec![];
    let pairs = pair.into_inner();

    for pair in pairs {
        match pair.as_rule() {
            Rule::decl => decls.push(parse_decl(pair)),
            Rule::EOI => (),
            _ => unreachable!(),
        }
    }

    Program { decls }
}

fn main() {
    let unparsed_file = fs::read_to_string("example.shady").unwrap();
    let pairs = ShadyParser::parse(Rule::program, &unparsed_file).unwrap();

    for pair in pairs {
        if pair.as_rule() == Rule::program {
            println!("{:?}", parse_program(pair));
        }
    }
}
