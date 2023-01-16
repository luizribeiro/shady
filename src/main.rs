extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::iterators::Pair;
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
            _ => println!("  {:?}: {:?}", pair.as_rule(), pair.as_str()),
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
        if pair.as_rule() == Rule::decl {
            decls.push(parse_decl(pair));
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
