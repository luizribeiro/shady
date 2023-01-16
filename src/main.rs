extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::iterators::Pair;
use pest::Parser;
use std::fs;

#[derive(Parser)]
#[grammar = "shady.pest"]
pub struct ShadyParser;

fn main() {
    let unparsed_file = fs::read_to_string("example.shady").unwrap();
    let pairs = ShadyParser::parse(Rule::program, &unparsed_file).unwrap();

    fn parse_value(pair: Pair<Rule>) {
        println!("-----------");
        println!("Rule: {:?}", pair.as_rule());
        println!("Span: {:?}", pair.as_span());
        println!("Text: {}", pair.as_str());
        match pair.as_rule() {
            _ => {
                for pair in pair.into_inner() {
                    parse_value(pair);
                }
            }
        }
    }

    for pair in pairs {
        parse_value(pair);
    }
}
