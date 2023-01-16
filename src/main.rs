extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;
use std::fs;

#[derive(Parser)]
#[grammar = "shady.pest"]
pub struct ShadyParser;

fn main() {
    let unparsed_file = fs::read_to_string("example.shady").unwrap();
    let pairs = ShadyParser::parse(Rule::program, &unparsed_file).unwrap();

    for pair in pairs {
        println!("Rule: {:?}", pair.as_rule());
        println!("Span: {:?}", pair.as_span());
        println!("Text: {}", pair.as_str());
    }
}
