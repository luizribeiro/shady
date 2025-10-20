extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;
mod builtins;
mod cli;
mod error;
mod eval;
mod types;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct ShadyArgs {
    #[arg(short, long)]
    ast: bool,

    filename: String,

    #[arg(allow_hyphen_values = true)]
    args: Vec<String>,
}

fn main() {
    let args = ShadyArgs::parse();

    let program = if args.filename == "-" {
        use std::io::Read;
        let mut buffer = String::new();
        std::io::stdin().read_to_string(&mut buffer).unwrap();
        ast::parse_script(&buffer)
    } else {
        ast::parse_file(&args.filename)
    };
    if args.ast {
        println!("{program:#?}");
        return;
    }

    let context = eval::build_context(args.filename.clone(), program);

    let mut script_args = vec![args.filename.clone()];
    script_args.extend(args.args);

    if let Err(e) = cli::run_fn(&context, &script_args) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}
