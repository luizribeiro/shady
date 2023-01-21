extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;
mod cli;
mod eval;

use clap::Parser;
use eval::ShadyContext;

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

    let program = ast::parse_file(&args.filename);
    let context = ShadyContext {
        filename: args.filename,
        program,
    };

    if args.ast {
        println!("{:#?}", context.program);
        return;
    }

    let mut script_args = vec![context.filename.clone()];
    script_args.extend(args.args.clone());

    cli::run_fn(&context, &script_args);
}
