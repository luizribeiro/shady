extern crate pest;
#[macro_use]
extern crate pest_derive;

mod args;
mod ast;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct ShadyArgs {
    #[arg(short, long)]
    ast: bool,

    filename: String,

    #[arg(raw = true)]
    args: Vec<String>,
}

pub struct ShadyContext {
    args: ShadyArgs,
    program: ast::ProgramAST,
}

fn main() {
    let args = ShadyArgs::parse();

    let program = ast::parse_script(&args.filename);
    let context = ShadyContext { args, program };

    if context.args.ast {
        println!("{:#?}", context.program);
        return;
    }

    let mut cmd = args::get_command(&context);
    cmd //.get_matches_from(&context.args.args)
        .print_help()
        .unwrap();
}
