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

    #[arg(allow_hyphen_values = true)]
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

    let mut script_args = vec![context.args.filename.clone()];
    script_args.extend(context.args.args.clone());
    let cmd = args::get_command(&context);
    let matches = cmd.get_matches_from(&script_args);
    println!("{:#?}", matches);
}
