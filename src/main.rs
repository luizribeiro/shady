extern crate pest;
#[macro_use]
extern crate pest_derive;

mod args;
mod ast;
mod eval;

use clap::Parser;
use std::collections::HashMap;

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

    let subcmd_name = matches.subcommand_name().unwrap();
    let fun = ast::get_fn_by_name(&context.program, subcmd_name).unwrap();
    let mut local_context = eval::LocalContext {
        vars: HashMap::new(),
    };
    let args = matches.subcommand_matches(&subcmd_name).unwrap();
    for param in &fun.signature.parameters {
        // TODO: handle types other than str
        args.get_raw(&param).unwrap().for_each(|value| {
            local_context.vars.insert(
                param.clone(),
                ast::Value::String(value.to_string_lossy().into_owned()),
            );
        });
    }
    println!("local_context: {:#?}", local_context);
    eval::eval_expr(&context, &fun.expr);
}
