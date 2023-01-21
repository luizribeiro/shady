extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;
mod cli;
mod eval;

use clap::Parser;
use eval::ShadyContext;
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

    let cmd = cli::get_command(&context);

    let mut script_args = vec![context.filename.clone()];
    script_args.extend(args.args.clone());
    let matches = cmd.get_matches_from(&script_args);

    let mut local_context = eval::LocalContext {
        vars: HashMap::new(),
    };
    let subcmd_name = matches.subcommand_name().unwrap_or("main");
    let fun = ast::get_fn_by_name(&context.program, subcmd_name).unwrap();
    let args = matches.subcommand_matches(&subcmd_name);
    if let Some(args) = args {
        for param in &fun.signature.parameters {
            args.get_raw(&param.name)
                .unwrap()
                .for_each(|raw_cli_value| {
                    let cli_value: String = raw_cli_value.to_string_lossy().into_owned();
                    let value = match param.typ {
                        ast::Type::Str => ast::Value::String(cli_value),
                        ast::Type::Int => ast::Value::Int(cli_value.parse().unwrap()),
                        ast::Type::Bool => ast::Value::Bool(cli_value.parse().unwrap()),
                    };
                    local_context.vars.insert(param.name.clone(), value);
                });
        }
    }

    eval::eval_expr(&local_context, &context, &fun.expr);
}
