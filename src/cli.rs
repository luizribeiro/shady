use std::collections::HashMap;

use crate::ast;
use crate::eval;
use crate::eval::ShadyContext;

use clap::{command, value_parser, Arg, Command};

fn string_to_static_str(s: String) -> &'static str {
    Box::leak(s.into_boxed_str())
}

fn get_command(context: &ShadyContext) -> clap::Command {
    let mut cmd = command!().bin_name(format!("shady {}", context.filename));

    for fun in &context.program.fn_definitions {
        let signature = &fun.signature;
        if !signature.is_public {
            continue;
        }

        let mut subcmd = Command::new(string_to_static_str(signature.fn_name.clone()));
        for param in &signature.parameters {
            let mut arg = Arg::new(string_to_static_str(param.name.to_string()));
            arg = arg.required(true);
            arg = match param.typ {
                ast::Type::Int => arg.value_parser(value_parser!(i64)),
                ast::Type::Str => arg.value_parser(value_parser!(String)),
                ast::Type::Bool => arg.value_parser(value_parser!(bool)),
            };
            subcmd = subcmd.arg(arg);
        }
        cmd = cmd.subcommand(subcmd);
    }

    return cmd;
}

pub fn run_fn(context: &ShadyContext, script_args: &Vec<String>) {
    let cmd = get_command(&context);
    let matches = cmd.get_matches_from(&*script_args);

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
