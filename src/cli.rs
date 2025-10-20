use std::collections::HashMap;

use crate::ast;
use crate::error::Result;
use crate::eval;
use crate::eval::ShadyContext;
use crate::types::{from_string, Type};

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
            arg = match param.typ {
                Type::Int => arg.value_parser(value_parser!(i64)),
                Type::Str => arg.value_parser(value_parser!(String)),
                Type::Bool => arg.value_parser(value_parser!(bool)),
                Type::List(_) => todo!(),
                // TODO: make it impossible to have public methods with certain types as a
                // parameter
                Type::Proc => unreachable!(),
                Type::Any => unreachable!(),
            };
            if let Some(default) = &param.spec.default_value {
                arg = arg.default_value(string_to_static_str(default.to_string()));
            } else {
                arg = arg.required(true);
            }
            if param.spec.is_option {
                arg = arg.long(string_to_static_str(param.name.to_string()));
                if let Some(short) = param.spec.short {
                    arg = arg.short(short);
                }
            }
            subcmd = subcmd.arg(arg);
        }
        cmd = cmd.subcommand(subcmd);
    }
    cmd
}

pub fn run_fn(context: &ShadyContext, script_args: &Vec<String>) -> Result<()> {
    let cmd = get_command(context);
    let matches = cmd.get_matches_from(script_args);

    let mut local_context = eval::LocalContext {
        vars: HashMap::new(),
    };
    let subcmd_name = matches.subcommand_name().unwrap_or("main");
    let fun = ast::get_fn_by_name(&context.program, subcmd_name)
        .unwrap_or_else(|| panic!("function {subcmd_name} not found"));
    if let Some(args) = matches.subcommand_matches(subcmd_name) {
        for param in &fun.signature.parameters {
            args.get_raw(&param.name)
                .unwrap_or_else(|| panic!("missing argument {}", param.name))
                .for_each(|raw_cli_value| {
                    let cli_value: String = raw_cli_value.to_string_lossy().into_owned();
                    let value = from_string(&param.typ, &cli_value)
                        .expect("failed to parse CLI argument");
                    local_context.vars.insert(param.name.clone(), value);
                });
        }
    }

    eval::eval_expr(&local_context, context, &fun.expr)?;
    Ok(())
}
