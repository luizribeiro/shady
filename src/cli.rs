use std::collections::HashMap;

use crate::ast;
use crate::error::Result;
use crate::eval;
use crate::eval::ShadyContext;
use crate::types::{from_string, Type};

use clap::{command, value_parser, Arg, Command};

// Note: Box::leak is used to convert Strings to 'static str for clap's API.
// This is an acceptable tradeoff because:
// 1. CLI parsing happens only once per program run
// 2. The amount of leaked memory is small (function and parameter names)
// 3. The program terminates shortly after, and the OS reclaims all memory
// 4. Clap 4.1's builder API requires 'static lifetimes for some methods
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
            let mut arg = Arg::new(string_to_static_str(param.name.clone()));
            arg = match param.typ {
                Type::Int => arg.value_parser(value_parser!(i64)),
                Type::Str => arg.value_parser(value_parser!(String)),
                Type::Bool => arg.value_parser(value_parser!(bool)),
                Type::List(_) => arg.value_parser(value_parser!(String)),
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
                arg = arg.long(string_to_static_str(param.name.clone()));
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

    let mut vars = HashMap::new();
    let subcmd_name = matches.subcommand_name().unwrap_or("main");
    let fun = ast::get_fn_by_name(&context.program, subcmd_name)
        .ok_or_else(|| crate::error::ShadyError::FunctionNotFound {
            name: subcmd_name.to_string()
        })?;
    if let Some(args) = matches.subcommand_matches(subcmd_name) {
        for param in &fun.signature.parameters {
            let raw_values = args.get_raw(&param.name)
                .ok_or_else(|| crate::error::ShadyError::MissingCliArgument(param.name.clone()))?;

            for raw_cli_value in raw_values {
                let cli_value: String = raw_cli_value.to_string_lossy().into_owned();
                let value = from_string(&param.typ, &cli_value)?;
                vars.insert(param.name.clone(), value);
            }
        }
    }

    let local_context = eval::LocalContext {
        vars,
        depth: 0,
    };

    eval::eval_expr_with_type(&local_context, context, &fun.expr, None)?;
    Ok(())
}
