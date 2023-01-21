use crate::ast::Type;
use crate::eval::ShadyContext;

use clap::{command, value_parser, Arg, Command};

fn string_to_static_str(s: String) -> &'static str {
    Box::leak(s.into_boxed_str())
}

pub fn get_command(context: &ShadyContext) -> clap::Command {
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
                Type::Int => arg.value_parser(value_parser!(i64)),
                Type::Str => arg.value_parser(value_parser!(String)),
                Type::Bool => arg.value_parser(value_parser!(bool)),
            };
            subcmd = subcmd.arg(arg);
        }
        cmd = cmd.subcommand(subcmd);
    }

    return cmd;
}
