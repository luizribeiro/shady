use crate::ShadyContext;

use clap::{command, Arg, Command};

fn string_to_static_str(s: String) -> &'static str {
    Box::leak(s.into_boxed_str())
}

pub fn get_command(context: &ShadyContext) -> clap::Command {
    let mut cmd = command!();

    for fun in &context.program.fn_definitions {
        let signature = &fun.signature;
        if !signature.is_public {
            continue;
        }

        let mut subcmd = Command::new(string_to_static_str(signature.fn_name.clone()));
        for param in &signature.parameters {
            subcmd =
                subcmd.arg(Arg::new(string_to_static_str(param[1..].to_string())).required(true));
        }
        cmd = cmd.subcommand(subcmd);
    }

    return cmd;
}
