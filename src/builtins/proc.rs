use crate::types::Value;
use shady_macros::builtin;
use std::process::Command;

fn command(proc: Value) -> Command {
    let (program, args) = match proc {
        Value::Proc { program, args } => (program, args),
        _ => panic!("expected proc"),
    };

    let mut command = Command::new(program);
    command.args(args);
    command
}

#[builtin]
fn exec(proc: Value) -> i64 {
    command(proc)
        .status()
        .expect("failed to execute process")
        .code()
        .unwrap_or(0) as i64
}

#[builtin]
fn stdout(proc: Value) -> String {
    String::from_utf8(
        command(proc)
            .output()
            .expect("failed to execute process")
            .stdout,
    )
    .unwrap()
}
