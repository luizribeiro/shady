use crate::types::{Proc, Value};
use shady_macros::builtin;
use std::process::Command;

fn command(proc: Proc) -> Command {
    let mut command = Command::new(proc.program);
    command.args(proc.args);
    command
}

#[builtin]
fn proc(program: String, args: Vec<String>) -> Value {
    Value::Proc(Proc { program, args })
}

#[builtin]
fn exec(proc: Proc) -> i64 {
    command(proc)
        .status()
        .expect("failed to execute process")
        .code()
        .unwrap_or(0) as i64
}

#[builtin]
fn stdout(proc: Proc) -> String {
    String::from_utf8(
        command(proc)
            .output()
            .expect("failed to execute process")
            .stdout,
    )
    .unwrap()
}

#[builtin("lines")]
fn lines_proc(proc: Proc) -> Vec<String> {
    stdout(proc).lines().map(|s| s.to_string()).collect()
}
