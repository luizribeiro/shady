use crate::types::{Proc, Value};
use shady_macros::builtin;
use std::process::{Child, Command, Output};

fn spawn(proc: Proc) -> std::io::Result<Child> {
    let mut command = Command::new(proc.program);
    command.args(proc.args);
    command.spawn()
}

fn spawn_and_wait(proc: Proc) -> Output {
    spawn(proc)
        .expect("Failed to execute process")
        .wait_with_output()
        .expect("Failed to wait on child")
}

#[builtin]
fn proc(program: String, args: Vec<String>) -> Value {
    Value::Proc(Proc { program, args })
}

#[builtin]
fn exec(proc: Proc) -> i64 {
    spawn_and_wait(proc).status.code().unwrap_or(0) as i64
}

#[builtin]
fn stdout(proc: Proc) -> String {
    String::from_utf8(spawn_and_wait(proc).stdout).unwrap()
}

#[builtin("lines")]
fn lines_proc(proc: Proc) -> Vec<String> {
    stdout(proc).lines().map(|s| s.to_string()).collect()
}
