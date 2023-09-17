use crate::types::{Proc, Value};
use shady_macros::builtin;
use std::process::{Child, Command, Output, Stdio};

fn custom_spawn<F: FnMut(&mut Command)>(proc: &Proc, mut fun: F) -> Child {
    let mut command = Command::new(&proc.program);
    command.args(&proc.args);
    fun(&mut command);
    command.spawn().expect("Failed to execute process")
}

fn spawn(proc: Proc, collect_stdout: bool) -> Child {
    if let Some(ref stdout) = proc.stdout {
        let mut child = custom_spawn(&proc, |command| {
            command.stdout(Stdio::piped());
        });

        custom_spawn(stdout, |command| {
            command.stdin(child.stdout.take().unwrap());
            if collect_stdout {
                command.stdout(Stdio::piped());
            }
        })
    } else {
        custom_spawn(&proc, |command| {
            if collect_stdout {
                command.stdout(Stdio::piped());
            }
        })
    }
}

fn spawn_and_wait(proc: Proc, collect_stdout: bool) -> Output {
    spawn(proc, collect_stdout)
        .wait_with_output()
        .expect("Failed to wait on child")
}

#[builtin]
fn proc(program: String, args: Vec<String>) -> Value {
    Value::Proc(Proc {
        program,
        args,
        stdout: None,
    })
}

#[builtin]
fn exec(proc: Proc) -> i64 {
    spawn_and_wait(proc, false).status.code().unwrap_or(0) as i64
}

#[builtin]
fn seq(procs: Vec<Proc>) -> i64 {
    let mut last = 0;
    for proc in procs {
        last = spawn_and_wait(proc, false).status.code().unwrap_or(0) as i64;
    }
    last
}

#[builtin]
fn stdout(proc: Proc) -> String {
    String::from_utf8(spawn_and_wait(proc, true).stdout).unwrap()
}

#[builtin("lines")]
fn lines_proc(proc: Proc) -> Vec<String> {
    stdout(proc).lines().map(|s| s.to_string()).collect()
}

#[builtin(">", infix = true)]
fn proc_into_prod(a: Proc, b: Proc) -> Proc {
    Proc {
        stdout: Some(Box::new(b)),
        ..a
    }
}

#[builtin("<", infix = true)]
fn proc_into_prod_reversed(a: Proc, b: Proc) -> Proc {
    Proc {
        stdout: Some(Box::new(a)),
        ..b
    }
}
