use crate::types::{Proc, Value};
use shady_macros::builtin;
use std::process::{Child, Command, Output, Stdio};

fn custom_spawn<F: FnMut(&mut Command)>(proc: &Proc, mut fun: F) -> Child {
    let mut command = Command::new(&proc.program);
    command.args(&proc.args);
    fun(&mut command);
    command.spawn().expect("Failed to execute process")
}

fn spawn(proc: Proc) -> Child {
    if let Some(ref stdout) = proc.stdout {
        let mut child = custom_spawn(&proc, |command| {
            command.stdout(Stdio::piped());
        });

        custom_spawn(stdout, |command| {
            command.stdin(child.stdout.take().unwrap());
        })
    } else {
        custom_spawn(&proc, |_command| {})
    }
}

fn spawn_and_wait(proc: Proc) -> Output {
    spawn(proc)
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

#[cfg(test)]
mod tests {
    use crate::test_utils::call_main;

    #[test]
    fn test_proc() {
        call_main("public main = exec (echo Hello World);", &[])
            .assert()
            .success()
            .stdout("Hello World\n");
    }

    #[test]
    fn test_stdout_redirection() {
        call_main("public main = exec (echo Hello World > grep World);", &[])
            .assert()
            .success()
            .stdout("Hello World\n");

        call_main("public main = exec (echo Hello People > grep World);", &[])
            .assert()
            .success()
            .stdout("");
    }
}
