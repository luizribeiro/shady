use crate::types::Proc;
use shady_macros::builtin;
use std::io::prelude::*;
use std::{io, thread};

#[builtin]
fn exec(mut proc: Proc) -> i64 {
    // TODO: add support for stdin
    let stdout_thread = thread::spawn(move || {
        io::copy(&mut proc.stdout_reader, &mut io::stdout()).unwrap();
    });
    let stderr_thread = thread::spawn(move || {
        io::copy(&mut proc.stderr_reader, &mut io::stderr()).unwrap();
    });
    stdout_thread.join().unwrap();
    stderr_thread.join().unwrap();
    42 // FIXME: return the exit code
}

#[builtin]
fn seq(procs: Vec<Proc>) -> i64 {
    let mut last = 0;
    for proc in procs {
        last = exec(proc);
    }
    last
}

#[builtin]
fn stdout(mut proc: Proc) -> String {
    // TODO: add support for stdin
    let stderr_thread = thread::spawn(move || {
        io::copy(&mut proc.stderr_reader, &mut io::stderr()).unwrap();
    });
    let mut output = String::new();
    proc.stdout_reader.read_to_string(&mut output).unwrap();
    stderr_thread.join().unwrap();
    output
}

#[builtin("lines")]
fn lines_proc(proc: Proc) -> Vec<String> {
    stdout(proc).lines().map(|s| s.to_string()).collect()
}

fn pipe_stdout(mut a: Proc, mut b: Proc) -> Proc {
    thread::spawn(move || {
        io::copy(&mut a.stdout_reader, &mut b.stdin_writer).unwrap();
    });

    let (stderr_reader, mut stderr_writer) = os_pipe::pipe().unwrap();
    let mut stderr_writer_clone = stderr_writer.try_clone().unwrap();
    thread::spawn(move || {
        io::copy(&mut a.stderr_reader, &mut stderr_writer).unwrap();
    });
    thread::spawn(move || {
        io::copy(&mut b.stderr_reader, &mut stderr_writer_clone).unwrap();
    });

    Proc {
        program: "".to_string(),
        args: vec![],
        stdin_writer: a.stdin_writer,
        stdout_reader: b.stdout_reader,
        stderr_reader,
    }
}

#[builtin(">", infix = true)]
fn proc_into_prod(a: Proc, b: Proc) -> Proc {
    pipe_stdout(a, b)
}

#[builtin("<", infix = true)]
fn proc_into_prod_reversed(a: Proc, b: Proc) -> Proc {
    proc_into_prod(b, a)
}
