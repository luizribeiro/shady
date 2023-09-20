use crate::types::Proc;
use shady_macros::builtin;
use std::io::prelude::*;
use std::{io, thread};

#[builtin]
fn exec(proc: Proc) -> i64 {
    let stdin_thread = redirect(io::stdin(), proc.stdin_writer);
    let stdout_thread = redirect(proc.stdout_reader, io::stdout());
    let stderr_thread = redirect(proc.stderr_reader, io::stderr());
    if !atty::is(atty::Stream::Stdin) {
        stdin_thread.join().unwrap();
    }
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
    let stdin_thread = redirect(io::stdin(), proc.stdin_writer);
    let stderr_thread = redirect(proc.stderr_reader, io::stderr());
    let mut output = String::new();
    if !atty::is(atty::Stream::Stdin) {
        stdin_thread.join().unwrap();
    }
    proc.stdout_reader.read_to_string(&mut output).unwrap();
    stderr_thread.join().unwrap();
    output
}

#[builtin("lines")]
fn lines_proc(proc: Proc) -> Vec<String> {
    stdout(proc).lines().map(|s| s.to_string()).collect()
}

#[builtin(">", infix = true)]
fn pipe_stdout(a: Proc, b: Proc) -> Proc {
    let (stderr_reader, stderr_writer) = os_pipe::pipe().unwrap();
    redirect(a.stdout_reader, b.stdin_writer);
    redirect(a.stderr_reader, stderr_writer.try_clone().unwrap());
    redirect(b.stderr_reader, stderr_writer);
    Proc {
        program: "".to_string(),
        args: vec![],
        stdin_writer: a.stdin_writer,
        stdout_reader: b.stdout_reader,
        stderr_reader,
    }
}

#[builtin("<", infix = true)]
fn pipe_stdout_reversed(a: Proc, b: Proc) -> Proc {
    pipe_stdout(b, a)
}

fn redirect<'a, R: Send + 'static, W: Send + 'static>(mut a: R, mut b: W) -> thread::JoinHandle<()>
where
    R: Read,
    W: Write,
{
    // TODO: move to async I/O instead of threading
    thread::spawn(move || {
        io::copy(&mut a, &mut b).unwrap();
    })
}
