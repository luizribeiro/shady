use crate::error::{Result, ShadyError};
use crate::types::Proc;
use shady_macros::builtin;
use std::io::prelude::*;
use std::{io, thread};

#[builtin]
fn exec(proc: Proc) -> Result<i64> {
    let stdin_thread = redirect(io::stdin(), proc.stdin_writer);
    let stdout_thread = redirect(proc.stdout_reader, io::stdout());
    let stderr_thread = redirect(proc.stderr_reader, io::stderr());

    if !atty::is(atty::Stream::Stdin) {
        stdin_thread.join().map_err(|_| ShadyError::ThreadPanic)?;
    }
    stdout_thread.join().map_err(|_| ShadyError::ThreadPanic)?;
    stderr_thread.join().map_err(|_| ShadyError::ThreadPanic)?;

    let exit_status = proc.child.borrow_mut().wait()?;
    let code = exit_status.code().unwrap_or(1); // Default to 1 if killed by signal
    Ok(code as i64)
}

#[builtin]
fn seq(procs: Vec<Proc>) -> Result<i64> {
    let mut last = 0;
    for proc in procs {
        last = exec(proc)?;
    }
    Ok(last)
}

#[builtin]
fn stdout(mut proc: Proc) -> Result<String> {
    let stdin_thread = redirect(io::stdin(), proc.stdin_writer);
    let stderr_thread = redirect(proc.stderr_reader, io::stderr());
    let mut output = String::new();

    if !atty::is(atty::Stream::Stdin) {
        stdin_thread.join().map_err(|_| ShadyError::ThreadPanic)?;
    }
    proc.stdout_reader
        .read_to_string(&mut output)
        .map_err(|e| ShadyError::IoError(format!("failed to read stdout: {}", e)))?;
    stderr_thread.join().map_err(|_| ShadyError::ThreadPanic)?;

    Ok(output)
}

#[builtin("lines")]
fn lines_proc(proc: Proc) -> Result<Vec<String>> {
    let output = stdout(proc)?;
    Ok(output.lines().map(|s| s.to_string()).collect())
}

#[builtin(">", infix = true)]
fn pipe_stdout(a: Proc, b: Proc) -> Result<Proc> {
    let (stderr_reader, stderr_writer) = os_pipe::pipe()
        .map_err(|e| ShadyError::IoError(format!("failed to create pipe: {}", e)))?;
    redirect(a.stdout_reader, b.stdin_writer);
    let stderr_writer_clone = stderr_writer
        .try_clone()
        .map_err(|e| ShadyError::PipeCloneError(format!("failed to clone stderr pipe: {}", e)))?;
    redirect(a.stderr_reader, stderr_writer_clone);
    redirect(b.stderr_reader, stderr_writer);
    Ok(Proc {
        child: b.child,
        program: "".to_string(),
        args: vec![],
        stdin_writer: a.stdin_writer,
        stdout_reader: b.stdout_reader,
        stderr_reader,
    })
}

#[builtin("<", infix = true)]
fn pipe_stdout_reversed(a: Proc, b: Proc) -> Result<Proc> {
    pipe_stdout(b, a)
}

fn redirect<R, W>(mut a: R, mut b: W) -> thread::JoinHandle<()>
where
    R: Read + Send + 'static,
    W: Write + Send + 'static,
{
    // TODO: move to async I/O instead of threading
    thread::spawn(move || {
        io::copy(&mut a, &mut b)
            .expect("I/O copy failed in redirect thread - this may indicate a broken pipe or system I/O error");
    })
}
