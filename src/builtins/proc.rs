use crate::error::{Result, ShadyError};
use crate::types::Proc;
use shady_macros::builtin;
use std::io::prelude::*;
use std::{io, thread};

#[builtin]
pub fn exec(proc: Proc) -> Result<i64> {
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
    // Close stdin immediately - stdout capture doesn't need terminal input
    drop(proc.stdin_writer);

    // Discard stderr by not redirecting it
    drop(proc.stderr_reader);

    // Read all stdout
    let mut output = String::new();
    proc.stdout_reader
        .read_to_string(&mut output)
        .map_err(|e| ShadyError::IoError(format!("failed to read stdout: {}", e)))?;

    // Wait for ALL processes in the pipeline to complete
    for child in &proc.pipeline_children {
        child.borrow_mut().wait()?;
    }
    proc.child.borrow_mut().wait()?;

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
    // Collect all children from both processes for proper cleanup
    let mut pipeline_children = a.pipeline_children.clone();
    pipeline_children.push(a.child.clone());
    pipeline_children.extend(b.pipeline_children.clone());

    Ok(Proc {
        child: b.child,
        program: "".to_string(),
        args: vec![],
        stdin_writer: a.stdin_writer,
        stdout_reader: b.stdout_reader,
        stderr_reader,
        pipeline_children,
    })
}

#[builtin("<", infix = true)]
fn pipe_stdout_reversed(a: Proc, b: Proc) -> Result<Proc> {
    pipe_stdout(b, a)
}

#[builtin(">", infix = true)]
fn pipe_string_to_proc(input: String, mut proc: Proc) -> Result<String> {
    // Write input to stdin and close it
    proc.stdin_writer
        .write_all(input.as_bytes())
        .map_err(|e| ShadyError::IoError(format!("failed to write to stdin: {}", e)))?;
    drop(proc.stdin_writer); // Close stdin to signal EOF

    // Redirect stderr in background
    let stderr_thread = redirect(proc.stderr_reader, io::stderr());

    // Read stdout
    let mut output = String::new();
    proc.stdout_reader
        .read_to_string(&mut output)
        .map_err(|e| ShadyError::IoError(format!("failed to read stdout: {}", e)))?;

    stderr_thread.join().map_err(|_| ShadyError::ThreadPanic)?;

    Ok(output)
}

fn redirect<R, W>(mut a: R, mut b: W) -> thread::JoinHandle<()>
where
    R: Read + Send + 'static,
    W: Write + Send + 'static,
{
    // TODO: move to async I/O instead of threading
    thread::spawn(move || {
        // Ignore errors - broken pipes are expected when processes terminate
        let _ = io::copy(&mut a, &mut b);
    })
}
