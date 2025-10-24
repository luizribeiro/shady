# Issue: 3-Process Pipeline Hanging

## Status
- **Test**: `test_stdout_redirection_chaining` in `tests/proc.rs:106`
- **Currently**: Ignored (marked with `#[ignore]`)
- **Severity**: High - blocks CI when not ignored

## Description

The test validates that 3-process pipelines work correctly when capturing stdout:

```rust
#[test]
fn test_stdout_redirection_chaining() {
    call_main(
        r#"
        this_host = stdout (echo "sodium") > (sed "s/o/a/g") > (tr -d "\n");
        public main = exec (echo ("pre" + (this_host) + "post"));
        "#,
        &[],
    )
    .assert()
    .success()
    .stdout("presadiumpost\n");
}
```

**Expected behavior**: The pipeline should execute `echo "sodium" | sed "s/o/a/g" | tr -d "\n"` and return "sadium"

**Actual behavior**: The test hangs indefinitely when running `stdout()` on the 3-process pipeline.

## Observations

### What Works
- ✅ Single process: `stdout (echo "hello")` - works fine
- ✅ 2-process pipeline: `stdout (echo "sodium") > (sed "s/o/a/g")` - works fine (verified by `test_stdout_from_redirection`)
- ❌ 3-process pipeline: `stdout (echo "sodium") > (sed "s/o/a/g") > (tr -d "\n")` - **hangs**

### Architecture Overview

When processes are piped together using the `>` operator:

```rust
// In src/builtins/proc.rs:58
fn pipe_stdout(a: Proc, b: Proc) -> Result<Proc> {
    // Creates redirect threads: a.stdout -> b.stdin
    redirect(a.stdout_reader, b.stdin_writer);

    // Merges stderr streams
    redirect(a.stderr_reader, stderr_writer_clone);
    redirect(b.stderr_reader, stderr_writer);

    // Collects all children from both processes
    let mut pipeline_children = a.pipeline_children.clone();
    pipeline_children.push(a.child.clone());
    pipeline_children.extend(b.pipeline_children.clone());

    // Returns combined Proc with all children tracked
    Ok(Proc {
        child: b.child,  // Only keeps last child as primary
        stdin_writer: a.stdin_writer,
        stdout_reader: b.stdout_reader,
        stderr_reader,
        pipeline_children,  // But tracks all children here
    })
}
```

When `stdout()` is called on a pipeline:

```rust
// In src/builtins/proc.rs:34
fn stdout(mut proc: Proc) -> Result<String> {
    drop(proc.stdin_writer);  // Close stdin
    drop(proc.stderr_reader);  // Discard stderr

    // Read all stdout - THIS IS WHERE IT HANGS
    let mut output = String::new();
    proc.stdout_reader.read_to_string(&mut output)?;

    // Wait for all pipeline children
    for child in &proc.pipeline_children {
        child.borrow_mut().wait()?;
    }
    proc.child.borrow_mut().wait()?;

    Ok(output)
}
```

## Investigation Done

### Attempt 1: Close stdin immediately
- **Rationale**: Maybe stdin redirection thread was blocking
- **Result**: Still hangs

### Attempt 2: Add pipeline_children tracking
- **Rationale**: Middle processes weren't being waited for
- **Result**: Still hangs (but this was necessary anyway)

### Attempt 3: Ignore redirect errors
- **Rationale**: Redirect threads might be panicking on broken pipes
- **Result**: Still hangs

### Attempt 4: Drop stderr instead of redirecting
- **Rationale**: Stderr redirect thread might be blocking
- **Result**: Still hangs

## Hypothesis: Deadlock in Pipe Chain

The issue appears to be a **circular wait condition** in the 3-process pipeline:

```
echo -> sed -> tr
```

Possible deadlock scenario:
1. `tr` is waiting to read from `sed`'s stdout
2. `sed` is waiting to read from `echo`'s stdout OR waiting for its stdout buffer to be drained by `tr`
3. `echo` is waiting for... something (maybe its stdout buffer to be drained by `sed`)
4. Meanwhile, `stdout()` is trying to read from `tr`'s stdout pipe

The `read_to_string()` call blocks until EOF, but EOF only comes when all writers to the pipe are closed. If any process in the chain is blocked on a full pipe buffer, we get a deadlock.

## Potential Root Causes

### 1. Pipe Buffer Deadlock
Unix pipes have limited buffer size (~64KB). If:
- Process A writes > 64KB
- Process B is blocked on writing to Process C
- Process C hasn't consumed Process B's output yet

Then Process A gets blocked, and we have a deadlock.

### 2. Process Not Starting
One of the middle processes (`sed`) might not be starting at all, causing the pipe chain to stall.

### 3. Redirect Thread Issues
The `redirect()` threads spawn background threads to copy data between pipes. If these threads are blocking or not running, data won't flow.

### 4. Child Process Wait Order
We wait for children AFTER reading stdout, but maybe we need to wait concurrently or in a different order for 3+ process chains.

## Potential Solutions

### Option 1: Use Async I/O
Replace threading with async/await using `tokio`:
```rust
async fn stdout(mut proc: Proc) -> Result<String> {
    drop(proc.stdin_writer);
    let output = tokio::io::read_to_string(proc.stdout_reader).await?;
    // Concurrently wait for all children
    Ok(output)
}
```

### Option 2: Read with Buffer and Poll
Instead of `read_to_string()` (which blocks), read in chunks while polling child processes:
```rust
loop {
    // Try to read available data
    // Check if children are still alive
    // Break when stdout is closed AND all children exited
}
```

### Option 3: Spawn Output Reader Thread
Read stdout in a background thread while waiting for children in the main thread:
```rust
let read_thread = thread::spawn(move || {
    let mut output = String::new();
    proc.stdout_reader.read_to_string(&mut output)?;
    Ok(output)
});

// Wait for all children
for child in &proc.pipeline_children {
    child.borrow_mut().wait()?;
}
proc.child.borrow_mut().wait()?;

// Then get output
let output = read_thread.join()??;
```

### Option 4: Debug Logging
Add extensive logging to understand exactly where it's blocking:
```rust
eprintln!("Closing stdin...");
drop(proc.stdin_writer);
eprintln!("Reading stdout...");
proc.stdout_reader.read_to_string(&mut output)?;
eprintln!("Waiting for {} children...", proc.pipeline_children.len());
```

## Next Steps

1. **Add debug logging** to understand where exactly it's hanging
2. **Test with simpler commands** - try `cat | cat | cat` instead of `echo | sed | tr`
3. **Check process state** - are all three processes actually running?
4. **Try spawning reader thread** (Option 3 above) as a quick fix
5. **Consider migrating to async I/O** for proper fix (Option 1)

## Files Involved

- `src/builtins/proc.rs` - Process builtin functions (pipe_stdout, stdout, exec)
- `src/types.rs` - Proc struct definition
- `src/eval.rs` - Process spawning logic
- `tests/proc.rs` - Integration tests for process handling

## Related Issues

- 2-process pipelines work fine, so the pipe_stdout logic is mostly correct
- The redirect() function may need improvement (currently just spawns threads)
- Process lifecycle management needs review
