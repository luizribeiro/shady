use crate::types::Value;
use shady_macros::builtin;
use std::process::Command;

#[builtin]
fn exec(proc: Value) -> i64 {
    let (program, args) = match proc {
        Value::Proc { program, args } => (program, args),
        _ => panic!("expected proc"),
    };

    Command::new(program)
        .args(args)
        .status()
        .expect("failed to execute process")
        .code()
        .unwrap_or(0) as i64
}
