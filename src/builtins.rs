use crate::eval::{BuiltinAdder, BuiltinIndex};

pub fn setup_builtins(builtins: &mut BuiltinIndex) {
    builtins.add("+", |a: i64, b: i64| a + b);
    builtins.add("-", |a: i64, b: i64| a - b);
    builtins.add("*", |a: i64, b: i64| a * b);
    builtins.add("/", |a: i64, b: i64| a / b);
    builtins.add("%", |a: i64, b: i64| a % b);
    builtins.add("^", |a: i64, b: i64| a.pow(b as u32));

    builtins.add(">", |a: i64, b: i64| a > b);
    builtins.add(">=", |a: i64, b: i64| a >= b);
    builtins.add("<", |a: i64, b: i64| a < b);
    builtins.add("<=", |a: i64, b: i64| a <= b);
    builtins.add("==", |a: i64, b: i64| a == b);
    builtins.add("!=", |a: i64, b: i64| a != b);

    builtins.add("&&", |a: bool, b: bool| a && b);
    builtins.add("||", |a: bool, b: bool| a || b);
    builtins.add("==", |a: String, b: String| a == b);
    builtins.add("!=", |a: String, b: String| a != b);
    builtins.add("==", |a: bool, b: bool| a == b);
    builtins.add("!=", |a: bool, b: bool| a != b);

    builtins.add("==", |a: String, b: String| a == b);
    builtins.add("!=", |a: String, b: String| a != b);

    builtins.add("env", |name: String, default: String| {
        std::env::var(name).unwrap_or(default)
    });
}
