use std::collections::HashMap;

use crate::ast::{get_fn_by_name, Expr, Value};
use crate::ShadyContext;

#[derive(Debug)]
pub struct LocalContext {
    pub vars: HashMap<String, Value>,
}

fn eval_math_op(op: &str, a: &Value, b: &Value) -> Value {
    match (a, b) {
        (Value::Int(a), Value::Int(b)) => match op {
            "+" => Value::Int(a + b),
            "-" => Value::Int(a - b),
            "*" => Value::Int(a * b),
            "/" => Value::Int(a / b),
            "%" => Value::Int(a % b),
            "^" => Value::Int(a.pow(*b as u32)),
            _ => panic!("unknown operator {}", op),
        },
        _ => panic!("invalid arguments for operator {}", op),
    }
}

fn eval_comparison_op(op: &str, a: &Value, b: &Value) -> Value {
    match (a, b) {
        (Value::Int(a), Value::Int(b)) => match op {
            ">" => Value::Bool(a > b),
            ">=" => Value::Bool(a >= b),
            "<" => Value::Bool(a < b),
            "<=" => Value::Bool(a <= b),
            "==" => Value::Bool(a == b),
            "!=" => Value::Bool(a != b),
            _ => panic!("unknown operator {}", op),
        },
        (a, b) => match op {
            "==" => Value::Bool(a == b),
            "!=" => Value::Bool(a == b),
            _ => panic!("unknown operator {}", op),
        },
    }
}

fn eval_bool_op(op: &str, a: &Value, b: &Value) -> Value {
    match (a, b) {
        (Value::Bool(a), Value::Bool(b)) => match op {
            "&&" => Value::Bool(*a && *b),
            "||" => Value::Bool(*a || *b),
            _ => panic!("unknown operator {}", op),
        },
        _ => panic!("invalid arguments for operator {}", op),
    }
}

pub fn eval_expr(local_context: &LocalContext, context: &ShadyContext, expr: &Expr) -> Value {
    match expr {
        Expr::Value(value) => value.clone(),
        Expr::Variable(var_name) => {
            let value = local_context.vars.get(var_name);
            if value.is_none() {
                panic!("variable {} not found", var_name);
            }
            value.unwrap().clone()
        }
        Expr::Call { fn_name, arguments } => {
            let mut args = Vec::new();
            for arg in arguments {
                args.push(eval_expr(&local_context, &context, arg));
            }
            match fn_name.as_str() {
                "print" => {
                    match args[0] {
                        Value::String(ref s) => println!("{}", s),
                        Value::Int(ref i) => println!("{}", i),
                        Value::Bool(ref b) => println!("{}", b),
                    }
                    Value::Int(0)
                }
                "+" | "-" | "*" | "/" | "%" | "^" => {
                    eval_math_op(fn_name.as_str(), &args[0], &args[1])
                }
                "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                    eval_comparison_op(fn_name.as_str(), &args[0], &args[1])
                }
                "&&" | "||" => eval_bool_op(fn_name.as_str(), &args[0], &args[1]),
                _ => {
                    if let Some(fun) = get_fn_by_name(&context.program, fn_name) {
                        eval_expr(&local_context, &context, &fun.expr)
                    } else {
                        // run ls shell command
                        let mut cmd = std::process::Command::new(fn_name);
                        for arg in args {
                            match arg {
                                Value::String(s) => {
                                    cmd.arg(s);
                                }
                                Value::Int(i) => {
                                    cmd.arg(i.to_string());
                                }
                                Value::Bool(b) => {
                                    cmd.arg(b.to_string());
                                }
                            }
                        }
                        // TODO: properly deal with errors
                        let status = cmd.status().unwrap().code().unwrap();
                        Value::Int(status as i64)
                    }
                }
            }
        }
        Expr::Block { statements } => {
            let mut result = Value::Int(0);
            for statement in statements {
                result = eval_expr(&local_context, context, statement);
            }
            result
        }
        Expr::If {
            condition,
            when_true,
            when_false,
        } => {
            let cond_result = eval_expr(&local_context, context, condition);
            if cond_result == Value::Bool(true) {
                eval_expr(&local_context, context, when_true)
            } else {
                eval_expr(&local_context, context, when_false)
            }
        }
    }
}

mod tests {
    use super::*;
    use crate::ast::parse_script;
    use crate::ShadyArgs;

    macro_rules! eval_tests {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    let program = parse_script(&format!("main = {};", input));
                    let local_context = LocalContext { vars: HashMap::new() };
                    let args = ShadyArgs {
                        ast: false,
                        filename: "test.shady".to_string(),
                        args: Vec::new(),
                    };
                    let context = ShadyContext { args, program };
                    let expr = &context.program.fn_definitions[0].expr;
                    assert_eq!(
                        eval_expr(&local_context, &context, expr),
                        expected,
                    );
                }
            )*
        }
    }

    eval_tests! {
        eval_value: ("1", Value::Int(1)),
        eval_add: ("1 + 2", Value::Int(3)),
        eval_sub: ("1 - 2", Value::Int(-1)),
        eval_precedence: ("3 + 2 * 5", Value::Int(13)),
        eval_precedence_2: ("(3 + 2) * 5", Value::Int(25)),
        eval_eq: ("1 == 1", Value::Bool(true)),
        eval_neq: ("1 != 1", Value::Bool(false)),
        eval_gt: ("2 > 1", Value::Bool(true)),
        eval_eq_str: (r#""a" == "b""#, Value::Bool(false)),
        eval_eq_str_2: (r#""a" == "a""#, Value::Bool(true)),
        eval_and: ("true && false", Value::Bool(false)),
        eval_and_2: ("true && true", Value::Bool(true)),
        eval_or: ("false || false", Value::Bool(false)),
        eval_or_2: ("false || true", Value::Bool(true)),
        eval_bool_and_math: ("1 > 2 || 3 < 3 + 1", Value::Bool(true)),
        eval_bool_precedence: ("true || false && false", Value::Bool(true)),
        eval_if: ("if true then 42 else 666", Value::Int(42)),
        eval_else: ("if false then 42 else 666", Value::Int(666)),
        eval_else_if: ("if false then 42 else if false then 666 else 51", Value::Int(51)),
        eval_else_if_2: ("if false then 42 else if true then 666 else 51", Value::Int(666)),
    }
}
