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
                    }
                    Value::Int(0)
                }
                "+" | "-" | "*" | "/" | "%" | "^" => {
                    eval_math_op(fn_name.as_str(), &args[0], &args[1])
                }
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
                            }
                        }
                        println!("cmd: {:#?}", cmd);
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
    }
}
