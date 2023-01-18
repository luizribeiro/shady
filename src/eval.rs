use crate::ast::{get_fn_by_name, Expr, Value};
use crate::ShadyContext;

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

pub fn eval_expr(context: &ShadyContext, expr: &Expr) -> Value {
    match expr {
        Expr::Value(value) => value.clone(),
        Expr::Variable(var_name) => panic!("variable {} not found", var_name),
        Expr::Call { fn_name, arguments } => {
            let mut args = Vec::new();
            for arg in arguments {
                args.push(eval_expr(&context, arg));
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
                        eval_expr(&context, &fun.expr)
                    } else {
                        panic!("function {} not found", fn_name);
                    }
                }
            }
        }
        Expr::Block { statements } => {
            let mut result = Value::Int(0);
            for statement in statements {
                result = eval_expr(context, statement);
            }
            result
        }
    }
}
