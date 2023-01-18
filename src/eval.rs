use crate::ast::{get_fn_by_name, Expr, Value};
use crate::ShadyContext;

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
                    println!("{:?}", args[0]);
                    Value::Int(0)
                }
                "*" => {
                    let a = match args[0] {
                        Value::Int(a) => a,
                        _ => panic!("add: expected int"),
                    };
                    let b = match args[1] {
                        Value::Int(b) => b,
                        _ => panic!("add: expected int"),
                    };
                    Value::Int(a * b)
                }
                "+" => {
                    let a = match args[0] {
                        Value::Int(a) => a,
                        _ => panic!("add: expected int"),
                    };
                    let b = match args[1] {
                        Value::Int(b) => b,
                        _ => panic!("add: expected int"),
                    };
                    Value::Int(a + b)
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
