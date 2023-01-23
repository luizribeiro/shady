use std::collections::HashMap;

use crate::ast::{get_fn_by_name, Expr, FnSignature, Parameter, ProgramAST};
use crate::builtins;
use crate::types::Value;

pub type BuiltinIndex = HashMap<FnSignature, Box<dyn Fn(Vec<Value>) -> Value>>;

pub struct ShadyContext {
    pub filename: String,
    pub program: ProgramAST,
    builtins: BuiltinIndex,
}

#[derive(Debug)]
pub struct LocalContext {
    pub vars: HashMap<String, Value>,
}

impl LocalContext {
    pub fn new_empty() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }
}

pub fn build_context(filename: String, program: ProgramAST) -> ShadyContext {
    let mut builtins: BuiltinIndex = HashMap::new();

    builtins::setup_builtins(&mut builtins);

    ShadyContext {
        filename,
        program,
        builtins,
    }
}

pub fn eval_expr(local_context: &LocalContext, context: &ShadyContext, expr: &Expr) -> Value {
    match expr {
        Expr::Value(value) => value.clone(),
        Expr::Variable(var_name) => {
            let value = local_context.vars.get(var_name);
            if value.is_none() {
                panic!("variable {var_name} not found");
            }
            value.unwrap().clone()
        }
        Expr::Call { fn_name, arguments } => {
            let args: Vec<Value> = arguments
                .iter()
                .map(|arg| eval_expr(local_context, context, arg))
                .collect();
            let signature = FnSignature {
                fn_name: fn_name.clone(),
                parameters: args
                    .iter()
                    .map(|a| Parameter {
                        name: "".to_string(),
                        typ: a.get_type(),
                    })
                    .collect(),
                is_public: true,
                is_infix: false,
            };
            if let Some(builtin_fn) = context.builtins.get(&signature) {
                builtin_fn(args)
            } else if let Some(fun) = get_fn_by_name(&context.program, fn_name) {
                let vars: HashMap<String, Value> = fun
                    .signature
                    .parameters
                    .iter()
                    .enumerate()
                    .map(|(i, param)| (param.name.clone(), args[i].clone()))
                    .collect();
                let local_context = LocalContext { vars };
                eval_expr(&local_context, context, &fun.expr)
            } else {
                let mut cmd = std::process::Command::new(fn_name);
                for arg in args {
                    cmd.arg(arg.to_string());
                }
                // TODO: properly deal with errors
                let status = cmd.status().unwrap().code().unwrap();
                Value::Int(status as i64)
            }
        }
        Expr::Block { statements } => {
            let mut result = Value::Int(0);
            for statement in statements {
                result = eval_expr(local_context, context, statement);
            }
            result
        }
        Expr::If {
            condition,
            when_true,
            when_false,
        } => match eval_expr(local_context, context, condition) {
            Value::Bool(true) => eval_expr(local_context, context, when_true),
            Value::Bool(false) => eval_expr(local_context, context, when_false),
            _ => panic!("if condition must return a boolean"),
        },
    }
}

pub fn eval_fn(context: &ShadyContext, fn_name: &str, args: Vec<Value>) -> Value {
    // FIXME: this is pretty hacky, there's no need to build an expression to
    // call a function. it would be better to simply populate the LocalContext
    // and call the function directly
    eval_expr(
        &LocalContext::new_empty(),
        context,
        &Expr::Call {
            fn_name: fn_name.to_string(),
            arguments: args.iter().map(|a| Expr::Value(a.clone())).collect(),
        },
    )
}

mod tests {
    use super::*;
    use crate::ast::parse_script;

    #[allow(dead_code)]
    fn eval_script(script: &str) -> Value {
        let program = parse_script(script);
        let context = build_context("test.shady".to_string(), program);
        eval_fn(&context, "main", vec![])
    }

    macro_rules! eval_tests {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    assert_eq!(
                        eval_script(&format!("main = {};", input)),
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
        eval_if: ("if (true) 42 else 666", Value::Int(42)),
        eval_else: ("if (false) 42 else 666", Value::Int(666)),
        eval_else_if: ("if (false) 42 else if (false) 666 else 51", Value::Int(51)),
        eval_else_if_2: ("if (false) 42 else if (true) 666 else 51", Value::Int(666)),
    }

    #[test]
    fn eval_fib() {
        assert_eq!(
            eval_script(
                r"#
                    public main = fib 10;
                    fib $x: int = if ($x < 2) $x
                        else (fib ($x - 1)) + (fib ($x - 2));
                #"
            ),
            Value::Int(55),
        );
    }
}
