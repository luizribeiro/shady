use std::collections::HashMap;

use crate::ast::{get_fn_by_name, Expr, FnDefinition, FnSignature, Parameter, ProgramAST};
use crate::builtins;
use crate::types::Value;

pub type BuiltinIndex = HashMap<FnSignature, Box<dyn Fn(Vec<Value>) -> Value>>;

fn get_builtin_fn<'a>(
    context: &'a ShadyContext,
    fn_name: &'a str,
    args: &[Value],
) -> Option<&'a dyn Fn(Vec<Value>) -> Value> {
    let signature = FnSignature {
        fn_name: fn_name.to_string(),
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
    match context.builtins.get(&signature) {
        Some(f) => Some(f.as_ref()),
        None => None,
    }
}

pub struct ShadyContext {
    pub filename: String,
    pub program: ProgramAST,
    builtins: BuiltinIndex,
}

#[derive(Debug)]
pub struct LocalContext {
    pub vars: HashMap<String, Value>,
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
            match value {
                Some(v) => v.clone(),
                None => panic!("variable {var_name} not found"),
            }
        }
        Expr::Call { fn_name, arguments } => {
            let args: Vec<Value> = arguments
                .iter()
                .map(|arg| eval_expr(local_context, context, arg))
                .collect();
            eval_fn(context, fn_name, args)
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
        Expr::List { elements } => {
            let values: Vec<Value> = elements
                .iter()
                .map(|e| eval_expr(local_context, context, e))
                .collect();
            let inner_type = match values.len() {
                0 => todo!("empty lists are not supported"),
                _ => values[0].get_type(),
            };
            if !values.iter().all(|v| v.get_type() == inner_type) {
                panic!("list elements must be of the same type");
            }
            Value::List { inner_type, values }
        }
    }
}

fn eval_fn(context: &ShadyContext, fn_name: &str, args: Vec<Value>) -> Value {
    if let Some(builtin_fn) = get_builtin_fn(context, fn_name, &args) {
        return builtin_fn(args);
    }

    if let Some(fun) = get_fn_by_name(&context.program, fn_name) {
        return eval_local_fn(context, fun, &args);
    }

    eval_shell_fn(fn_name, &args)
}

pub fn eval_shell_fn(fn_name: &str, args: &[Value]) -> Value {
    let mut cmd = std::process::Command::new(fn_name);
    for arg in args {
        cmd.arg(arg.to_string());
    }
    // TODO: properly deal with errors
    let status = cmd.status().unwrap().code().unwrap();
    Value::Int(status as i64)
}

pub fn eval_local_fn(context: &ShadyContext, fun: &FnDefinition, args: &[Value]) -> Value {
    let vars: HashMap<String, Value> = fun
        .signature
        .parameters
        .iter()
        .enumerate()
        .map(|(i, param)| (param.name.clone(), args[i].clone()))
        .collect();
    let local_context = LocalContext { vars };
    eval_expr(&local_context, context, &fun.expr)
}

// rustc doesn't deal well with macros and these checks
#[allow(dead_code)]
#[allow(unused_imports)]
mod tests {
    use super::*;
    use crate::ast::parse_script;
    use crate::types::Type;

    fn eval_script(script: &str) -> Value {
        let program = parse_script(script);
        let context = build_context("test.shady".to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").expect("main function not found");
        eval_local_fn(&context, fun, &[])
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
        eval_list: ("[1+2, 3+4]", Value::List {
            inner_type: Type::Int,
            values: vec![Value::Int(3), Value::Int(7)],
        }),
        // FIXME: builtin lookup is broken
        //eval_first: ("first [5, 3, 2]", Value::Int(5)),
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
