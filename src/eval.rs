use std::collections::HashMap;

use crate::ast::{get_fn_by_name, Expr, FnDefinition, FnSignature, Parameter, ProgramAST};
use crate::builtins;
use crate::types::{Proc, Type, Value};

pub type BuiltinIndex = HashMap<FnSignature, Box<dyn Fn(Vec<Value>) -> Value>>;

fn get_builtin_fn<'a>(
    context: &'a ShadyContext,
    signature: &'a FnSignature,
) -> Option<&'a dyn Fn(Vec<Value>) -> Value> {
    match context.builtins.get_key_value(signature) {
        Some((_, f)) => Some(f.as_ref()),
        None => None,
    }
}

fn get_builtins_by_name<'a>(
    context: &'a ShadyContext,
    fn_name: &'a str,
) -> Option<Vec<&'a FnSignature>> {
    let mut result = Vec::new();
    for (signature, _) in context.builtins.iter() {
        if signature.fn_name == fn_name {
            result.push(signature);
        }
    }
    if result.is_empty() {
        None
    } else {
        Some(result)
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
        Expr::Call { .. } => eval_fn(local_context, context, expr),
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

fn build_signature(call: &Expr, types: Vec<Type>) -> FnSignature {
    match call {
        Expr::Call {
            fn_name, is_infix, ..
        } => FnSignature {
            fn_name: fn_name.to_string(),
            parameters: types
                .iter()
                .map(|t| Parameter {
                    name: "".to_string(),
                    typ: t.clone(),
                })
                .collect(),
            is_public: true,
            is_infix: *is_infix,
            return_type: Type::Any,
        },
        _ => panic!("not a call"),
    }
}

fn eval_fn(local_context: &LocalContext, context: &ShadyContext, expr: &Expr) -> Value {
    let arguments = match expr {
        Expr::Call { arguments, .. } => arguments
            .iter()
            .map(|arg| eval_expr(local_context, context, arg))
            .collect::<Vec<Value>>(),
        _ => panic!("not a call"),
    };
    let signature = build_signature(expr, arguments.iter().map(|a| a.get_type()).collect());

    if let Some(builtin_fn) = get_builtin_fn(context, &signature) {
        return builtin_fn(arguments);
    }

    if let Some(fns) = get_builtins_by_name(context, &signature.fn_name) {
        panic!(
            "function {} not found, did you mean one of these?\n{}",
            signature,
            fns.iter()
                .map(|s| format!("  {}", s))
                .collect::<Vec<String>>()
                .join("\n"),
        );
    }

    if let Some(fun) = get_fn_by_name(&context.program, &signature.fn_name) {
        return eval_local_fn(context, fun, &arguments);
    }

    Value::Proc(Proc {
        program: signature.fn_name,
        args: arguments.iter().map(|a| a.to_string()).collect(),
        stdout: None,
    })
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

#[cfg(test)]
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
        eval_list: ("[1+2; 3+4]", Value::List {
            inner_type: Type::Int,
            values: vec![Value::Int(3), Value::Int(7)],
        }),
        eval_first: ("first [5; 3; 2]", Value::Int(5)),
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

    #[test]
    fn eval_list_add() {
        assert_eq!(
            eval_script(
                r"#
                    public main = add_lists [1; 2] [3; 4];
                    add_lists $x: [int] $y: [int] = $x + $y;
                #"
            ),
            Value::List {
                inner_type: Type::Int,
                values: vec![Value::Int(1), Value::Int(2), Value::Int(3), Value::Int(4)],
            },
        );
    }
}
