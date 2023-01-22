use std::collections::HashMap;

use crate::ast::{get_fn_by_name, Expr, FnSignature, Parameter, ProgramAST};
use crate::builtins;
use crate::types::{Type, Value};

pub type BuiltinIndex = HashMap<FnSignature, Box<dyn Fn(Vec<Value>) -> Value>>;

pub struct ShadyContext {
    pub filename: String,
    pub program: ProgramAST,
    builtins: BuiltinIndex,
}

impl Value {
    fn get_type(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int,
            Value::Str(_) => Type::Str,
            Value::Bool(_) => Type::Bool,
        }
    }
}

pub trait PrimitiveValue {
    fn value_type() -> Type;
    fn from_value(value: Value) -> Self;
    fn to_value(&self) -> Value;
}

impl PrimitiveValue for i64 {
    fn value_type() -> Type {
        Type::Int
    }

    fn from_value(value: Value) -> Self {
        match value {
            Value::Int(i) => i,
            _ => panic!("Expected int value"),
        }
    }

    fn to_value(&self) -> Value {
        Value::Int(*self)
    }
}

impl PrimitiveValue for String {
    fn value_type() -> Type {
        Type::Str
    }

    fn from_value(value: Value) -> Self {
        match value {
            Value::Str(s) => s,
            _ => panic!("Expected string value"),
        }
    }

    fn to_value(&self) -> Value {
        Value::Str(self.clone())
    }
}

impl PrimitiveValue for bool {
    fn value_type() -> Type {
        Type::Bool
    }

    fn from_value(value: Value) -> Self {
        match value {
            Value::Bool(b) => b,
            _ => panic!("Expected bool value"),
        }
    }

    fn to_value(&self) -> Value {
        Value::Bool(*self)
    }
}

pub fn value_type<T: PrimitiveValue>() -> Type {
    <T>::value_type()
}

pub fn from_value<T: PrimitiveValue>(value: Value) -> T {
    <T>::from_value(value)
}

pub fn to_value<T: PrimitiveValue>(value: T) -> Value {
    <T>::to_value(&value)
}

pub trait BuiltinAdder {
    fn add<
        Ta: PrimitiveValue + 'static,
        Tb: PrimitiveValue + 'static,
        Tr: PrimitiveValue + 'static,
    >(
        &mut self,
        name: &str,
        fun: fn(Ta, Tb) -> Tr,
    );
}

impl BuiltinAdder for BuiltinIndex {
    fn add<
        Ta: PrimitiveValue + 'static,
        Tb: PrimitiveValue + 'static,
        Tr: PrimitiveValue + 'static,
    >(
        &mut self,
        name: &str,
        fun: fn(Ta, Tb) -> Tr,
    ) {
        let signature = FnSignature {
            fn_name: name.to_string(),
            parameters: vec![
                Parameter {
                    name: "x".to_string(),
                    typ: Ta::value_type(),
                },
                Parameter {
                    name: "x".to_string(),
                    typ: Tb::value_type(),
                },
            ],
            is_public: true,
            is_infix: false,
        };
        self.insert(
            signature,
            Box::new(move |args| {
                let a = Ta::from_value(args[0].clone());
                let b = Tb::from_value(args[1].clone());
                let r = fun(a, b);
                r.to_value()
            }),
        );
    }
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
                        Value::Str(ref s) => println!("{}", s),
                        Value::Int(ref i) => println!("{}", i),
                        Value::Bool(ref b) => println!("{}", b),
                    }
                    Value::Int(0)
                }
                _ => {
                    let signature = FnSignature {
                        fn_name: fn_name.clone(),
                        parameters: args
                            .iter()
                            .map(|a| Parameter {
                                name: "x".to_string(),
                                typ: a.get_type(),
                            })
                            .collect(),
                        is_public: true,
                        is_infix: false,
                    };
                    if let Some(builtin_fn) = context.builtins.get(&signature) {
                        builtin_fn(args)
                    } else if let Some(fun) = get_fn_by_name(&context.program, fn_name) {
                        let mut local_context = LocalContext {
                            vars: HashMap::new(),
                        };
                        for (i, param) in fun.signature.parameters.iter().enumerate() {
                            local_context
                                .vars
                                .insert(param.name.clone(), args[i].clone());
                        }
                        eval_expr(&local_context, &context, &fun.expr)
                    } else {
                        // run ls shell command
                        let mut cmd = std::process::Command::new(fn_name);
                        for arg in args {
                            match arg {
                                Value::Str(s) => {
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

    #[allow(dead_code)]
    fn eval_script(script: &str) -> Value {
        let program = parse_script(script);
        let local_context = LocalContext {
            vars: HashMap::new(),
        };
        let context = build_context("test.shady".to_string(), program);
        let expr = &context.program.fn_definitions[0].expr;
        eval_expr(&local_context, &context, expr)
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
