// Higher-order functional primitives (map, filter, reduce)
// These are implemented as Eval builtins (special forms) because they need
// access to the evaluation context to call lambdas.

use crate::ast::{Expr, FnSignature, ParamSpec, Parameter};
use crate::error::{Result, ShadyError};
use crate::eval::{eval_expr_with_type, Builtin, BuiltinIndex, LocalContext, ShadyContext};
use crate::types::{Type, Value};

/// Implements map: applies a lambda to each element of a list
/// map $fn: fn(T) -> R $list: [T] -> [R]
pub fn map_impl(
    arg_exprs: &[Expr],
    local_context: &LocalContext,
    context: &ShadyContext,
) -> Result<Value> {
    if arg_exprs.len() != 2 {
        return Err(ShadyError::EvalError(format!(
            "map requires exactly 2 arguments, got {}",
            arg_exprs.len()
        )));
    }

    let lambda_expr = &arg_exprs[0];
    let list_expr = &arg_exprs[1];

    // Evaluate the lambda
    let lambda_val = eval_expr_with_type(local_context, context, lambda_expr, None)?;
    let lambda = match lambda_val {
        Value::Lambda(l) => l,
        _ => {
            return Err(ShadyError::TypeMismatch {
                expected: "lambda function".to_string(),
                actual: format!("{}", lambda_val.get_type()),
                span: lambda_expr.span().to_source_span(),
            })
        }
    };

    // Check that lambda takes exactly one parameter
    if lambda.parameters.len() != 1 {
        return Err(ShadyError::FunctionSignatureMismatch {
            name: "map".to_string(),
            arg_types: format!(
                "map requires a lambda with 1 parameter, got {}",
                lambda.parameters.len()
            ),
            span: lambda_expr.span().to_source_span(),
        });
    }

    // Evaluate the list
    let list_val = eval_expr_with_type(local_context, context, list_expr, None)?;
    let (_inner_type, values) = match list_val {
        Value::List { inner_type, values } => (inner_type, values),
        _ => {
            return Err(ShadyError::TypeMismatch {
                expected: "list".to_string(),
                actual: format!("{}", list_val.get_type()),
                span: list_expr.span().to_source_span(),
            })
        }
    };

    // Apply lambda to each element
    let mut result_values = Vec::new();
    let mut result_type: Option<Type> = None;

    for value in values {
        // Create context with captured environment + parameter binding
        let mut lambda_vars = lambda.captured_env.clone();
        lambda_vars.insert(lambda.parameters[0].clone(), value);

        let lambda_context = LocalContext {
            vars: lambda_vars,
            depth: local_context.depth + 1,
        };

        // Evaluate lambda body
        let result = eval_expr_with_type(
            &lambda_context,
            context,
            &lambda.body,
            Some(&lambda.return_type),
        )?;

        // Determine result type from first element
        if result_type.is_none() {
            result_type = Some(result.get_type());
        }

        result_values.push(result);
    }

    // Handle empty list case
    let final_type = result_type.unwrap_or_else(|| lambda.return_type.clone());

    Ok(Value::List {
        inner_type: final_type,
        values: result_values,
    })
}

/// Implements filter: keeps only elements where lambda returns true
/// filter $fn: fn(T) -> bool $list: [T] -> [T]
pub fn filter_impl(
    arg_exprs: &[Expr],
    local_context: &LocalContext,
    context: &ShadyContext,
) -> Result<Value> {
    if arg_exprs.len() != 2 {
        return Err(ShadyError::EvalError(format!(
            "filter requires exactly 2 arguments, got {}",
            arg_exprs.len()
        )));
    }

    let lambda_expr = &arg_exprs[0];
    let list_expr = &arg_exprs[1];

    // Evaluate the lambda
    let lambda_val = eval_expr_with_type(local_context, context, lambda_expr, None)?;
    let lambda = match lambda_val {
        Value::Lambda(l) => l,
        _ => {
            return Err(ShadyError::TypeMismatch {
                expected: "lambda function".to_string(),
                actual: format!("{}", lambda_val.get_type()),
                span: lambda_expr.span().to_source_span(),
            })
        }
    };

    // Check that lambda takes exactly one parameter
    if lambda.parameters.len() != 1 {
        return Err(ShadyError::FunctionSignatureMismatch {
            name: "filter".to_string(),
            arg_types: format!(
                "filter requires a lambda with 1 parameter, got {}",
                lambda.parameters.len()
            ),
            span: lambda_expr.span().to_source_span(),
        });
    }

    // Check that lambda returns bool
    if lambda.return_type != Type::Bool && lambda.return_type != Type::Any {
        return Err(ShadyError::TypeMismatch {
            expected: "bool".to_string(),
            actual: format!("{}", lambda.return_type),
            span: lambda_expr.span().to_source_span(),
        });
    }

    // Evaluate the list
    let list_val = eval_expr_with_type(local_context, context, list_expr, None)?;
    let (inner_type, values) = match list_val {
        Value::List { inner_type, values } => (inner_type, values),
        _ => {
            return Err(ShadyError::TypeMismatch {
                expected: "list".to_string(),
                actual: format!("{}", list_val.get_type()),
                span: list_expr.span().to_source_span(),
            })
        }
    };

    // Filter elements where lambda returns true
    let mut result_values = Vec::new();

    for value in values {
        // Create context with captured environment + parameter binding
        let mut lambda_vars = lambda.captured_env.clone();
        lambda_vars.insert(lambda.parameters[0].clone(), value.clone());

        let lambda_context = LocalContext {
            vars: lambda_vars,
            depth: local_context.depth + 1,
        };

        // Evaluate lambda body
        let result =
            eval_expr_with_type(&lambda_context, context, &lambda.body, Some(&Type::Bool))?;

        // Check result is bool
        match result {
            Value::Bool(true) => result_values.push(value),
            Value::Bool(false) => {}
            _ => {
                return Err(ShadyError::TypeMismatch {
                    expected: "bool".to_string(),
                    actual: format!("{}", result.get_type()),
                    span: lambda_expr.span().to_source_span(),
                })
            }
        }
    }

    Ok(Value::List {
        inner_type,
        values: result_values,
    })
}

/// Implements reduce: folds a list using a lambda and initial value
/// reduce $fn: fn(Acc, T) -> Acc $init: Acc $list: [T] -> Acc
pub fn reduce_impl(
    arg_exprs: &[Expr],
    local_context: &LocalContext,
    context: &ShadyContext,
) -> Result<Value> {
    if arg_exprs.len() != 3 {
        return Err(ShadyError::EvalError(format!(
            "reduce requires exactly 3 arguments, got {}",
            arg_exprs.len()
        )));
    }

    let lambda_expr = &arg_exprs[0];
    let init_expr = &arg_exprs[1];
    let list_expr = &arg_exprs[2];

    // Evaluate the lambda
    let lambda_val = eval_expr_with_type(local_context, context, lambda_expr, None)?;
    let lambda = match lambda_val {
        Value::Lambda(l) => l,
        _ => {
            return Err(ShadyError::TypeMismatch {
                expected: "lambda function".to_string(),
                actual: format!("{}", lambda_val.get_type()),
                span: lambda_expr.span().to_source_span(),
            })
        }
    };

    // Check that lambda takes exactly two parameters (accumulator, element)
    if lambda.parameters.len() != 2 {
        return Err(ShadyError::FunctionSignatureMismatch {
            name: "reduce".to_string(),
            arg_types: format!(
                "reduce requires a lambda with 2 parameters (accumulator, element), got {}",
                lambda.parameters.len()
            ),
            span: lambda_expr.span().to_source_span(),
        });
    }

    // Evaluate the initial value
    let mut accumulator = eval_expr_with_type(local_context, context, init_expr, None)?;

    // Evaluate the list
    let list_val = eval_expr_with_type(local_context, context, list_expr, None)?;
    let values = match list_val {
        Value::List { values, .. } => values,
        _ => {
            return Err(ShadyError::TypeMismatch {
                expected: "list".to_string(),
                actual: format!("{}", list_val.get_type()),
                span: list_expr.span().to_source_span(),
            })
        }
    };

    // Fold the list
    for value in values {
        // Create context with captured environment + parameter bindings
        let mut lambda_vars = lambda.captured_env.clone();
        lambda_vars.insert(lambda.parameters[0].clone(), accumulator.clone());
        lambda_vars.insert(lambda.parameters[1].clone(), value);

        let lambda_context = LocalContext {
            vars: lambda_vars,
            depth: local_context.depth + 1,
        };

        // Evaluate lambda body
        accumulator = eval_expr_with_type(
            &lambda_context,
            context,
            &lambda.body,
            Some(&lambda.return_type),
        )?;
    }

    Ok(accumulator)
}

// Setup functions to register these as Eval builtins

#[allow(clippy::mutable_key_type)]
pub fn setup_map_builtin(builtins: &mut BuiltinIndex) {
    let signature = FnSignature {
        fn_name: "map".to_string(),
        parameters: vec![
            Parameter {
                name: "fn".to_string(),
                typ: Type::Fn(vec![Type::Any], Box::new(Type::Any)),
                spec: ParamSpec::default(),
            },
            Parameter {
                name: "list".to_string(),
                typ: Type::List(Box::new(Type::Any)),
                spec: ParamSpec::default(),
            },
        ],
        is_public: true,
        is_infix: false,
        return_type: Type::List(Box::new(Type::Any)),
    };
    builtins.insert(signature, Builtin::Eval(Box::new(map_impl)));
}

#[allow(clippy::mutable_key_type)]
pub fn setup_filter_builtin(builtins: &mut BuiltinIndex) {
    let signature = FnSignature {
        fn_name: "filter".to_string(),
        parameters: vec![
            Parameter {
                name: "fn".to_string(),
                typ: Type::Fn(vec![Type::Any], Box::new(Type::Bool)),
                spec: ParamSpec::default(),
            },
            Parameter {
                name: "list".to_string(),
                typ: Type::List(Box::new(Type::Any)),
                spec: ParamSpec::default(),
            },
        ],
        is_public: true,
        is_infix: false,
        return_type: Type::List(Box::new(Type::Any)),
    };
    builtins.insert(signature, Builtin::Eval(Box::new(filter_impl)));
}

#[allow(clippy::mutable_key_type)]
pub fn setup_reduce_builtin(builtins: &mut BuiltinIndex) {
    let signature = FnSignature {
        fn_name: "reduce".to_string(),
        parameters: vec![
            Parameter {
                name: "fn".to_string(),
                typ: Type::Fn(vec![Type::Any, Type::Any], Box::new(Type::Any)),
                spec: ParamSpec::default(),
            },
            Parameter {
                name: "init".to_string(),
                typ: Type::Any,
                spec: ParamSpec::default(),
            },
            Parameter {
                name: "list".to_string(),
                typ: Type::List(Box::new(Type::Any)),
                spec: ParamSpec::default(),
            },
        ],
        is_public: true,
        is_infix: false,
        return_type: Type::Any,
    };
    builtins.insert(signature, Builtin::Eval(Box::new(reduce_impl)));
}
