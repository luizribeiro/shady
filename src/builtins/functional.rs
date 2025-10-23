// Higher-order functional primitives (map, filter, reduce)
// These are implemented as Eval builtins (special forms) because they need
// access to the evaluation context to call lambdas.

use crate::ast::Expr;
use crate::error::{Result, ShadyError};
use crate::eval::{eval_expr_with_type, LocalContext, ShadyContext};
use crate::types::{Type, Value};
use shady_macros::eval_builtin;

/// Implements map: applies a lambda to each element of a list
/// map $fn: fn(T) -> R $list: [T] -> [R]
#[eval_builtin("map", "fn(any)->any, [any]", "[any]")]
pub fn map_impl(
    arg_exprs: &[Expr],
    local_context: &LocalContext,
    context: &ShadyContext,
) -> Result<Value> {
    // Argument validation is handled by the macro-generated wrapper
    let lambda_expr = &arg_exprs[0];
    let list_expr = &arg_exprs[1];

    // Re-evaluate the lambda (already validated by wrapper)
    let lambda_val = eval_expr_with_type(local_context, context, lambda_expr, None)?;
    let lambda = match lambda_val {
        Value::Lambda(l) => l,
        _ => unreachable!("Lambda validation should have been done by wrapper"),
    };

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
#[eval_builtin("filter", "fn(any)->bool, [any]", "[any]")]
pub fn filter_impl(
    arg_exprs: &[Expr],
    local_context: &LocalContext,
    context: &ShadyContext,
) -> Result<Value> {
    // Argument validation is handled by the macro-generated wrapper
    let lambda_expr = &arg_exprs[0];
    let list_expr = &arg_exprs[1];

    // Re-evaluate the lambda (already validated by wrapper)
    let lambda_val = eval_expr_with_type(local_context, context, lambda_expr, None)?;
    let lambda = match lambda_val {
        Value::Lambda(l) => l,
        _ => unreachable!("Lambda validation should have been done by wrapper"),
    };

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
#[eval_builtin("reduce", "fn(any,any)->any, any, [any]", "any")]
pub fn reduce_impl(
    arg_exprs: &[Expr],
    local_context: &LocalContext,
    context: &ShadyContext,
) -> Result<Value> {
    // Argument validation is handled by the macro-generated wrapper
    let lambda_expr = &arg_exprs[0];
    let init_expr = &arg_exprs[1];
    let list_expr = &arg_exprs[2];

    // Re-evaluate the lambda (already validated by wrapper)
    let lambda_val = eval_expr_with_type(local_context, context, lambda_expr, None)?;
    let lambda = match lambda_val {
        Value::Lambda(l) => l,
        _ => unreachable!("Lambda validation should have been done by wrapper"),
    };

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
