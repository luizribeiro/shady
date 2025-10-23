use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{
    get_fn_by_name, Expr, FnDefinition, FnSignature, ParamSpec, Parameter, ProgramAST,
    StringSegment,
};
use crate::builtins;
use crate::error::{Result, ShadyError};
use crate::types::{Proc, Type, Value};

/// Builtin function types: Pure (eagerly evaluated) or Eval (context-aware)
pub enum Builtin {
    /// Pure builtin: receives evaluated arguments
    Pure(Box<dyn Fn(Vec<Value>) -> Result<Value>>),
    /// Eval builtin: receives evaluated arguments plus evaluation contexts (for calling lambdas, etc.)
    Eval(Box<dyn Fn(Vec<Value>, &LocalContext, &ShadyContext) -> Result<Value>>),
}

// FnSignature contains interior mutability through Value -> Proc -> Rc<RefCell<Child>>,
// but the Hash implementation only uses fn_name and is_infix, which are immutable.
#[allow(clippy::mutable_key_type)]
pub type BuiltinIndex = HashMap<FnSignature, Builtin>;

/// Resource limits for execution safety
#[derive(Debug, Clone)]
pub struct ResourceLimits {
    pub max_recursion_depth: usize,
    pub max_process_count: usize,
}

impl Default for ResourceLimits {
    fn default() -> Self {
        ResourceLimits {
            max_recursion_depth: 1000,
            max_process_count: 100,
        }
    }
}

fn get_builtin<'a>(context: &'a ShadyContext, signature: &'a FnSignature) -> Option<&'a Builtin> {
    context.builtins.get(signature)
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

/// Check if a call signature matches a function definition signature
fn signature_matches(definition: &FnSignature, call: &FnSignature) -> bool {
    // Check that we have the right number of arguments
    // (accounting for default values)
    let required_params = definition
        .parameters
        .iter()
        .filter(|p| p.spec.default_value.is_none())
        .count();
    let total_params = definition.parameters.len();
    let provided_args = call.parameters.len();

    if provided_args < required_params || provided_args > total_params {
        return false;
    }

    // Check that types match for all provided arguments
    for (i, provided_param) in call.parameters.iter().enumerate() {
        if let Some(expected_param) = definition.parameters.get(i) {
            if provided_param.typ != expected_param.typ {
                return false;
            }
        } else {
            return false;
        }
    }

    true
}

/// Find the index of the first argument that has a type mismatch
/// Returns None if signatures match or if the issue is argument count
fn find_mismatched_arg_index(definition: &FnSignature, call: &FnSignature) -> Option<usize> {
    // Check that types match for all provided arguments
    for (i, provided_param) in call.parameters.iter().enumerate() {
        if let Some(expected_param) = definition.parameters.get(i) {
            if provided_param.typ != expected_param.typ {
                return Some(i);
            }
        }
    }
    None
}

pub struct ShadyContext {
    pub filename: String,
    pub program: ProgramAST,
    pub builtins: BuiltinIndex,
    pub limits: ResourceLimits,
    process_count: RefCell<usize>,
}

#[derive(Debug)]
pub struct LocalContext {
    pub vars: HashMap<String, Value>,
    pub depth: usize,
}

pub fn build_context(filename: String, _source: String, program: ProgramAST) -> ShadyContext {
    build_context_with_limits(filename, program, ResourceLimits::default())
}

pub fn build_context_with_limits(
    filename: String,
    program: ProgramAST,
    limits: ResourceLimits,
) -> ShadyContext {
    #[allow(clippy::mutable_key_type)]
    let mut builtins: BuiltinIndex = HashMap::new();

    builtins::setup_builtins(&mut builtins);

    ShadyContext {
        filename,
        program,
        builtins,
        limits,
        process_count: RefCell::new(0),
    }
}

pub fn eval_expr_with_type(
    local_context: &LocalContext,
    context: &ShadyContext,
    expr: &Expr,
    expected_type: Option<&Type>,
) -> Result<Value> {
    // Check recursion depth limit
    if local_context.depth > context.limits.max_recursion_depth {
        return Err(ShadyError::RecursionLimitExceeded(
            context.limits.max_recursion_depth,
        ));
    }

    match expr {
        Expr::Value(value, _) => Ok(value.clone()),
        Expr::Variable(var_name, span) => {
            local_context
                .vars
                .get(var_name)
                .cloned()
                .ok_or_else(|| ShadyError::VariableNotFound {
                    name: var_name.clone(),
                    span: span.to_source_span(),
                })
        }
        Expr::Call { .. } => eval_fn(local_context, context, expr),
        Expr::If {
            condition,
            when_true,
            when_false,
            ..
        } => {
            let cond_val =
                eval_expr_with_type(local_context, context, condition, Some(&Type::Bool))?;
            match cond_val {
                Value::Bool(true) => {
                    eval_expr_with_type(local_context, context, when_true, expected_type)
                }
                Value::Bool(false) => {
                    eval_expr_with_type(local_context, context, when_false, expected_type)
                }
                _ => Err(ShadyError::TypeMismatch {
                    expected: "bool".to_string(),
                    actual: format!("{:?}", cond_val.get_type()),
                    span: condition.span().to_source_span(),
                }),
            }
        }
        Expr::Block { expressions, .. } => {
            // Evaluate all expressions in the block sequentially
            // Auto-exec intermediate Proc values, return the last value
            if expressions.is_empty() {
                // Empty block returns unit (we'll use Int(0) for now)
                return Ok(Value::Int(0));
            }

            let last_idx = expressions.len() - 1;
            for (i, expr) in expressions.iter().enumerate() {
                let value = eval_expr_with_type(
                    local_context,
                    context,
                    expr,
                    if i == last_idx { expected_type } else { None },
                )?;

                // For intermediate expressions (not the last one),
                // auto-exec if it's a Proc
                if i < last_idx {
                    if let Value::Proc(proc) = value {
                        builtins::proc::exec(proc)?;
                    }
                } else {
                    // Last expression - return its value
                    return Ok(value);
                }
            }

            unreachable!("Should have returned in the loop")
        }
        Expr::List { elements, span } => {
            // Extract inner type from expected type if it's a list type
            let expected_inner_type = expected_type.and_then(|t| match t {
                Type::List(inner) => Some(inner.as_ref()),
                _ => None,
            });

            let values: Vec<Value> = elements
                .iter()
                .map(|e| eval_expr_with_type(local_context, context, e, expected_inner_type))
                .collect::<Result<Vec<Value>>>()?;

            let inner_type = match values.len() {
                0 => {
                    // Use expected type for empty lists
                    expected_inner_type
                        .cloned()
                        .ok_or(ShadyError::EmptyListNeedsType {
                            span: span.to_source_span(),
                        })?
                }
                _ => values[0].get_type(),
            };

            if !values.iter().all(|v| v.get_type() == inner_type) {
                return Err(ShadyError::TypeMismatch {
                    expected: format!("[{}]", inner_type),
                    actual: "mixed types in list".to_string(),
                    span: span.to_source_span(),
                });
            }
            Ok(Value::List { inner_type, values })
        }
        Expr::InterpolatedString { segments, .. } => {
            // Evaluate each segment and concatenate into a final string
            let mut result = String::new();
            for segment in segments {
                match segment {
                    StringSegment::Text(text) => {
                        result.push_str(text);
                    }
                    StringSegment::Interpolated(expr) => {
                        let value = eval_expr_with_type(local_context, context, expr, None)?;
                        result.push_str(&value.to_string());
                    }
                }
            }
            Ok(Value::Str(result))
        }
        Expr::Lambda {
            parameters,
            body,
            return_type,
            ..
        } => {
            // Lambda evaluation: capture the current environment
            let captured_env = local_context.vars.clone();

            // Extract parameter names and types
            let param_names: Vec<String> =
                parameters.iter().map(|(name, _)| name.clone()).collect();
            let param_types: Vec<Type> = parameters.iter().map(|(_, typ)| typ.clone()).collect();

            // Determine return type (use provided or infer from expected_type or default to Any)
            let ret_type = return_type.clone().or_else(|| {
                // If expected_type is a function type, extract its return type
                expected_type.and_then(|t| match t {
                    Type::Fn(_, ret) => Some((**ret).clone()),
                    _ => Some(t.clone()),
                })
            }).unwrap_or(Type::Any);

            Ok(Value::Lambda(crate::types::Lambda {
                parameters: param_names,
                param_types,
                return_type: ret_type,
                body: Rc::new((**body).clone()),
                captured_env,
            }))
        }
    }
}

fn eval_fn(local_context: &LocalContext, context: &ShadyContext, expr: &Expr) -> Result<Value> {
    let (fn_name, arg_exprs, is_infix, call_span) = match expr {
        Expr::Call {
            fn_name,
            arguments,
            is_infix,
            span,
        } => (fn_name, arguments, *is_infix, span),
        _ => return Err(ShadyError::EvalError("not a call".to_string())),
    };

    // Check if fn_name is a variable containing a lambda
    if let Some(Value::Lambda(lambda)) = local_context.vars.get(fn_name) {
        // Evaluate arguments
        let arguments: Vec<Value> = arg_exprs
            .iter()
            .enumerate()
            .map(|(i, arg)| {
                let expected = lambda.param_types.get(i);
                eval_expr_with_type(local_context, context, arg, expected)
            })
            .collect::<Result<Vec<Value>>>()?;

        // Check argument count
        if arguments.len() != lambda.parameters.len() {
            return Err(ShadyError::FunctionSignatureMismatch {
                name: "<lambda>".to_string(),
                arg_types: format!(
                    "expected {} arguments, got {}",
                    lambda.parameters.len(),
                    arguments.len()
                ),
                span: call_span.to_source_span(),
            });
        }

        // Check argument types
        for (i, (arg, expected_type)) in arguments.iter().zip(&lambda.param_types).enumerate() {
            if arg.get_type() != *expected_type && *expected_type != Type::Any {
                return Err(ShadyError::FunctionSignatureMismatch {
                    name: "<lambda>".to_string(),
                    arg_types: format!(
                        "argument {} has wrong type: expected {}, got {}",
                        i,
                        expected_type,
                        arg.get_type()
                    ),
                    span: arg_exprs
                        .get(i)
                        .map(|e| e.span().to_source_span())
                        .unwrap_or_else(|| call_span.to_source_span()),
                });
            }
        }

        // Call the lambda: create new context with captured environment + parameters
        let mut lambda_vars = lambda.captured_env.clone();
        for (i, param_name) in lambda.parameters.iter().enumerate() {
            lambda_vars.insert(param_name.clone(), arguments[i].clone());
        }

        let lambda_context = LocalContext {
            vars: lambda_vars,
            depth: local_context.depth + 1,
        };

        // Evaluate lambda body
        return eval_expr_with_type(
            &lambda_context,
            context,
            &lambda.body,
            Some(&lambda.return_type),
        );
    }

    // Evaluate arguments for all builtins and user functions
    let param_types: Option<Vec<Type>> = {
        // Check if it's a user-defined function
        if let Some(fun) = get_fn_by_name(&context.program, fn_name) {
            Some(
                fun.signature
                    .parameters
                    .iter()
                    .map(|p| p.typ.clone())
                    .collect(),
            )
        } else {
            // Check builtins by name to get possible signatures
            if let Some(builtin_sigs) = get_builtins_by_name(context, fn_name) {
                // For now, just take the first signature's parameter types
                // In the future, we could be smarter about overload resolution
                builtin_sigs
                    .first()
                    .map(|sig| sig.parameters.iter().map(|p| p.typ.clone()).collect())
            } else {
                None
            }
        }
    };

    // Evaluate arguments with expected types if available
    let arguments: Vec<Value> = if let Some(ref types) = param_types {
        arg_exprs
            .iter()
            .enumerate()
            .map(|(i, arg)| {
                let expected = types.get(i);
                eval_expr_with_type(local_context, context, arg, expected)
            })
            .collect::<Result<Vec<Value>>>()?
    } else {
        // No type information available, evaluate without expected types
        arg_exprs
            .iter()
            .map(|arg| eval_expr_with_type(local_context, context, arg, None))
            .collect::<Result<Vec<Value>>>()?
    };

    let signature = FnSignature {
        fn_name: fn_name.to_string(),
        parameters: arguments
            .iter()
            .map(|v| Parameter {
                name: "".to_string(),
                typ: v.get_type(),
                spec: ParamSpec::default(),
            })
            .collect(),
        is_public: true,
        is_infix,
        return_type: Type::Any,
    };

    if let Some(builtin) = get_builtin(context, &signature) {
        return match builtin {
            Builtin::Pure(f) => f(arguments.clone()),
            Builtin::Eval(f) => f(arguments.clone(), local_context, context),
        };
    }

    if let Some(fns) = get_builtins_by_name(context, &signature.fn_name) {
        // Try to find which argument has the wrong type
        let error_span = fns
            .iter()
            .find_map(|f| find_mismatched_arg_index(f, &signature))
            .and_then(|i| arg_exprs.get(i))
            .map(|expr| expr.span().to_source_span())
            .unwrap_or_else(|| call_span.to_source_span());

        return Err(ShadyError::FunctionSignatureMismatch {
            name: signature.fn_name.clone(),
            arg_types: format!(
                "did you mean one of these?\n{}",
                fns.iter()
                    .map(|s| format!("  {}", s))
                    .collect::<Vec<String>>()
                    .join("\n")
            ),
            span: error_span,
        });
    }

    if let Some(fun) = get_fn_by_name(&context.program, &signature.fn_name) {
        // Validate that the call signature matches the function definition
        if !signature_matches(&fun.signature, &signature) {
            // Try to find which argument has the wrong type
            let error_span = find_mismatched_arg_index(&fun.signature, &signature)
                .and_then(|i| arg_exprs.get(i))
                .map(|expr| expr.span().to_source_span())
                .unwrap_or_else(|| call_span.to_source_span());

            return Err(ShadyError::FunctionSignatureMismatch {
                name: signature.fn_name.clone(),
                arg_types: format!("expected signature: {}", fun.signature),
                span: error_span,
            });
        }
        return eval_local_fn(local_context, context, fun, &arguments);
    }

    let program = signature.fn_name;
    let args = arguments.iter().map(|a| a.to_string()).collect();
    Ok(Value::Proc(spawn(context, program, args)?))
}

fn spawn(context: &ShadyContext, program: String, args: Vec<String>) -> Result<Proc> {
    // Check process limit
    let mut count = context.process_count.borrow_mut();
    if *count >= context.limits.max_process_count {
        return Err(ShadyError::ProcessLimitExceeded(
            context.limits.max_process_count,
        ));
    }
    *count += 1;
    drop(count); // Release the borrow

    let mut command = std::process::Command::new(program.clone());
    command.args(args.clone());

    let (stdin_reader, stdin_writer) = os_pipe::pipe().map_err(ShadyError::ProcessError)?;
    command.stdin(stdin_reader);
    let (stdout_reader, stdout_writer) = os_pipe::pipe().map_err(ShadyError::ProcessError)?;
    command.stdout(stdout_writer);
    let (stderr_reader, stderr_writer) = os_pipe::pipe().map_err(ShadyError::ProcessError)?;
    command.stderr(stderr_writer);

    let child = command.spawn()?;

    Ok(Proc {
        child: Rc::new(RefCell::new(child)),
        program,
        args,
        stdin_writer,
        stdout_reader,
        stderr_reader,
    })
}

pub fn eval_local_fn(
    parent_context: &LocalContext,
    context: &ShadyContext,
    fun: &FnDefinition,
    args: &[Value],
) -> Result<Value> {
    let vars: HashMap<String, Value> = fun
        .signature
        .parameters
        .iter()
        .enumerate()
        .map(|(i, param)| {
            (
                param.name.clone(),
                args.get(i)
                    .cloned()
                    .unwrap_or_else(|| {
                        param.spec.default_value.clone()
                            .expect("BUG: missing argument but no default value - signature validation should prevent this")
                    }),
            )
        })
        .collect();

    let local_context = LocalContext {
        vars,
        depth: parent_context.depth + 1,
    };

    // Use return type as expected type if it's not Type::Any
    let expected_type = match &fun.signature.return_type {
        Type::Any => None,
        t => Some(t),
    };

    eval_expr_with_type(&local_context, context, &fun.expr, expected_type)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::parse_script;
    use crate::types::Type;

    fn eval_script(script: &str) -> Value {
        let program = parse_script(script).expect("parse failed");
        let context = build_context("test.shady".to_string(), script.to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").expect("main function not found");
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        eval_local_fn(&local_context, &context, fun, &[]).expect("evaluation failed")
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
        eval_block_simple: ("{ 42 }", Value::Int(42)),
        eval_block_returns_last: ("{ 1; 2; 3 }", Value::Int(3)),
        eval_block_with_computation: ("{ 1 + 1; 2 + 2 }", Value::Int(4)),
        // String interpolation tests
        eval_interpolation_simple: (r#""hello {1 + 1}""#, Value::Str("hello 2".to_string())),
        eval_interpolation_multiple: (r#""x={1}, y={2}""#, Value::Str("x=1, y=2".to_string())),
        eval_interpolation_with_string: (r#""hello {"world"}""#, Value::Str("hello world".to_string())),
        eval_interpolation_with_bool: (r#""value: {true}""#, Value::Str("value: true".to_string())),
        eval_interpolation_with_expression: (r#""result: {2 * 3 + 4}""#, Value::Str("result: 10".to_string())),
        eval_interpolation_empty: (r#""test""#, Value::Str("test".to_string())),
        eval_interpolation_only_expr: (r#""{42}""#, Value::Str("42".to_string())),
        eval_interpolation_adjacent: (r#""{1}{2}{3}""#, Value::Str("123".to_string())),
        eval_interpolation_with_comparison: (r#""is true: {1 == 1}""#, Value::Str("is true: true".to_string())),
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
    fn eval_default_value() {
        assert_eq!(
            eval_script(
                r"#
                    public main = val 10;
                    val $x: int (42) = $x;
                #"
            ),
            Value::Int(10),
        );

        assert_eq!(
            eval_script(
                r"#
                    public main = val;
                    val $x: int (42) = $x;
                #"
            ),
            Value::Int(42),
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

    // Empty list tests
    #[test]
    fn eval_empty_list_with_int_context() {
        // Empty list passed to function expecting [int]
        assert_eq!(
            eval_script(
                r"#
                    public main = identity_int [];
                    identity_int $x: [int] = $x;
                #"
            ),
            Value::List {
                inner_type: Type::Int,
                values: vec![],
            },
        );
    }

    #[test]
    fn eval_empty_list_with_str_context() {
        // Empty list passed to function expecting [str]
        assert_eq!(
            eval_script(
                r"#
                    public main = identity_str [];
                    identity_str $x: [str] = $x;
                #"
            ),
            Value::List {
                inner_type: Type::Str,
                values: vec![],
            },
        );
    }

    #[test]
    fn eval_empty_list_with_bool_context() {
        // Empty list passed to function expecting [bool]
        assert_eq!(
            eval_script(
                r"#
                    public main = identity_bool [];
                    identity_bool $x: [bool] = $x;
                #"
            ),
            Value::List {
                inner_type: Type::Bool,
                values: vec![],
            },
        );
    }

    #[test]
    fn eval_empty_list_concat_with_non_empty() {
        // Empty list concatenated with non-empty list via builtin
        assert_eq!(
            eval_script(
                r"#
                    public main = concat_lists [] [1; 2; 3];
                    concat_lists $x: [int] $y: [int] = $x + $y;
                #"
            ),
            Value::List {
                inner_type: Type::Int,
                values: vec![Value::Int(1), Value::Int(2), Value::Int(3)],
            },
        );
    }

    #[test]
    fn eval_empty_list_concat_both_empty() {
        // Both lists empty - should infer from function signature
        assert_eq!(
            eval_script(
                r"#
                    public main = concat_lists [] [];
                    concat_lists $x: [int] $y: [int] = $x + $y;
                #"
            ),
            Value::List {
                inner_type: Type::Int,
                values: vec![],
            },
        );
    }

    #[test]
    fn eval_empty_list_in_if_branches() {
        // Empty list in both branches of if expression
        assert_eq!(
            eval_script(
                r"#
                    public main = get_list true;
                    get_list $cond: bool -> [int] = if ($cond) [] else [];
                #"
            ),
            Value::List {
                inner_type: Type::Int,
                values: vec![],
            },
        );
    }

    #[test]
    fn eval_empty_list_without_context_fails() {
        // Empty list without any type context should still fail
        let script = "main = [];";
        let program = parse_script(script).unwrap();
        let context = build_context("test.shady".to_string(), script.to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::EmptyListNeedsType { .. } => {}
            e => panic!("Expected EmptyListNeedsType error, got {:?}", e),
        }
    }

    #[test]
    fn eval_nested_empty_list() {
        // Empty list inside a list (nested)
        assert_eq!(
            eval_script(
                r"#
                    public main = identity_nested [[]];
                    identity_nested $x: [[int]] = $x;
                #"
            ),
            Value::List {
                inner_type: Type::List(Box::new(Type::Int)),
                values: vec![Value::List {
                    inner_type: Type::Int,
                    values: vec![],
                }],
            },
        );
    }

    // Error case tests
    #[test]
    fn eval_undefined_variable_fails() {
        let script = "main = $foo;";
        let program = parse_script(script).unwrap();
        let context = build_context("test.shady".to_string(), script.to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::VariableNotFound { name, .. } => assert_eq!(name, "foo"),
            e => panic!("Expected VariableNotFound error, got {:?}", e),
        }
    }

    #[test]
    fn eval_type_mismatch_in_if_condition_fails() {
        let script = "main = if (42) 1 else 2;";
        let program = parse_script(script).unwrap();
        let context = build_context("test.shady".to_string(), script.to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::TypeMismatch {
                expected, actual, ..
            } => {
                assert_eq!(expected, "bool");
                assert!(actual.contains("int") || actual.contains("Int"));
            }
            e => panic!("Expected TypeMismatch error, got {:?}", e),
        }
    }

    #[test]
    fn eval_mixed_types_in_list_fails() {
        let script = r"#
                main = mixed_list;
                mixed_list = [1; true];
            #";
        let program = parse_script(script).unwrap();
        let context = build_context("test.shady".to_string(), script.to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::TypeMismatch { .. } => {} // Expected
            e => panic!(
                "Expected TypeMismatch error for mixed list types, got {:?}",
                e
            ),
        }
    }

    #[test]
    fn eval_builtin_signature_mismatch_gives_helpful_error() {
        // Test that calling a builtin with wrong types gives helpful error
        let script = "main = 1 + true;";
        let program = parse_script(script).unwrap();
        let context = build_context("test.shady".to_string(), script.to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        // The + operator exists for (int, int) but we're passing (int, bool)
        match result.unwrap_err() {
            ShadyError::FunctionSignatureMismatch {
                name, arg_types, ..
            } => {
                assert_eq!(name, "+");
                assert!(arg_types.contains("did you mean one of these?"));
            }
            e => panic!("Expected FunctionSignatureMismatch error, got {:?}", e),
        }
    }

    #[test]
    fn eval_deeply_nested_expressions() {
        // Test that deeply nested expressions work correctly
        assert_eq!(
            eval_script("main = (((((1 + 2) * 3) - 4) / 5) ^ 2);"),
            Value::Int(1), // ((((3) * 3) - 4) / 5) ^ 2 = ((9 - 4) / 5) ^ 2 = (5 / 5) ^ 2 = 1 ^ 2 = 1
        );
    }

    #[test]
    fn eval_user_function_signature_mismatch_fails() {
        // Test that user-defined functions validate signatures at call time
        let script = r"#
                main = custom_add 1 true;
                custom_add $a: int $b: int = $a + $b;
            #";
        let program = parse_script(script).unwrap();
        let context = build_context("test.shady".to_string(), script.to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        // The function expects (int, int) but we're calling with (int, bool)
        // This should now be caught at call time, not during body evaluation
        match result.unwrap_err() {
            ShadyError::FunctionSignatureMismatch {
                name, arg_types, ..
            } => {
                assert_eq!(name, "custom_add");
                assert!(arg_types.contains("expected signature"));
            }
            e => panic!("Expected FunctionSignatureMismatch error, got {:?}", e),
        }
    }

    #[test]
    fn eval_user_function_wrong_arg_count_fails() {
        // Test that calling with wrong number of arguments fails
        let script = r"#
                main = takes_two 1;
                takes_two $a: int $b: int = $a + $b;
            #";
        let program = parse_script(script).unwrap();
        let context = build_context("test.shady".to_string(), script.to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::FunctionSignatureMismatch { name, .. } => {
                assert_eq!(name, "takes_two");
            }
            e => panic!(
                "Expected FunctionSignatureMismatch error for wrong arg count, got {:?}",
                e
            ),
        }
    }

    #[test]
    fn eval_user_function_with_defaults_still_works() {
        // Test that functions with default values still work correctly
        assert_eq!(
            eval_script(
                r"#
                    public main = add_with_default 5;
                    add_with_default $a: int $b: int (10) = $a + $b;
                #"
            ),
            Value::Int(15),
        );
    }

    // Resource limit tests
    #[test]
    fn eval_recursion_limit_exceeded() {
        // Test that recursion depth limit is enforced
        let script = r"#
                public main = infinite 0;
                infinite $x: int = infinite ($x + 1);
            #";
        let program = parse_script(script).unwrap();

        // Set a low recursion limit for testing
        let limits = ResourceLimits {
            max_recursion_depth: 10,
            max_process_count: 100,
        };
        let context = build_context_with_limits("test.shady".to_string(), program, limits);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::RecursionLimitExceeded(limit) => {
                assert_eq!(limit, 10);
            }
            e => panic!("Expected RecursionLimitExceeded error, got {:?}", e),
        }
    }

    #[test]
    fn eval_process_limit_exceeded() {
        // Test that process count limit is enforced
        let script = r"#
                public main = spawn_many 0;
                spawn_many $x: int = if ($x < 20)
                    (spawn_many ($x + 1)) + (exec (echo test))
                    else 0;
            #";
        let program = parse_script(script).unwrap();

        // Set a low process limit for testing
        let limits = ResourceLimits {
            max_recursion_depth: 1000,
            max_process_count: 5,
        };
        let context = build_context_with_limits("test.shady".to_string(), program, limits);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::ProcessLimitExceeded(limit) => {
                assert_eq!(limit, 5);
            }
            e => panic!("Expected ProcessLimitExceeded error, got {:?}", e),
        }
    }

    #[test]
    fn eval_recursion_within_limit_works() {
        // Test that recursion within the limit works fine
        let script = r"#
                public main = countdown 3;
                countdown $x: int = if ($x > 0) countdown ($x - 1) else 0;
            #";
        let program = parse_script(script).unwrap();

        // Set a limit that should be sufficient for countdown(3)
        // Need enough for: main call + countdown(3) + countdown(2) + countdown(1) + countdown(0)
        // Plus each if/else adds depth
        let limits = ResourceLimits {
            max_recursion_depth: 50,
            max_process_count: 100,
        };
        let context = build_context_with_limits("test.shady".to_string(), program, limits);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(0));
    }

    // Tests for improved error span reporting (Priority 2)
    #[test]
    fn eval_builtin_type_error_points_to_wrong_argument() {
        // Test that builtin type errors point to the specific wrong argument
        let script = "main = 1 + true;";
        let program = parse_script(script).unwrap();
        let context = build_context("test.shady".to_string(), script.to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::FunctionSignatureMismatch { name, span, .. } => {
                assert_eq!(name, "+");
                // The span should point to 'true' (offset 11, length 4)
                // "main = 1 + true;"
                //  0123456789012345
                //            ^^^^
                assert_eq!(span.offset(), 11);
                assert_eq!(span.len(), 4);
            }
            e => panic!("Expected FunctionSignatureMismatch error, got {:?}", e),
        }
    }

    #[test]
    fn eval_user_function_type_error_points_to_wrong_argument() {
        // Test that user function type errors point to the specific wrong argument
        let script = r#"
                main = takes_ints 42 "hello";
                takes_ints $a: int $b: int = $a + $b;
            "#;
        let program = parse_script(script).unwrap();
        let context = build_context("test.shady".to_string(), script.to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::FunctionSignatureMismatch { name, span, .. } => {
                assert_eq!(name, "takes_ints");
                // The span should point to "hello", not the entire call
                // Verify offset is after "42 " and length is for "hello" (7 chars including quotes)
                assert_eq!(span.len(), 7); // length of "hello"
            }
            e => panic!("Expected FunctionSignatureMismatch error, got {:?}", e),
        }
    }

    #[test]
    fn eval_first_wrong_argument_is_identified() {
        // Test that when multiple arguments are wrong, we report the first one
        let script = r#"
                main = takes_ints "wrong" true;
                takes_ints $a: int $b: int = $a + $b;
            "#;
        let program = parse_script(script).unwrap();
        let context = build_context("test.shady".to_string(), script.to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::FunctionSignatureMismatch { name, span, .. } => {
                assert_eq!(name, "takes_ints");
                // The span should point to "wrong" (first wrong arg), not "true"
                // Length should be 7 for "wrong"
                assert_eq!(span.len(), 7);
            }
            e => panic!("Expected FunctionSignatureMismatch error, got {:?}", e),
        }
    }

    // Block expression tests
    #[test]
    fn eval_block_auto_execs_intermediate_procs() {
        // Test that blocks automatically execute intermediate Proc values
        // The echo commands should execute, and the block returns 42
        let script = "public main = { echo test1; echo test2; 42 };";
        let program = parse_script(script).unwrap();
        let context = build_context("test.shady".to_string(), script.to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        // Should successfully execute both echo commands and return 42
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(42));
    }

    #[test]
    fn eval_block_empty() {
        // Test that empty blocks return 0
        let script = "public main = { };";
        let program = parse_script(script).unwrap();
        let context = build_context("test.shady".to_string(), script.to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(0));
    }

    #[test]
    fn eval_function_returns_proc_without_executing() {
        // Test that functions can return Proc values WITHOUT executing them immediately
        // Verify that the Proc is only created, not executed, when the function returns
        let script = r#"
            public main = check_proc_type (make_greeting test);
            make_greeting $name -> proc = echo $name;
            check_proc_type $p: proc -> str = "got proc";
        "#;
        let program = parse_script(script).unwrap();
        let context = build_context("test.shady".to_string(), script.to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        // If lazy evaluation works, make_greeting should return a Proc value
        // and check_proc_type should receive it and return "got proc"
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Str("got proc".to_string()));
    }

    #[test]
    fn eval_proc_executes_when_called_not_when_created() {
        // Test that Procs execute only when exec() is called, not when created
        let script = r#"
            public main = run_twice (make_greeting test);
            make_greeting $name -> proc = echo $name;
            run_twice $cmd: proc = {
                exec $cmd;
                exec $cmd;
                42
            };
        "#;
        let program = parse_script(script).unwrap();
        let context = build_context("test.shady".to_string(), script.to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        // Should successfully execute echo twice and return 42
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(42));
    }

    // Lambda tests - tested through map/filter/reduce and helper functions
    #[test]
    fn eval_simple_lambda_with_map() {
        // Test basic lambda with map
        assert_eq!(
            eval_script(
                r#"
                public main = first (map (lambda $x -> $x + 1) [5]);
            "#
            ),
            Value::Int(6),
        );
    }

    #[test]
    fn eval_lambda_with_keyword() {
        // Test using 'lambda' keyword instead of lambda
        assert_eq!(
            eval_script(
                r#"
                public main = first (map (lambda $x -> $x * 2) [21]);
            "#
            ),
            Value::Int(42),
        );
    }

    #[test]
    fn eval_lambda_with_type_annotations() {
        // Test lambda with explicit type annotations
        assert_eq!(
            eval_script(
                r#"
                public main = first (map (lambda $x: int -> int = $x + 1) [41]);
            "#
            ),
            Value::Int(42),
        );
    }

    #[test]
    fn eval_lambda_with_closure() {
        // Test that lambdas capture variables from their environment
        assert_eq!(
            eval_script(
                r#"
                public main = make_adder_list 10 [32];
                make_adder_list $y: int $list: [int] = map (lambda $a -> $a + $y) $list;
            "#
            ),
            Value::List {
                inner_type: Type::Int,
                values: vec![Value::Int(42)],
            },
        );
    }

    #[test]
    fn eval_lambda_multi_param_with_reduce() {
        // Test lambda with multiple parameters using reduce
        assert_eq!(
            eval_script(
                r#"
                public main = reduce (lambda $x $y -> $x + $y) 10 [32];
            "#
            ),
            Value::Int(42),
        );
    }

    #[test]
    fn eval_map_simple() {
        // Test map with simple lambda
        assert_eq!(
            eval_script(
                r#"
                public main = map (lambda $x -> $x * 2) [1; 2; 3];
            "#
            ),
            Value::List {
                inner_type: Type::Int,
                values: vec![Value::Int(2), Value::Int(4), Value::Int(6)],
            },
        );
    }

    #[test]
    fn eval_map_empty_list() {
        // Test map on empty list - type inference from lambda return type
        assert_eq!(
            eval_script(
                r#"
                public main = identity_list (map (lambda $x: int -> int = $x * 2) (get_empty_list 0));
                identity_list $xs: [int] = $xs;
                get_empty_list $dummy: int -> [int] = [];
            "#
            ),
            Value::List {
                inner_type: Type::Int,
                values: vec![],
            },
        );
    }

    #[test]
    fn eval_map_with_closure() {
        // Test map with lambda that captures variable
        assert_eq!(
            eval_script(
                r#"
                public main = map_with_offset 10 [1; 2; 3];
                map_with_offset $offset: int $list: [int] = map (lambda $x -> $x + $offset) $list;
            "#
            ),
            Value::List {
                inner_type: Type::Int,
                values: vec![Value::Int(11), Value::Int(12), Value::Int(13)],
            },
        );
    }

    #[test]
    fn eval_filter_simple() {
        // Test filter with simple predicate
        assert_eq!(
            eval_script(
                r#"
                public main = filter (lambda $x -> $x > 2) [1; 2; 3; 4; 5];
            "#
            ),
            Value::List {
                inner_type: Type::Int,
                values: vec![Value::Int(3), Value::Int(4), Value::Int(5)],
            },
        );
    }

    #[test]
    fn eval_filter_empty_result() {
        // Test filter that removes all elements
        assert_eq!(
            eval_script(
                r#"
                public main = filter (lambda $x -> $x > 10) [1; 2; 3];
            "#
            ),
            Value::List {
                inner_type: Type::Int,
                values: vec![],
            },
        );
    }

    #[test]
    fn eval_filter_with_closure() {
        // Test filter with lambda that captures variable
        assert_eq!(
            eval_script(
                r#"
                public main = filter_above 3 [1; 2; 3; 4; 5];
                filter_above $threshold: int $list: [int] = filter (lambda $x -> $x > $threshold) $list;
            "#
            ),
            Value::List {
                inner_type: Type::Int,
                values: vec![Value::Int(4), Value::Int(5)],
            },
        );
    }

    #[test]
    fn eval_reduce_simple() {
        // Test reduce to sum a list
        assert_eq!(
            eval_script(
                r#"
                public main = reduce (lambda $acc $x -> $acc + $x) 0 [1; 2; 3; 4];
            "#
            ),
            Value::Int(10),
        );
    }

    #[test]
    fn eval_reduce_product() {
        // Test reduce to calculate product
        assert_eq!(
            eval_script(
                r#"
                public main = reduce (lambda $acc $x -> $acc * $x) 1 [2; 3; 4];
            "#
            ),
            Value::Int(24),
        );
    }

    #[test]
    fn eval_reduce_empty_list() {
        // Test reduce on empty list returns initial value
        assert_eq!(
            eval_script(
                r#"
                public main = reduce (lambda $acc $x -> $acc + $x) 42 (empty_list []);
                empty_list $xs: [int] = $xs;
            "#
            ),
            Value::Int(42),
        );
    }

    #[test]
    fn eval_reduce_with_closure() {
        // Test reduce with lambda that captures variable
        assert_eq!(
            eval_script(
                r#"
                public main = reduce_with_mult 2 [1; 2; 3];
                reduce_with_mult $multiplier: int $list: [int] = reduce (lambda $acc $x -> $acc + ($x * $multiplier)) 0 $list;
            "#
            ),
            Value::Int(12), // (0 + 1*2) + (2*2) + (3*2) = 2 + 4 + 6 = 12
        );
    }

    #[test]
    fn eval_nested_lambdas() {
        // Test lambda returning another lambda (currying) via map
        // The outer lambda captures nothing, the inner lambda captures $a
        assert_eq!(
            eval_script(
                r#"
                public main = first (map (lambda $a: int -> first (map (lambda $b: int -> $a + $b) [37])) [5]);
            "#
            ),
            Value::Int(42),
        );
    }

    #[test]
    fn eval_chained_higher_order_functions() {
        // Test combining map, filter, and reduce
        assert_eq!(
            eval_script(
                r#"
                public main = process_list [1; 2; 3; 4; 5];
                process_list $nums: [int] = reduce (lambda $acc: int $x: int -> $acc + $x) 0 (filter (lambda $x: int -> $x > 4) (map (lambda $x: int -> $x * 2) $nums));
            "#
            ),
            Value::Int(24), // [1,2,3,4,5] -> map(*2) -> [2,4,6,8,10] -> filter(>4) -> [6,8,10] -> reduce(+) -> 24
        );
    }

    #[test]
    fn eval_lambda_passed_through_function() {
        // Test passing lambda through a function that applies it twice
        assert_eq!(
            eval_script(
                r#"
                public main = first (map (lambda $x: int -> $x + 1) (map (lambda $x: int -> $x + 1) [40]));
            "#
            ),
            Value::Int(42),
        );
    }

    #[test]
    fn eval_map_str() {
        // Test map on string list (type checking)
        let script = r#"
            public main = map (lambda $s -> $s + "!") ["hello"; "world"];
        "#;
        assert_eq!(
            eval_script(script),
            Value::List {
                inner_type: Type::Str,
                values: vec![
                    Value::Str("hello!".to_string()),
                    Value::Str("world!".to_string()),
                ],
            },
        );
    }

    // Error case tests for lambdas
    #[test]
    fn eval_map_wrong_param_count() {
        // Test that map rejects lambda with wrong number of parameters
        let script = r#"
            public main = map (lambda $x $y -> $x + $y) [1; 2; 3];
        "#;
        let program = parse_script(script).unwrap();
        let context = build_context("test.shady".to_string(), script.to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::FunctionSignatureMismatch { name, .. } => {
                assert_eq!(name, "map");
            }
            e => panic!("Expected FunctionSignatureMismatch error, got {:?}", e),
        }
    }

    #[test]
    fn eval_filter_wrong_param_count() {
        // Test that filter rejects lambda with wrong number of parameters
        let script = r#"
            public main = filter (lambda $x $y -> $x > $y) [1; 2; 3];
        "#;
        let program = parse_script(script).unwrap();
        let context = build_context("test.shady".to_string(), script.to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::FunctionSignatureMismatch { name, .. } => {
                assert_eq!(name, "filter");
            }
            e => panic!("Expected FunctionSignatureMismatch error, got {:?}", e),
        }
    }

    #[test]
    fn eval_reduce_wrong_param_count() {
        // Test that reduce rejects lambda with wrong number of parameters
        let script = r#"
            public main = reduce (lambda $x -> $x + 1) 0 [1; 2; 3];
        "#;
        let program = parse_script(script).unwrap();
        let context = build_context("test.shady".to_string(), script.to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::FunctionSignatureMismatch { name, .. } => {
                assert_eq!(name, "reduce");
            }
            e => panic!("Expected FunctionSignatureMismatch error, got {:?}", e),
        }
    }

    #[test]
    fn eval_lambda_call_wrong_arg_count_via_reduce() {
        // Test that calling lambda with wrong number of args fails
        // reduce expects a 2-param lambda, we give it a 1-param lambda
        let script = r#"
            public main = reduce (lambda $x -> $x + 1) 0 [1; 2; 3];
        "#;
        let program = parse_script(script).unwrap();
        let context = build_context("test.shady".to_string(), script.to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::FunctionSignatureMismatch { name, .. } => {
                assert_eq!(name, "reduce");
            }
            e => panic!("Expected FunctionSignatureMismatch error, got {:?}", e),
        }
    }
}
