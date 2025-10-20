use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{
    get_fn_by_name, Expr, FnDefinition, FnSignature, ParamSpec, Parameter, ProgramAST,
};
use crate::builtins;
use crate::error::{Result, ShadyError};
use crate::types::{Proc, Type, Value};

pub type BuiltinIndex = HashMap<FnSignature, Box<dyn Fn(Vec<Value>) -> Result<Value>>>;

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

fn get_builtin_fn<'a>(
    context: &'a ShadyContext,
    signature: &'a FnSignature,
) -> Option<&'a dyn Fn(Vec<Value>) -> Result<Value>> {
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

/// Check if a call signature matches a function definition signature
fn signature_matches(definition: &FnSignature, call: &FnSignature) -> bool {
    // Check that we have the right number of arguments
    // (accounting for default values)
    let required_params = definition.parameters.iter()
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

pub struct ShadyContext {
    pub filename: String,
    pub program: ProgramAST,
    builtins: BuiltinIndex,
    pub limits: ResourceLimits,
    process_count: RefCell<usize>,
}

#[derive(Debug)]
pub struct LocalContext {
    pub vars: HashMap<String, Value>,
    pub depth: usize,
}

pub fn build_context(filename: String, program: ProgramAST) -> ShadyContext {
    build_context_with_limits(filename, program, ResourceLimits::default())
}

pub fn build_context_with_limits(
    filename: String,
    program: ProgramAST,
    limits: ResourceLimits,
) -> ShadyContext {
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
        return Err(ShadyError::RecursionLimitExceeded(context.limits.max_recursion_depth));
    }

    match expr {
        Expr::Value(value) => Ok(value.clone()),
        Expr::Variable(var_name) => {
            local_context.vars.get(var_name)
                .cloned()
                .ok_or_else(|| ShadyError::VariableNotFound(var_name.clone()))
        }
        Expr::Call { .. } => eval_fn(local_context, context, expr),
        Expr::If {
            condition,
            when_true,
            when_false,
        } => {
            let cond_val = eval_expr_with_type(local_context, context, condition, Some(&Type::Bool))?;
            match cond_val {
                Value::Bool(true) => eval_expr_with_type(local_context, context, when_true, expected_type),
                Value::Bool(false) => eval_expr_with_type(local_context, context, when_false, expected_type),
                _ => Err(ShadyError::TypeMismatch {
                    expected: "bool".to_string(),
                    actual: format!("{:?}", cond_val.get_type()),
                }),
            }
        }
        Expr::List { elements } => {
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
                        .ok_or(ShadyError::EmptyListNeedsType)?
                }
                _ => values[0].get_type(),
            };

            if !values.iter().all(|v| v.get_type() == inner_type) {
                return Err(ShadyError::TypeMismatch {
                    expected: format!("[{}]", inner_type),
                    actual: "mixed types in list".to_string(),
                });
            }
            Ok(Value::List { inner_type, values })
        }
    }
}

fn eval_fn(local_context: &LocalContext, context: &ShadyContext, expr: &Expr) -> Result<Value> {
    let (fn_name, arg_exprs, is_infix) = match expr {
        Expr::Call { fn_name, arguments, is_infix } => (fn_name, arguments, *is_infix),
        _ => return Err(ShadyError::EvalError("not a call".to_string())),
    };

    // Try to find the function to get parameter types
    let param_types: Option<Vec<Type>> = {
        // Check if it's a user-defined function
        if let Some(fun) = get_fn_by_name(&context.program, fn_name) {
            Some(fun.signature.parameters.iter().map(|p| p.typ.clone()).collect())
        } else {
            // Check builtins by name to get possible signatures
            if let Some(builtin_sigs) = get_builtins_by_name(context, fn_name) {
                // For now, just take the first signature's parameter types
                // In the future, we could be smarter about overload resolution
                builtin_sigs.first().map(|sig| {
                    sig.parameters.iter().map(|p| p.typ.clone()).collect()
                })
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

    if let Some(builtin_fn) = get_builtin_fn(context, &signature) {
        return builtin_fn(arguments);
    }

    if let Some(fns) = get_builtins_by_name(context, &signature.fn_name) {
        return Err(ShadyError::FunctionSignatureMismatch {
            name: signature.fn_name.clone(),
            arg_types: format!(
                "did you mean one of these?\n{}",
                fns.iter()
                    .map(|s| format!("  {}", s))
                    .collect::<Vec<String>>()
                    .join("\n")
            ),
        });
    }

    if let Some(fun) = get_fn_by_name(&context.program, &signature.fn_name) {
        // Validate that the call signature matches the function definition
        if !signature_matches(&fun.signature, &signature) {
            return Err(ShadyError::FunctionSignatureMismatch {
                name: signature.fn_name.clone(),
                arg_types: format!("expected signature: {}", fun.signature),
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
        return Err(ShadyError::ProcessLimitExceeded(context.limits.max_process_count));
    }
    *count += 1;
    drop(count); // Release the borrow

    let mut command = std::process::Command::new(program.clone());
    command.args(args.clone());

    let (stdin_reader, stdin_writer) = os_pipe::pipe()
        .map_err(|e| ShadyError::ProcessError(e))?;
    command.stdin(stdin_reader);
    let (stdout_reader, stdout_writer) = os_pipe::pipe()
        .map_err(|e| ShadyError::ProcessError(e))?;
    command.stdout(stdout_writer);
    let (stderr_reader, stderr_writer) = os_pipe::pipe()
        .map_err(|e| ShadyError::ProcessError(e))?;
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
                    .unwrap_or_else(|| param.spec.default_value.clone().unwrap()),
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
        let context = build_context("test.shady".to_string(), program);
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
        let program = parse_script("main = [];").unwrap();
        let context = build_context("test.shady".to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::EmptyListNeedsType => {},
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
        let program = parse_script("main = $foo;").unwrap();
        let context = build_context("test.shady".to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::VariableNotFound(name) => assert_eq!(name, "foo"),
            e => panic!("Expected VariableNotFound error, got {:?}", e),
        }
    }

    #[test]
    fn eval_type_mismatch_in_if_condition_fails() {
        let program = parse_script("main = if (42) 1 else 2;").unwrap();
        let context = build_context("test.shady".to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::TypeMismatch { expected, actual } => {
                assert_eq!(expected, "bool");
                assert!(actual.contains("int") || actual.contains("Int"));
            }
            e => panic!("Expected TypeMismatch error, got {:?}", e),
        }
    }

    #[test]
    fn eval_mixed_types_in_list_fails() {
        let program = parse_script(
            r"#
                main = mixed_list;
                mixed_list = [1; true];
            #"
        ).unwrap();
        let context = build_context("test.shady".to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::TypeMismatch { .. } => {}, // Expected
            e => panic!("Expected TypeMismatch error for mixed list types, got {:?}", e),
        }
    }

    #[test]
    fn eval_builtin_signature_mismatch_gives_helpful_error() {
        // Test that calling a builtin with wrong types gives helpful error
        let program = parse_script("main = 1 + true;").unwrap();
        let context = build_context("test.shady".to_string(), program);
        let fun = get_fn_by_name(&context.program, "main").unwrap();
        let local_context = LocalContext {
            vars: HashMap::new(),
            depth: 0,
        };
        let result = eval_local_fn(&local_context, &context, fun, &[]);

        assert!(result.is_err());
        // The + operator exists for (int, int) but we're passing (int, bool)
        match result.unwrap_err() {
            ShadyError::FunctionSignatureMismatch { name, arg_types } => {
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
        let program = parse_script(
            r"#
                main = custom_add 1 true;
                custom_add $a: int $b: int = $a + $b;
            #"
        ).unwrap();
        let context = build_context("test.shady".to_string(), program);
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
            ShadyError::FunctionSignatureMismatch { name, arg_types } => {
                assert_eq!(name, "custom_add");
                assert!(arg_types.contains("expected signature"));
            }
            e => panic!("Expected FunctionSignatureMismatch error, got {:?}", e),
        }
    }

    #[test]
    fn eval_user_function_wrong_arg_count_fails() {
        // Test that calling with wrong number of arguments fails
        let program = parse_script(
            r"#
                main = takes_two 1;
                takes_two $a: int $b: int = $a + $b;
            #"
        ).unwrap();
        let context = build_context("test.shady".to_string(), program);
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
            e => panic!("Expected FunctionSignatureMismatch error for wrong arg count, got {:?}", e),
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
        let program = parse_script(
            r"#
                public main = infinite 0;
                infinite $x: int = infinite ($x + 1);
            #"
        ).unwrap();

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
        let program = parse_script(
            r"#
                public main = spawn_many 0;
                spawn_many $x: int = if ($x < 20)
                    (spawn_many ($x + 1)) + (exec (echo test))
                    else 0;
            #"
        ).unwrap();

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
        let program = parse_script(
            r"#
                public main = countdown 3;
                countdown $x: int = if ($x > 0) countdown ($x - 1) else 0;
            #"
        ).unwrap();

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
}
