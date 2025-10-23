use crate::ast::{Expr, FnDefinition, FnSignature, Parameter, ParamSpec, ProgramAST, StringSegment};
use crate::error::{Result, ShadyError};
use crate::eval::BuiltinIndex;
use crate::types::Type;
use std::collections::HashMap;

/// Type environment mapping variable names to their types
#[derive(Debug, Clone)]
pub struct TypeEnv {
    vars: HashMap<String, Type>,
}

impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv {
            vars: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: String, typ: Type) {
        self.vars.insert(name, typ);
    }

    pub fn get(&self, name: &str) -> Option<&Type> {
        self.vars.get(name)
    }
}

/// Type checker for Shady programs
pub struct TypeChecker<'a> {
    /// User-defined function signatures
    functions: HashMap<String, &'a FnSignature>,
    /// Builtin function signatures
    builtins: &'a BuiltinIndex,
}

impl<'a> TypeChecker<'a> {
    pub fn new(program: &'a ProgramAST, builtins: &'a BuiltinIndex) -> Self {
        let mut functions = HashMap::new();
        for fun in &program.fn_definitions {
            functions.insert(fun.signature.fn_name.clone(), &fun.signature);
        }

        TypeChecker {
            functions,
            builtins,
        }
    }

    /// Type check an entire program
    pub fn typecheck_program(&self, program: &'a ProgramAST) -> Result<()> {
        for fun in &program.fn_definitions {
            self.typecheck_function(fun)?;
        }
        Ok(())
    }

    /// Type check a single function definition
    fn typecheck_function(&self, fun: &FnDefinition) -> Result<()> {
        // Build type environment from parameters
        let mut env = TypeEnv::new();
        for param in &fun.signature.parameters {
            env.insert(param.name.clone(), param.typ.clone());
        }

        // Infer the type of the function body
        let body_type = self.infer_expr_type(&env, &fun.expr)?;

        // Check that body type matches declared return type (if not Any)
        // Note: Can't use != because Type::Any equals everything in PartialEq
        if !matches!(fun.signature.return_type, Type::Any) {
            if !self.types_compatible(&body_type, &fun.signature.return_type) {
                return Err(ShadyError::TypeMismatch {
                    expected: fun.signature.return_type.to_string(),
                    actual: body_type.to_string(),
                    span: fun.expr.span().to_source_span(),
                });
            }
        }

        Ok(())
    }

    /// Infer the type of an expression
    fn infer_expr_type(&self, env: &TypeEnv, expr: &Expr) -> Result<Type> {
        match expr {
            Expr::Value(val, _) => Ok(val.get_type()),

            Expr::Variable(name, span) => {
                env.get(name).cloned().ok_or_else(|| {
                    ShadyError::VariableNotFound {
                        name: name.clone(),
                        span: span.to_source_span(),
                    }
                })
            }

            Expr::Call {
                fn_name,
                arguments,
                is_infix,
                span,
            } => {
                // Infer types of all arguments
                let arg_types: Result<Vec<Type>> = arguments
                    .iter()
                    .map(|arg| self.infer_expr_type(env, arg))
                    .collect();
                let arg_types = arg_types?;

                // Find matching function signature
                let signature = self.find_function_signature(fn_name, &arg_types, *is_infix)?;

                // If no exact match, try to find close match for better error
                if signature.is_none() {
                    // Check if function exists with different signature
                    if let Some(candidates) = self.find_function_candidates(fn_name, *is_infix) {
                        return Err(ShadyError::FunctionSignatureMismatch {
                            name: fn_name.clone(),
                            arg_types: format!(
                                "expected one of:\n{}",
                                candidates
                                    .iter()
                                    .map(|sig| format!("  {}", sig))
                                    .collect::<Vec<_>>()
                                    .join("\n")
                            ),
                            span: span.to_source_span(),
                        });
                    }

                    // Assume it's an external command - returns Proc
                    return Ok(Type::Proc);
                }

                Ok(signature.unwrap().return_type.clone())
            }

            Expr::If {
                condition,
                when_true,
                when_false,
                span,
            } => {
                // Condition must be bool
                let cond_type = self.infer_expr_type(env, condition)?;
                if !self.types_compatible(&cond_type, &Type::Bool) {
                    return Err(ShadyError::TypeMismatch {
                        expected: "bool".to_string(),
                        actual: cond_type.to_string(),
                        span: condition.span().to_source_span(),
                    });
                }

                // Both branches must have compatible types
                let true_type = self.infer_expr_type(env, when_true)?;
                let false_type = self.infer_expr_type(env, when_false)?;

                // Try to unify the types
                self.unify_types(&true_type, &false_type, span)
            }

            Expr::List { elements, span } => {
                if elements.is_empty() {
                    // Empty list - cannot infer type statically
                    return Err(ShadyError::EmptyListNeedsType {
                        span: span.to_source_span(),
                    });
                }

                // Infer type from first element
                let first_type = self.infer_expr_type(env, &elements[0])?;

                // Check all elements have same type
                for (i, elem) in elements.iter().enumerate().skip(1) {
                    let elem_type = self.infer_expr_type(env, elem)?;
                    if !self.types_compatible(&elem_type, &first_type) {
                        return Err(ShadyError::TypeMismatch {
                            expected: format!("list of {}", first_type),
                            actual: format!("element {} has type {}", i, elem_type),
                            span: elem.span().to_source_span(),
                        });
                    }
                }

                Ok(Type::List(Box::new(first_type)))
            }

            Expr::Block { expressions, .. } => {
                if expressions.is_empty() {
                    // Empty block returns int (0)
                    return Ok(Type::Int);
                }

                // Type is the type of the last expression
                let last = expressions.last().unwrap();
                self.infer_expr_type(env, last)
            }

            Expr::InterpolatedString { segments, .. } => {
                // Verify all interpolated expressions are valid
                for segment in segments {
                    if let StringSegment::Interpolated(expr) = segment {
                        // We don't care about the type - to_string works on all types
                        self.infer_expr_type(env, expr)?;
                    }
                }
                // String interpolation always returns string
                Ok(Type::Str)
            }
        }
    }

    /// Find a function signature that matches the given arguments
    fn find_function_signature(
        &self,
        name: &str,
        arg_types: &[Type],
        is_infix: bool,
    ) -> Result<Option<&FnSignature>> {
        // Build a temporary signature for matching
        let call_sig = FnSignature {
            fn_name: name.to_string(),
            parameters: arg_types
                .iter()
                .map(|t| Parameter {
                    name: String::new(),
                    typ: t.clone(),
                    spec: ParamSpec::default(),
                })
                .collect(),
            is_public: true,
            is_infix,
            return_type: Type::Any,
        };

        // Check builtins first
        if let Some(builtin_sig) = self.builtins.get_key_value(&call_sig) {
            return Ok(Some(builtin_sig.0));
        }

        // Check user-defined functions
        if let Some(user_sig) = self.functions.get(name) {
            if self.signature_matches(user_sig, &call_sig) {
                return Ok(Some(user_sig));
            }
        }

        Ok(None)
    }

    /// Find all function signatures with the given name (for error messages)
    fn find_function_candidates(&self, name: &str, is_infix: bool) -> Option<Vec<&FnSignature>> {
        let mut candidates = Vec::new();

        // Check builtins
        for (sig, _) in self.builtins.iter() {
            if sig.fn_name == name && sig.is_infix == is_infix {
                candidates.push(sig);
            }
        }

        // Check user functions
        if let Some(sig) = self.functions.get(name) {
            if sig.is_infix == is_infix {
                candidates.push(*sig);
            }
        }

        if candidates.is_empty() {
            None
        } else {
            Some(candidates)
        }
    }

    /// Check if a call signature matches a function definition
    fn signature_matches(&self, definition: &FnSignature, call: &FnSignature) -> bool {
        // Must have same infix status
        if definition.is_infix != call.is_infix {
            return false;
        }

        // Check argument count (considering default values)
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
                if !self.types_compatible(&provided_param.typ, &expected_param.typ) {
                    return false;
                }
            } else {
                return false;
            }
        }

        true
    }

    /// Check if two types are compatible (accounting for Type::Any)
    fn types_compatible(&self, t1: &Type, t2: &Type) -> bool {
        match (t1, t2) {
            // Any matches everything
            (Type::Any, _) | (_, Type::Any) => true,
            // Lists must have compatible inner types
            (Type::List(a), Type::List(b)) => self.types_compatible(a, b),
            // Everything else must match exactly
            _ => t1 == t2,
        }
    }

    /// Unify two types, returning the more specific type
    fn unify_types(&self, t1: &Type, t2: &Type, span: &crate::ast::Span) -> Result<Type> {
        match (t1, t2) {
            // Any unifies with anything, returning the other type
            (Type::Any, t) | (t, Type::Any) => Ok(t.clone()),
            // Lists unify if their inner types unify
            (Type::List(a), Type::List(b)) => {
                let inner = self.unify_types(a, b, span)?;
                Ok(Type::List(Box::new(inner)))
            }
            // Same types unify
            (a, b) if a == b => Ok(a.clone()),
            // Otherwise, types are incompatible
            _ => Err(ShadyError::TypeMismatch {
                expected: t1.to_string(),
                actual: t2.to_string(),
                span: span.to_source_span(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::parse_script;
    use crate::builtins;
    use crate::eval::BuiltinIndex;

    fn typecheck(code: &str) -> Result<()> {
        let program = parse_script(code).expect("parse failed");
        let mut builtins: BuiltinIndex = HashMap::new();
        builtins::setup_builtins(&mut builtins);
        let checker = TypeChecker::new(&program, &builtins);
        checker.typecheck_program(&program)
    }

    #[test]
    fn test_simple_function_correct_type() {
        let result = typecheck("add $a: int $b: int -> int = $a + $b;");
        assert!(result.is_ok());
    }

    #[test]
    fn test_simple_function_wrong_return_type() {
        let result = typecheck("bad $a: int -> str = $a + 1;");
        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::TypeMismatch { expected, actual, .. } => {
                assert_eq!(expected, "str");
                assert_eq!(actual, "int");
            }
            e => panic!("Expected TypeMismatch, got {:?}", e),
        }
    }

    #[test]
    fn test_undefined_variable() {
        let result = typecheck("bad = $unknown;");
        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::VariableNotFound { name, .. } => {
                assert_eq!(name, "unknown");
            }
            e => panic!("Expected VariableNotFound, got {:?}", e),
        }
    }

    #[test]
    fn test_if_condition_must_be_bool() {
        let result = typecheck("bad = if (42) 1 else 2;");
        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::TypeMismatch { expected, .. } => {
                assert_eq!(expected, "bool");
            }
            e => panic!("Expected TypeMismatch for condition, got {:?}", e),
        }
    }

    #[test]
    fn test_if_branches_compatible_types() {
        // Same types - OK
        let result = typecheck("ok = if (true) 1 else 2;");
        assert!(result.is_ok());

        // Different types - Error
        let result = typecheck("bad = if (true) 1 else \"hello\";");
        assert!(result.is_err());
    }

    #[test]
    fn test_list_homogeneous() {
        let result = typecheck("ok = [1; 2; 3];");
        assert!(result.is_ok());

        let result = typecheck("bad = [1; true; 3];");
        assert!(result.is_err());
    }

    #[test]
    fn test_empty_list_requires_context() {
        // Empty list without type context should fail in static checking
        let result = typecheck("bad = [];");
        assert!(result.is_err());
        match result.unwrap_err() {
            ShadyError::EmptyListNeedsType { .. } => {}
            e => panic!("Expected EmptyListNeedsType, got {:?}", e),
        }
    }

    #[test]
    fn test_function_call_correct_types() {
        let result = typecheck(
            r#"
            helper $x: int -> int = $x + 1;
            main = helper 42;
        "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_function_call_wrong_types() {
        let result = typecheck(
            r#"
            helper $x: int -> int = $x + 1;
            main = helper "wrong";
        "#,
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_string_interpolation() {
        let result = typecheck(r#"ok = "hello {1 + 1}";"#);
        assert!(result.is_ok());

        let result = typecheck(r#"ok $name: str = "hello {$name}";"#);
        assert!(result.is_ok());
    }

    #[test]
    fn test_block_returns_last_expr() {
        let result = typecheck("ok -> int = { 1; 2; 3 };");
        assert!(result.is_ok());

        let result = typecheck("bad -> str = { 1; 2; 3 };");
        assert!(result.is_err());
    }

    #[test]
    fn test_builtin_type_checking() {
        // Correct builtin usage
        let result = typecheck("ok = 1 + 2;");
        assert!(result.is_ok());

        // Wrong builtin usage
        let result = typecheck("bad = 1 + true;");
        assert!(result.is_err());
    }

    #[test]
    fn test_external_command_assumed_proc() {
        // Unknown function assumed to be external command
        let result = typecheck("ok = echo hello;");
        assert!(result.is_ok());
    }

    #[test]
    fn test_nested_expressions() {
        let result = typecheck(
            r#"
            double $x: int -> int = $x * 2;
            main = double (double 5);
        "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_recursive_function() {
        let result = typecheck(
            r#"
            factorial $n: int -> int =
                if ($n <= 1)
                    1
                else
                    $n * (factorial ($n - 1));
        "#,
        );
        assert!(result.is_ok());
    }
}
