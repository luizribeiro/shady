use crate::types::Type;
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

/// Main error type for Shady interpreter
#[derive(Error, Debug, Diagnostic)]
pub enum ShadyError {
    #[error("Variable '{name}' not found")]
    #[diagnostic(
        code(shady::eval::variable_not_found),
        help("Did you forget to define this variable, or is there a typo?")
    )]
    VariableNotFound {
        name: String,
        #[label("undefined variable referenced here")]
        span: SourceSpan,
    },

    #[error("Type mismatch: expected {expected}, got {actual}")]
    #[diagnostic(
        code(shady::types::mismatch),
        help("Check that the types of your arguments match the function signature")
    )]
    TypeMismatch {
        expected: String,
        actual: String,
        #[label("type mismatch occurs here")]
        span: SourceSpan,
    },

    #[error("Parse error: {message}")]
    #[diagnostic(
        code(shady::parse::syntax),
        help("Check for missing semicolons, unmatched parentheses, or invalid syntax")
    )]
    ParseErrorSimple {
        message: String,
        #[label("syntax error here")]
        span: SourceSpan,
    },

    #[error("Function '{name}' not found")]
    #[diagnostic(
        code(shady::eval::function_not_found),
        help("Check that the function is defined or that you've spelled the name correctly")
    )]
    FunctionNotFound {
        name: String,
        #[label("function called here")]
        span: SourceSpan,
    },

    #[error("Function '{name}' found but no matching signature for argument types: {arg_types}")]
    #[diagnostic(
        code(shady::eval::signature_mismatch),
        help("Check the types of arguments you're passing to the function")
    )]
    FunctionSignatureMismatch {
        name: String,
        arg_types: String,
        #[label("function called here")]
        span: SourceSpan,
    },

    #[error("Process execution failed: {0}")]
    #[diagnostic(code(shady::process::exec_failed))]
    ProcessError(#[from] std::io::Error),

    #[error("I/O error: {0}")]
    #[diagnostic(code(shady::io::error))]
    IoError(String),

    #[error("Empty lists require type annotation or context to infer type")]
    #[diagnostic(
        code(shady::types::empty_list),
        help("Try providing a type annotation or using the list in a context where the type can be inferred")
    )]
    EmptyListNeedsType {
        #[label("empty list here")]
        span: SourceSpan,
    },

    #[error("Required parameter '{0}' cannot appear after optional parameters")]
    #[diagnostic(
        code(shady::parse::param_order),
        help("Move all required parameters before optional ones in the function signature")
    )]
    RequiredAfterOptional(String),

    #[error("Default value type mismatch for parameter '{param}': expected {expected:?}, got {actual:?}")]
    #[diagnostic(
        code(shady::parse::default_type_mismatch),
        help("Ensure the default value matches the parameter's type annotation")
    )]
    DefaultTypeMismatch {
        param: String,
        expected: Type,
        actual: Type,
    },

    #[error("Thread panicked during execution")]
    #[diagnostic(
        code(shady::runtime::thread_panic),
        help("This is likely a bug in shady. Please report it.")
    )]
    ThreadPanic,

    #[error("Invalid conversion from {from} to {to}")]
    #[diagnostic(code(shady::types::invalid_conversion))]
    InvalidConversion { from: String, to: String },

    #[error("Pipe clone failed: {0}")]
    #[diagnostic(
        code(shady::process::pipe_clone_failed),
        help("This might indicate a resource limit or file descriptor issue")
    )]
    PipeCloneError(String),

    #[error("Missing required CLI argument: {0}")]
    #[diagnostic(
        code(shady::cli::missing_argument),
        help("Check the function signature to see which arguments are required")
    )]
    MissingCliArgument(String),

    #[error("Recursion depth limit exceeded (max: {0})")]
    #[diagnostic(
        code(shady::runtime::recursion_limit),
        help("Your function is recursing too deeply. Check for infinite recursion or increase the limit.")
    )]
    RecursionLimitExceeded(usize),

    #[error("Process limit exceeded (max: {0})")]
    #[diagnostic(
        code(shady::runtime::process_limit),
        help("Too many processes spawned. This might indicate a fork bomb or resource leak.")
    )]
    ProcessLimitExceeded(usize),

    #[error("Evaluation error: {0}")]
    #[diagnostic(code(shady::eval::error))]
    EvalError(String),
}

/// Result type alias for Shady operations
pub type Result<T> = std::result::Result<T, ShadyError>;
