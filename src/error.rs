use thiserror::Error;
use crate::types::Type;

/// Main error type for Shady interpreter
#[derive(Error, Debug)]
pub enum ShadyError {
    #[error("Variable '{0}' not found")]
    VariableNotFound(String),

    #[error("Type mismatch: expected {expected}, got {actual}")]
    TypeMismatch { expected: String, actual: String },

    #[error("Parse error at line {line}: {message}")]
    ParseError { line: usize, message: String },

    #[error("Parse error: {0}")]
    ParseErrorSimple(String),

    #[error("Function '{name}' not found")]
    FunctionNotFound { name: String },

    #[error("Function '{name}' found but no matching signature for argument types: {arg_types}")]
    FunctionSignatureMismatch { name: String, arg_types: String },

    #[error("Process execution failed: {0}")]
    ProcessError(#[from] std::io::Error),

    #[error("I/O error: {0}")]
    IoError(String),

    #[error("Empty lists require type annotation or context to infer type")]
    EmptyListNeedsType,

    #[error("Required parameter '{0}' cannot appear after optional parameters")]
    RequiredAfterOptional(String),

    #[error("Default value type mismatch for parameter '{param}': expected {expected:?}, got {actual:?}")]
    DefaultTypeMismatch {
        param: String,
        expected: Type,
        actual: Type,
    },

    #[error("CLI argument error: {0}")]
    CliError(String),

    #[error("Thread panicked during execution")]
    ThreadPanic,

    #[error("Process timeout")]
    ProcessTimeout,

    #[error("Invalid conversion from {from} to {to}")]
    InvalidConversion { from: String, to: String },

    #[error("Pipe clone failed: {0}")]
    PipeCloneError(String),

    #[error("Missing required CLI argument: {0}")]
    MissingCliArgument(String),

    #[error("Recursion depth limit exceeded (max: {0})")]
    RecursionLimitExceeded(usize),

    #[error("Process limit exceeded (max: {0})")]
    ProcessLimitExceeded(usize),

    #[error("Evaluation error: {0}")]
    EvalError(String),
}

/// Result type alias for Shady operations
pub type Result<T> = std::result::Result<T, ShadyError>;
