// Shady language library

pub mod ast;
pub mod builtins;
pub mod cli;
pub mod error;
pub mod eval;
pub mod typecheck;
pub mod types;

// LSP module is only used by the shady-lsp binary
pub mod lsp;

// Re-export commonly used items
pub use ast::{parse_script, ProgramAST};
pub use error::{Result, ShadyError};
pub use eval::{build_context, ShadyContext};
pub use types::{Type, Value};

// Parser module (tree-sitter generated code)
mod parser {
    include!("parser.rs");
}
