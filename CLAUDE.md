# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Shady is a prototype scripting shell language written in Rust. It's a typed scripting language with a focus on external command execution and process management.

## Development Environment

This project uses **devenv** for development environment management with Nix.

### Common Commands

```bash
# Enter the development shell
devenv shell

# Build the project
devenv shell cargo build

# Run tests (76 unit tests + 10 integration tests)
devenv shell cargo test

# Run a specific test
devenv shell cargo test test_name

# Run the shady interpreter
devenv shell cargo run -- <script.shady>

# Run with --ast flag to see the parsed AST
devenv shell cargo run -- --ast <script.shady>

# Read from stdin
devenv shell cargo run -- - <args>
```

## Architecture

### High-Level Flow

1. **Parsing** (`ast.rs`): Source code → AST using Pest parser grammar (`shady.pest`)
2. **Evaluation** (`eval.rs`): AST → Execution
3. **CLI** (`cli.rs`): Command-line interface generation using clap (public functions become subcommands)

### Key Components

#### Type System (`types.rs`)

Shady has a small type system with:
- Primitive types: `Int`, `Str`, `Bool`
- Composite types: `List(T)`, `Proc` (process handle)
- Special: `Any` (acts as wildcard in type matching)

The `PrimitiveValue` trait provides conversion between Rust types and Shady `Value` enum.

`Proc` type represents spawned external processes with stdin/stdout/stderr pipes.

#### AST Representation (`ast.rs`)

Key structures:
- `ProgramAST`: Collection of function definitions
- `FnDefinition`: Function signature + expression body
- `FnSignature`: Name, parameters, return type, visibility (`public`/private), and whether it's `infix`
- `Expr`: Recursive expression type (values, variables, calls, if-expressions, lists)
- `Parameter`: Has name, type, and `ParamSpec` (for default values, CLI options)

Function signatures support:
- Default parameter values: `fn $x: int (42) = ...`
- CLI options: `fn $x: int (42, option) = ...`
- Short flags: `fn $x: int (42, option, short 'x') = ...`

#### Evaluation (`eval.rs`)

Two-phase context system:
- `ShadyContext`: Global (program AST + builtin function index)
- `LocalContext`: Per-function (variable bindings)

Function resolution order:
1. Check builtin functions (in `BuiltinIndex` HashMap)
2. Check user-defined functions (in program AST)
3. Fall back to spawning external command (becomes a `Proc`)

**Critical**: External commands are spawned with pipes for stdin/stdout/stderr, stored in the `Proc` struct with `Rc<RefCell<Child>>` for shared ownership.

#### Builtins (`builtins/`)

Builtin functions are organized by type:
- `bool.rs`: Boolean operations (`&&`, `||`)
- `int.rs`: Integer arithmetic (`+`, `-`, `*`, `/`, `%`, `^`) and comparisons
- `str.rs`: String operations (`+` concat, `==`, `!=`, `lines`, `to_string`)
- `list.rs`: List operations (`+` concat, `first`, `add_all`)
- `proc.rs`: Process operations (`exec`, `stdin`, `stdout`, `stderr`)
- `os.rs`: OS operations (`env`, `os`)
- `io.rs`: I/O operations (`echo`, `cat`)

The `setup_builtins!()` macro (from `shady-macros`) auto-registers all builtin functions into the `BuiltinIndex` by scanning function attributes.

#### CLI Interface (`cli.rs`)

Public functions automatically become CLI subcommands via clap:
- Function name → subcommand name
- Parameters → CLI arguments (positional or `--flags` if marked as `option`)
- Default values → optional arguments
- Type annotations → value parsers

Example: `public main $x: int (42, option) = ...` creates `--x <INT>` with default 42.

### Pest Grammar (`shady.pest`)

The parser uses Pest with a Pratt parser for expressions, supporting:
- Standard operator precedence (including `^` for power)
- Infix and prefix operators
- If-expressions (ternary-like: `if (cond) val1 else val2`)
- List literals with semicolon separators: `[1; 2; 3]`
- Variables prefixed with `$`: `$varname`
- Unquoted string arguments for command calls

### Testing Structure

- **Unit tests**: Embedded in source files using `#[cfg(test)]`
  - `ast.rs`: Parser tests (parse_* functions)
  - `eval.rs`: Evaluation tests (eval_* functions)
  - `builtins/**/*.rs`: Per-type builtin tests

- **Integration tests**: `tests/proc.rs` for process/command execution

Test macro pattern: Use `eval_tests!` or `parse_expr_tests!` macros for concise test definitions.

## Code Patterns

### Adding Builtin Functions

1. Add function in appropriate `builtins/*.rs` file
2. Use type signature with `PrimitiveValue` bounds
3. Mark with visibility (the macro scans for public functions)
4. The `setup_builtins!()` macro will auto-register it

Example:
```rust
pub fn add(a: i64, b: i64) -> i64 {
    a + b
}
```

### Type Matching

`Type::Any` acts as a wildcard - it equals any type. This is used for return types when exact type inference isn't needed.

Parameter matching uses `PartialEq` on the parameter type list to find the right function overload.

## Current Limitations / TODOs

(From README.md - these are known missing features):
- No variadic functions
- No optional function parameters (only default values)
- Builtins and local functions don't return proc/use streams
- No lambdas / function pointers as parameters
- No map/reduce for lists
- No static type analyzer
- No miette for better error rendering
- Limited stdio redirection from seq
