# Issue: Error Span Points to Operator Instead of Wrong Argument

## Status
- **Test**: `eval_builtin_type_error_points_to_wrong_argument` in `src/eval.rs:1107`
- **Currently**: Ignored (marked with `#[ignore]`)
- **Severity**: Low - improves error messages but doesn't affect functionality

## Description

When a builtin function is called with the wrong type for one of its arguments, the error span should point to the specific wrong argument, not to the operator itself.

### Current Behavior

```rust
#[test]
fn eval_builtin_type_error_points_to_wrong_argument() {
    let script = "main = 1 + true;";
    //              0123456789012345
    //                     ^  ^^^^
    //                     |  expected: offset 11
    //                     actual: offset 7 (the '+' operator)

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
            // Expected: span points to 'true' (offset 11, length 4)
            assert_eq!(span.offset(), 11);  // ❌ FAILS - actual is 7
        }
        _ => panic!("Wrong error type"),
    }
}
```

**Expected**: Error span points to `true` (the wrong argument) at offset 11
**Actual**: Error span points to `+` (the operator) at offset 7

### Error Message Comparison

**Current error message:**
```
Error: Type mismatch in function '+'
  --> test.shady:1:8
   |
 1 | main = 1 + true;
   |        ^
   |
Expected signature: fn(int, int) -> int
Actual arguments: int, bool
```

**Desired error message:**
```
Error: Type mismatch in function '+'
  --> test.shady:1:12
   |
 1 | main = 1 + true;
   |            ^^^^
   |
Expected: int
Actual: bool
```

## Root Cause

The issue is in how we track source spans when evaluating expressions. When evaluating an infix expression like `1 + true`:

1. The parser creates a `FnCall` node with:
   - `name`: "+"
   - `args`: [Expr::Value(1), Expr::Value(true)]
   - `span`: Points to the `+` operator

2. When evaluation fails in `eval_builtin()`, we return the error with the span from the FnCall node, which points to the operator, not the arguments.

### Current Code Path

```rust
// In src/eval.rs
pub fn eval_builtin(
    local_context: &LocalContext,
    context: &ShadyContext,
    fn_name: &str,
    args: &[Expr],
    span: &Span,  // <-- This span comes from the FnCall node (points to operator)
) -> Result<Value> {
    // ... type checking happens here ...

    if !signature_matches {
        return Err(ShadyError::FunctionSignatureMismatch {
            name: fn_name.to_string(),
            span: span.clone(),  // <-- Uses the operator's span, not argument's
            expected: signature.params.clone(),
            actual: arg_types,
        });
    }
    // ...
}
```

## Investigation Done

The test was already written (but failing), indicating this was a known TODO item. The test expects:
- Offset 11 (pointing to `true`)
- Actual offset is 7 (pointing to `+`)

Character positions in `"main = 1 + true;"`:
```
Position: 0123456789012345
Content:  main = 1 + true;
                  ^ ^
                  7 11
```

## Potential Solutions

### Option 1: Pass Argument Spans Separately

Modify `eval_builtin()` to receive both the operator span and the argument expressions (which have their own spans):

```rust
pub fn eval_builtin(
    local_context: &LocalContext,
    context: &ShadyContext,
    fn_name: &str,
    args: &[Expr],  // Each Expr has its own span
    operator_span: &Span,
) -> Result<Value> {
    // Evaluate arguments and track their spans
    let mut arg_values = Vec::new();
    let mut arg_spans = Vec::new();

    for arg in args {
        arg_values.push(eval_expr(local_context, context, arg)?);
        arg_spans.push(arg.span());  // Get span from each argument
    }

    // When signature doesn't match, find WHICH argument is wrong
    for (i, (expected, actual)) in expected_types.iter().zip(&arg_types).enumerate() {
        if expected != actual {
            return Err(ShadyError::FunctionSignatureMismatch {
                name: fn_name.to_string(),
                span: arg_spans[i].clone(),  // <-- Use argument's span
                expected: vec![expected.clone()],
                actual: vec![actual.clone()],
            });
        }
    }
    // ...
}
```

### Option 2: Include Spans in Expr Enum

Ensure every `Expr` variant includes its span, then extract it during evaluation:

```rust
pub enum Expr {
    Value(Value, Span),
    Variable(String, Span),
    FnCall {
        name: String,
        args: Vec<Expr>,
        span: Span,  // Operator span
    },
    // ... etc
}

impl Expr {
    pub fn span(&self) -> &Span {
        match self {
            Expr::Value(_, span) => span,
            Expr::Variable(_, span) => span,
            Expr::FnCall { span, .. } => span,
            // ... etc
        }
    }
}
```

### Option 3: Enhanced Error Type

Create a more detailed error type that can point to multiple spans:

```rust
pub enum ShadyError {
    TypeMismatch {
        function_name: String,
        function_span: Span,     // Points to operator/function name
        argument_index: usize,
        argument_span: Span,     // Points to the wrong argument
        expected: Type,
        actual: Type,
    },
    // ... other variants
}
```

## Challenges

1. **Tree-sitter AST**: The current AST comes from Tree-sitter, which provides spans. We need to make sure we preserve spans throughout the evaluation pipeline.

2. **Span Extraction**: Currently, spans might not be propagated from the Tree-sitter AST to our internal `Expr` enum. Need to verify this.

3. **Multiple Candidates**: With function overloading (multiple signatures for same function), we might need to track which argument failed for which signature candidate.

4. **Performance**: Adding spans to every Expr variant might increase memory usage, but this is probably negligible.

## Current AST Structure

Looking at `src/ast.rs`, the `Expr` enum does include spans in some variants:

```rust
pub enum Expr {
    Value(Value),
    Variable(String),
    FnCall {
        name: String,
        args: Vec<Expr>,
        span: Span,
    },
    // ...
}
```

So `FnCall` has a span, but `Value` and `Variable` don't. This is the problem!

## Recommended Solution

**Phase 1: Add spans to all Expr variants**
```rust
pub enum Expr {
    Value(Value, Span),
    Variable(String, Span),
    FnCall {
        name: String,
        args: Vec<Expr>,
        span: Span,  // Keep this for the operator itself
    },
    // ... etc, add Span to all variants
}
```

**Phase 2: Extract spans during parsing**

In `src/ast.rs`, when constructing `Expr` from Tree-sitter nodes, capture the span:

```rust
fn build_expr(node: &Node, source: &str) -> Result<Expr> {
    match node.kind() {
        "number" => {
            let span = node_to_span(node);
            let value = parse_number(node, source)?;
            Ok(Expr::Value(Value::Int(value), span))
        }
        "identifier" => {
            let span = node_to_span(node);
            let name = node_text(node, source);
            Ok(Expr::Variable(name, span))
        }
        // ... etc
    }
}
```

**Phase 3: Use argument spans in error reporting**

Modify error reporting in `src/eval.rs` to point to the specific wrong argument:

```rust
// When type mismatch found
return Err(ShadyError::TypeMismatch {
    function_name: fn_name.to_string(),
    function_span: operator_span,
    argument_index: i,
    argument_span: args[i].span(),  // Now we can get the arg's span!
    expected: expected_types[i].clone(),
    actual: arg_types[i].clone(),
});
```

## Next Steps

1. **Audit Expr enum** - Verify which variants have spans and which don't
2. **Add spans to all Expr variants** - Ensure every expression carries its source location
3. **Update AST building** - Make sure Tree-sitter spans are captured when building Expr
4. **Update eval_builtin** - Use argument spans in error messages
5. **Update error formatting** - Make miette show the right span in error output
6. **Re-enable test** - Once fixed, remove `#[ignore]` attribute

## Files Involved

- `src/ast.rs` - Expr enum definition and AST building from Tree-sitter
- `src/eval.rs` - Expression evaluation and error reporting
- `src/error.rs` - Error type definitions
- `src/types.rs` - Type definitions (possibly Span type)

## Related Issues

This is part of a larger effort to improve error messages with better source spans. The infrastructure is mostly there (Tree-sitter provides spans, miette formats them nicely), we just need to thread spans through the evaluation pipeline properly.

## Priority

Low priority - this is a quality-of-life improvement for error messages. The errors are still reported correctly, just pointing to a less helpful location. This can be fixed incrementally without breaking existing functionality.
