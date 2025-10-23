# LSP Variable Completion Redesign

## Problem Statement

Variable completion in the Shady LSP is currently unreliable due to **parser fragility with incomplete code**. When typing:

```shady
doit $something: str = seq [
  echo $some<TAB>
```

The parser fails because:
- Missing closing `]`
- Missing closing `;`

When parsing fails, we have no AST, so we can't find the enclosing function or its parameters.

### Current Hacky Approach

We try multiple "fixes":
1. Append `;`
2. Append `];`
3. Append `\n];`
4. Fall back to regex-based line parsing

**Problems:**
- Fragile: breaks with nested structures
- Incomplete: doesn't handle all cases (missing `)`, missing `}`, etc.)
- Unmaintainable: adding more cases makes it exponentially complex
- Wrong approach: fighting the parser instead of fixing it

## Root Cause Analysis

The Pest parser we use is a **PEG parser** that expects complete, valid input. It has no error recovery mechanism. When it encounters unexpected tokens or EOF, it immediately fails with no partial results.

### What We Actually Need

**Error-tolerant parsing** (also called "resilient parsing" or "error recovery"):
- Parse as much as possible, even with syntax errors
- Insert synthetic tokens for missing delimiters
- Return a partial AST with error markers
- Used by all major IDE language servers (rust-analyzer, TypeScript, etc.)

## Solution Options

### Option 1: Add Error Recovery to Pest Parser (REJECTED)

**Approach:** Modify our Pest grammar to handle incomplete input.

**Pros:**
- Stays with current parser technology
- Minimal architectural changes

**Cons:**
- **Pest doesn't support error recovery well** - it's designed for complete parsing
- Would require extensive grammar changes
- Hacky workarounds in grammar rules
- Not the right tool for the job

**Decision:** ❌ Don't pursue this

### Option 2: Custom Error-Tolerant Parser (RECOMMENDED)

**Approach:** Write a simple, custom recursive-descent parser specifically for LSP purposes that tolerates errors.

**Implementation:**
1. Keep the existing Pest parser for normal compilation/execution
2. Add a **parallel LSP-specific parser** that:
   - Parses incrementally
   - Inserts synthetic tokens for missing delimiters
   - Continues parsing after errors
   - Returns a partial AST with error recovery

**Pros:**
- ✅ Complete control over error recovery
- ✅ Can handle any incomplete code scenario
- ✅ Clean separation: production parser vs LSP parser
- ✅ LSP parser can be simpler (doesn't need full validation)

**Cons:**
- ⚠️ Need to maintain two parsers
- ⚠️ More initial implementation work

**Decision:** ✅ **This is the right approach**

### Option 3: Use an Existing Error-Tolerant Parser (ALTERNATIVE)

**Approach:** Replace Pest with a parser framework that supports error recovery.

Options:
- **tree-sitter**: Incremental, error-tolerant parser generator
- **rowan** (from rust-analyzer): Lossless syntax tree with error recovery
- **chumsky**: Parser combinator with error recovery

**Pros:**
- ✅ Battle-tested error recovery
- ✅ Designed for IDE use cases
- ✅ Good tooling and documentation

**Cons:**
- ⚠️ Major refactoring required
- ⚠️ Would break existing parser
- ⚠️ Learning curve for new framework
- ⚠️ May be overkill for our simple language

**Decision:** 🤔 Consider for future, but too disruptive for now

## Recommended Implementation Plan

### Phase 1: Implement LSP-Specific Parser (2-3 hours)

**Goal:** Create a simple error-tolerant parser just for LSP operations.

**File:** `src/lsp_parser.rs`

**Features:**
```rust
pub struct LspParser<'a> {
    text: &'a str,
    pos: usize,
}

pub struct LspParseResult {
    // Partial AST - may have errors
    pub functions: Vec<LspFunction>,
    // Errors encountered (for diagnostics)
    pub errors: Vec<LspError>,
}

pub struct LspFunction {
    pub name: String,
    pub parameters: Vec<LspParameter>,
    pub span: Range<usize>,
    // Where the body starts (for scope detection)
    pub body_start: usize,
}

impl<'a> LspParser<'a> {
    /// Parse even incomplete code
    pub fn parse_tolerant(text: &'a str) -> LspParseResult {
        // ...
    }

    /// Find function containing a byte offset
    pub fn find_function_at_position(&self, offset: usize) -> Option<&LspFunction> {
        // ...
    }
}
```

**Error Recovery Rules:**
1. **Missing semicolons**: Assume statement ends at newline or `}`
2. **Unclosed brackets/parens**: Insert synthetic closing tokens at reasonable places:
   - End of line for simple expressions
   - Before next function definition
   - At EOF
3. **Incomplete expressions**: Mark as synthetic and continue
4. **Invalid tokens**: Skip and continue parsing

**Parsing Strategy:**
- Top-down: Parse function definitions first
- For each function:
  - Extract name, parameters, return type
  - Mark body boundaries (from `=` to `;` or EOF)
  - Don't parse expression details (we don't need them for completion!)

### Phase 2: Integrate with LSP Completion (1 hour)

**Changes to `src/lsp.rs`:**

```rust
async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
    // ... existing setup ...

    // NEW: Use LSP parser instead of hacks
    let lsp_parse = LspParser::parse_tolerant(&doc.text);

    if completing_variable {
        // Find function containing cursor
        let offset = position_to_offset(&doc.text, position);
        if let Some(func) = lsp_parse.find_function_at_position(offset) {
            // Return parameter completions
            return Ok(Some(create_variable_completions(func, position, line_up_to_cursor)));
        }
    }

    // For function completions, try full parse first, then fall back
    let ast = match parse_script(&doc.text) {
        Ok(ast) => ast,
        Err(_) => {
            // Use LSP parser's functions for completion too
            return Ok(Some(create_function_completions_from_lsp(&lsp_parse)));
        }
    };

    // ... existing function completion logic ...
}
```

### Phase 3: Remove Hacks (30 mins)

**Delete:**
- `extract_parameters_from_line()` - replaced by LSP parser
- Multiple parse fix attempts (`;`, `];`, etc.) - no longer needed
- Line-based fallbacks - LSP parser handles it

**Clean up:**
- Simplify completion handler
- Remove defensive code
- Better error messages

## Testing Strategy

**Unit Tests** (`src/lsp_parser.rs`):
```rust
#[test]
fn test_parse_incomplete_list() {
    let code = "doit $x: str = seq [\n  echo $x";
    let result = LspParser::parse_tolerant(code);
    assert_eq!(result.functions.len(), 1);
    assert_eq!(result.functions[0].name, "doit");
    assert_eq!(result.functions[0].parameters.len(), 1);
}

#[test]
fn test_parse_missing_semicolon() {
    let code = "doit $x: str = echo $x";
    let result = LspParser::parse_tolerant(code);
    assert_eq!(result.functions.len(), 1);
}

#[test]
fn test_parse_nested_incomplete() {
    let code = "doit $x: str = if ($x) seq [\n  echo";
    let result = LspParser::parse_tolerant(code);
    assert_eq!(result.functions.len(), 1);
}

#[test]
fn test_find_function_at_position() {
    let code = "f1 = 1;\nf2 $x: int = $x;\nf3 = 3;";
    let result = LspParser::parse_tolerant(code);

    // Position in f2
    let func = result.find_function_at_position(20);
    assert_eq!(func.unwrap().name, "f2");
}
```

**Integration Tests** (`src/lsp.rs`):
- Test completion with all incomplete code scenarios
- Test multiline cases
- Test nested structures

## Implementation Checklist

- [ ] Create `src/lsp_parser.rs` with basic structure
- [ ] Implement tolerant function definition parsing
- [ ] Implement parameter extraction
- [ ] Implement `find_function_at_position()`
- [ ] Add comprehensive unit tests
- [ ] Integrate with LSP completion handler
- [ ] Remove old hacks and fallbacks
- [ ] Update integration tests
- [ ] Test in real editor with various scenarios
- [ ] Update LSP_SETUP.md with notes about error recovery

## Benefits of This Approach

1. **Robustness**: Handles ANY incomplete code, not just specific cases
2. **Maintainability**: Clear separation between production parser and LSP parser
3. **Simplicity**: LSP parser is simpler (doesn't need full expression parsing)
4. **Performance**: Can optimize for LSP use case (only parse what's needed)
5. **Extensibility**: Easy to add more LSP features (hover, references, etc.)
6. **Correctness**: No more guessing with regex patterns

## Future Enhancements

Once LSP parser is in place:
- **Incremental parsing**: Only re-parse changed portions
- **Syntax highlighting**: Use LSP parser for better highlighting
- **Better diagnostics**: Show multiple errors at once
- **Semantic tokens**: Distinguish variables from functions, etc.
- **Find references**: Works even in incomplete code
