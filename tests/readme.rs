// Integration test to validate all Shady code examples in README.md

use shady::ast::{parse_script, parse_script_tolerant};
use shady::eval::build_context;
use shady::formatter::{format_script_with_config, FormatterConfig};
use shady::typecheck::TypeChecker;
use std::fs;

/// Extract all ```shady code blocks from README.md
fn extract_shady_code_blocks() -> Vec<(usize, String)> {
    let readme_path = concat!(env!("CARGO_MANIFEST_DIR"), "/README.md");
    let content = fs::read_to_string(readme_path)
        .expect("Failed to read README.md");

    let mut blocks = Vec::new();
    let mut in_shady_block = false;
    let mut current_block = String::new();
    let mut block_start_line = 0;

    for (line_num, line) in content.lines().enumerate() {
        if line.trim() == "```shady" {
            in_shady_block = true;
            block_start_line = line_num + 1;
            current_block.clear();
        } else if in_shady_block && line.trim() == "```" {
            in_shady_block = false;
            if !current_block.trim().is_empty() {
                blocks.push((block_start_line, current_block.clone()));
            }
        } else if in_shady_block {
            current_block.push_str(line);
            current_block.push('\n');
        }
    }

    blocks
}

/// Check if a code block should be skipped (contains placeholders or is incomplete by design)
fn should_skip_block(code: &str) -> bool {
    // Skip blocks that contain placeholders or are intentionally incomplete examples
    code.contains("...") ||
    code.contains("# In future:") ||
    code.contains("(simplified example)") || // Skip simplified examples that may not be complete
    code.contains("# Before") || // Before/after comparison snippets
    code.contains("# After") ||
    // Skip blocks that reference undefined variables or are just snippets
    code.contains("rest $files") || // rest function doesn't exist yet
    // Skip blocks that use function types as parameters (not yet supported)
    code.contains("$f: fn(") ||
    // Skip blocks with undefined variables (incomplete snippets)
    (code.contains("$name") && !code.contains("$name:")) ||
    (code.contains("$version") && !code.contains("$version:")) ||
    (code.contains("$server") && !code.contains("$server:")) ||
    (code.contains("$logfile") && !code.contains("$logfile:")) ||
    (code.contains("$hours") && !code.contains("$hours:")) ||
    (code.contains("$files") && !code.contains("$files:")) ||
    (code.contains("$services") && !code.contains("$services:")) ||
    (code.contains("$dirs") && !code.contains("$dirs:"))
}

#[test]
fn test_readme_code_blocks_parse() {
    let blocks = extract_shady_code_blocks();

    assert!(
        !blocks.is_empty(),
        "README.md should contain Shady code blocks"
    );

    let mut failed_blocks = Vec::new();

    for (line_num, code) in &blocks {
        if should_skip_block(code) {
            println!("Skipping block at line {} (contains placeholders)", line_num);
            continue;
        }

        match parse_script(code) {
            Ok(_) => {
                // Parsing succeeded
            }
            Err(e) => {
                failed_blocks.push((*line_num, code.clone(), format!("{:?}", e)));
            }
        }
    }

    if !failed_blocks.is_empty() {
        let mut error_msg = String::from("\n\nFailed to parse the following README code blocks:\n");
        for (line_num, code, error) in failed_blocks {
            error_msg.push_str(&format!(
                "\n--- Block at line {} ---\n{}\nError: {}\n",
                line_num, code, error
            ));
        }
        panic!("{}", error_msg);
    }
}

#[test]
fn test_readme_code_blocks_typecheck() {
    let blocks = extract_shady_code_blocks();
    let mut failed_blocks = Vec::new();

    for (line_num, code) in &blocks {
        if should_skip_block(code) {
            continue;
        }

        // Parse the code
        let program = match parse_script(code) {
            Ok(p) => p,
            Err(_) => continue, // Skip if it doesn't parse (covered by other test)
        };

        // Build context for type checking
        let context = build_context("README.md".to_string(), code.clone(), program);

        // Run type checker
        let checker = TypeChecker::new(&context.program, &context.builtins);
        if let Err(type_error) = checker.typecheck_program(&context.program) {
            failed_blocks.push((*line_num, code.clone(), format!("{:?}", type_error)));
        }
    }

    if !failed_blocks.is_empty() {
        let mut error_msg = String::from("\n\nType errors in the following README code blocks:\n");
        for (line_num, code, error) in failed_blocks {
            error_msg.push_str(&format!(
                "\n--- Block at line {} ---\n{}\nType Error: {}\n",
                line_num, code, error
            ));
        }
        panic!("{}", error_msg);
    }
}

#[test]
fn test_readme_has_lambda_examples() {
    let blocks = extract_shady_code_blocks();

    // Ensure README contains lambda examples
    let has_lambda = blocks.iter().any(|(_, code)| {
        code.contains("lambda")
    });

    assert!(
        has_lambda,
        "README should contain lambda expression examples"
    );
}

#[test]
fn test_readme_has_map_filter_reduce_examples() {
    let blocks = extract_shady_code_blocks();

    // Ensure README demonstrates all three higher-order functions
    let has_map = blocks.iter().any(|(_, code)| code.contains("map"));
    let has_filter = blocks.iter().any(|(_, code)| code.contains("filter"));
    let has_reduce = blocks.iter().any(|(_, code)| code.contains("reduce"));

    assert!(has_map, "README should contain map examples");
    assert!(has_filter, "README should contain filter examples");
    assert!(has_reduce, "README should contain reduce examples");
}

#[test]
fn test_readme_code_block_count() {
    let blocks = extract_shady_code_blocks();

    // We should have a substantial number of code examples
    assert!(
        blocks.len() >= 15,
        "README should contain at least 15 Shady code examples, found {}",
        blocks.len()
    );

    println!("README.md contains {} Shady code blocks", blocks.len());
}

#[test]
fn test_readme_code_blocks_are_formatted() {
    let blocks = extract_shady_code_blocks();
    let mut unformatted_blocks = Vec::new();

    // Use 70-character line length for README examples
    let config = FormatterConfig {
        indent_size: 2,
        max_line_length: 70,
    };

    for (line_num, code) in &blocks {
        if should_skip_block(code) {
            continue;
        }

        // Parse the code
        let (parse_result, _) = match parse_script_tolerant(code) {
            Ok(result) => result,
            Err(_) => continue, // Skip if it doesn't parse (covered by other test)
        };

        // Format the code with 70-char limit
        let formatted = format_script_with_config(code, &parse_result, &config);

        // Check if formatting changed anything
        if &formatted != code && !code.ends_with('\n') && &formatted == &format!("{}\n", code) {
            // Allow missing trailing newline - formatter always adds one
            continue;
        }

        if &formatted != code {
            unformatted_blocks.push((*line_num, code.clone(), formatted));
        }
    }

    if !unformatted_blocks.is_empty() {
        let mut error_msg = String::from("\n\nThe following README code blocks are not properly formatted:\n");
        for (line_num, original, formatted) in unformatted_blocks {
            error_msg.push_str(&format!(
                "\n--- Block at line {} ---\nOriginal:\n{}\nFormatted:\n{}\n",
                line_num, original, formatted
            ));
        }
        error_msg.push_str("\nRun the formatter on these blocks to fix them.\n");
        panic!("{}", error_msg);
    }
}

#[test]
fn test_readme_code_blocks_line_length() {
    let blocks = extract_shady_code_blocks();
    // Enforce 70 character line length (including indentation)
    let max_line_length = 70;
    let mut long_line_blocks = Vec::new();

    for (line_num, code) in &blocks {
        if should_skip_block(code) {
            continue;
        }

        let mut long_lines = Vec::new();
        for (i, line) in code.lines().enumerate() {
            if line.len() > max_line_length {
                long_lines.push((i + 1, line.len(), line.to_string()));
            }
        }

        if !long_lines.is_empty() {
            long_line_blocks.push((*line_num, long_lines));
        }
    }

    if !long_line_blocks.is_empty() {
        let mut error_msg = format!(
            "\n\nThe following README code blocks have lines longer than {} characters:\n",
            max_line_length
        );
        for (block_line, long_lines) in long_line_blocks {
            error_msg.push_str(&format!("\n--- Block at line {} ---\n", block_line));
            for (line_num, length, line) in long_lines {
                error_msg.push_str(&format!(
                    "  Line {}: {} chars: {}\n",
                    line_num, length, line
                ));
            }
        }
        error_msg.push_str(&format!(
            "\nPlease wrap these lines to {} characters or less.\n",
            max_line_length
        ));
        panic!("{}", error_msg);
    }
}
