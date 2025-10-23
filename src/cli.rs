use std::collections::HashMap;

use crate::ast;
use crate::builtins::proc::exec;
use crate::error::Result;
use crate::eval;
use crate::eval::ShadyContext;
use crate::types::{from_string, Type, Value};

use clap::{command, value_parser, Arg, Command};
use miette::SourceSpan;

// Note: Box::leak is used to convert Strings to 'static str for clap's API.
// This is an acceptable tradeoff because:
// 1. CLI parsing happens only once per program run
// 2. The amount of leaked memory is small (function and parameter names)
// 3. The program terminates shortly after, and the OS reclaims all memory
// 4. Clap 4.1's builder API requires 'static lifetimes for some methods
fn string_to_static_str(s: String) -> &'static str {
    Box::leak(s.into_boxed_str())
}

fn get_command(context: &ShadyContext) -> clap::Command {
    let mut cmd = command!()
        .bin_name(&context.filename)
        .args_conflicts_with_subcommands(true);

    // Find the main function if it exists
    let main_fn = context
        .program
        .fn_definitions
        .iter()
        .find(|f| f.signature.is_public && f.signature.fn_name == "main");

    for fun in &context.program.fn_definitions {
        let signature = &fun.signature;
        if !signature.is_public {
            continue;
        }

        let mut subcmd = Command::new(string_to_static_str(signature.fn_name.clone()));
        for param in &signature.parameters {
            let mut arg = Arg::new(string_to_static_str(param.name.clone()));
            arg = match param.typ {
                Type::Int => arg.value_parser(value_parser!(i64)),
                Type::Str => arg.value_parser(value_parser!(String)),
                Type::Bool => arg.value_parser(value_parser!(bool)),
                Type::List(_) => arg.value_parser(value_parser!(String)),
                // TODO: make it impossible to have public methods with certain types as a
                // parameter
                Type::Proc => unreachable!(),
                Type::Fn(_, _) => unreachable!(),
                Type::Any => unreachable!(),
            };
            if let Some(default) = &param.spec.default_value {
                arg = arg.default_value(string_to_static_str(default.to_string()));
            } else {
                arg = arg.required(true);
            }
            if param.spec.is_option {
                arg = arg.long(string_to_static_str(param.name.clone()));
                if let Some(short) = param.spec.short {
                    arg = arg.short(short);
                }
            }

            // If this is the main function, also add its args to the top-level command
            if signature.fn_name == "main" {
                cmd = cmd.arg(arg.clone());
            }

            subcmd = subcmd.arg(arg);
        }
        cmd = cmd.subcommand(subcmd);
    }

    // If there's a main function, allow calling without subcommand
    if main_fn.is_some() {
        cmd = cmd.subcommand_required(false);
    }

    cmd
}

pub fn run_fn(context: &ShadyContext, script_args: &Vec<String>) -> Result<()> {
    let cmd = get_command(context);
    let matches = match cmd.try_get_matches_from(script_args) {
        Ok(m) => m,
        Err(e) => {
            // Handle help and version specially - these should print and exit successfully
            use clap::error::ErrorKind;
            match e.kind() {
                ErrorKind::DisplayHelp | ErrorKind::DisplayVersion => {
                    print!("{}", e);
                    std::process::exit(0);
                }
                _ => {
                    return Err(crate::error::ShadyError::EvalError(e.to_string()));
                }
            }
        }
    };

    let mut vars = HashMap::new();
    let subcmd_name = matches.subcommand_name().unwrap_or("main");
    let fun = ast::get_fn_by_name(&context.program, subcmd_name).ok_or_else(|| {
        crate::error::ShadyError::FunctionNotFound {
            name: subcmd_name.to_string(),
            span: SourceSpan::from(0..0),
        }
    })?;

    // Determine which matches object to use for extracting arguments
    let args = if let Some(subcommand_args) = matches.subcommand_matches(subcmd_name) {
        // Explicit subcommand was used (e.g., "./script.shady main value")
        subcommand_args
    } else if subcmd_name == "main" {
        // No subcommand given, use top-level matches for main (e.g., "./script.shady value")
        &matches
    } else {
        // No subcommand and it's not main - this shouldn't happen
        return Err(crate::error::ShadyError::FunctionNotFound {
            name: subcmd_name.to_string(),
            span: SourceSpan::from(0..0),
        });
    };

    for param in &fun.signature.parameters {
        let raw_values = args
            .get_raw(&param.name)
            .ok_or_else(|| crate::error::ShadyError::MissingCliArgument(param.name.clone()))?;

        for raw_cli_value in raw_values {
            let cli_value: String = raw_cli_value.to_string_lossy().into_owned();
            let value = from_string(&param.typ, &cli_value)?;
            vars.insert(param.name.clone(), value);
        }
    }

    let local_context = eval::LocalContext { vars, depth: 0 };

    let result = eval::eval_expr_with_type(&local_context, context, &fun.expr, None)?;

    // Auto-execute Proc return values (Phase 1: Make commands run by default)
    // This makes `public main $name: str = echo $name` work without explicit exec
    if let Value::Proc(proc) = result {
        exec(proc)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::parse_script;

    #[test]
    fn test_main_without_explicit_subcommand() {
        // Test that main can be called without "main" subcommand
        let code = "public main $arg: str = echo $arg;";
        let ast = parse_script(code).unwrap();
        let context = eval::build_context("test.shady".to_string(), code.to_string(), ast);

        // Simulate: ./test.shady "value"
        let args = vec!["./test.shady".to_string(), "hello".to_string()];
        let result = run_fn(&context, &args);

        assert!(result.is_ok(), "Should succeed with implicit main");
    }

    #[test]
    fn test_main_with_explicit_subcommand() {
        // Test that main can still be called with explicit "main" subcommand
        let code = "public main $arg: str = echo $arg;";
        let ast = parse_script(code).unwrap();
        let context = eval::build_context("test.shady".to_string(), code.to_string(), ast);

        // Simulate: ./test.shady main "value"
        let args = vec![
            "./test.shady".to_string(),
            "main".to_string(),
            "hello".to_string(),
        ];
        let result = run_fn(&context, &args);

        assert!(result.is_ok(), "Should succeed with explicit main");
    }

    #[test]
    fn test_main_missing_required_arg() {
        // Test that missing required arguments show proper error
        let code = "public main $arg: str = echo $arg;";
        let ast = parse_script(code).unwrap();
        let context = eval::build_context("test.shady".to_string(), code.to_string(), ast);

        // Simulate: ./test.shady (no arguments)
        let args = vec!["./test.shady".to_string()];
        let result = run_fn(&context, &args);

        assert!(
            result.is_err(),
            "Should fail when required argument is missing"
        );
    }

    #[test]
    fn test_main_with_default_arg() {
        // Test that default arguments work without explicit subcommand
        let code = "public main $arg: str (\"default\") = echo $arg;";
        let ast = parse_script(code).unwrap();
        let context = eval::build_context("test.shady".to_string(), code.to_string(), ast);

        // Simulate: ./test.shady (no arguments, should use default)
        let args = vec!["./test.shady".to_string()];
        let result = run_fn(&context, &args);

        assert!(result.is_ok(), "Should succeed with default value");
    }

    #[test]
    fn test_non_main_function_requires_subcommand() {
        // Test that non-main functions still require explicit subcommand
        let code = "public greet $name: str = echo $name;";
        let ast = parse_script(code).unwrap();
        let context = eval::build_context("test.shady".to_string(), code.to_string(), ast);

        // Simulate: ./test.shady greet "Alice"
        let args = vec![
            "./test.shady".to_string(),
            "greet".to_string(),
            "Alice".to_string(),
        ];
        let result = run_fn(&context, &args);

        assert!(result.is_ok(), "Should succeed with explicit subcommand");
    }

    #[test]
    fn test_main_with_multiple_args() {
        // Test main with multiple parameters
        let code = "public main $first: str $second: int = echo $first;";
        let ast = parse_script(code).unwrap();
        let context = eval::build_context("test.shady".to_string(), code.to_string(), ast);

        // Simulate: ./test.shady "hello" 42
        let args = vec![
            "./test.shady".to_string(),
            "hello".to_string(),
            "42".to_string(),
        ];
        let result = run_fn(&context, &args);

        assert!(
            result.is_ok(),
            "Should succeed with multiple args without subcommand"
        );
    }

    #[test]
    fn test_multiple_functions_with_main() {
        // Test that having multiple functions doesn't break main's implicit behavior
        let code = "public main $arg: str = echo $arg;\npublic other $x: int = $x;";
        let ast = parse_script(code).unwrap();
        let context = eval::build_context("test.shady".to_string(), code.to_string(), ast);

        // Simulate: ./test.shady "value" (should call main implicitly)
        let args = vec!["./test.shady".to_string(), "value".to_string()];
        let result = run_fn(&context, &args);

        assert!(
            result.is_ok(),
            "Should call main implicitly even with other functions present"
        );

        // Simulate: ./test.shady other 123 (should call other explicitly)
        let args = vec![
            "./test.shady".to_string(),
            "other".to_string(),
            "123".to_string(),
        ];
        let result = run_fn(&context, &args);

        assert!(result.is_ok(), "Should call other function explicitly");
    }

    // Auto-exec tests (Phase 1)
    #[test]
    fn test_auto_exec_single_proc() {
        // Test that returning a single Proc auto-executes it
        let code = "public main $msg: str = echo $msg;";
        let ast = parse_script(code).unwrap();
        let context = eval::build_context("test.shady".to_string(), code.to_string(), ast);

        // This should auto-execute the echo command without explicit exec
        let args = vec!["./test.shady".to_string(), "hello".to_string()];
        let result = run_fn(&context, &args);

        assert!(result.is_ok(), "Should auto-exec Proc return value");
    }

    #[test]
    fn test_auto_exec_with_seq() {
        // Test that seq still works (it internally calls exec)
        let code = "public main $msg: str = seq [(echo $msg); (echo $msg)];";
        let ast = parse_script(code).unwrap();
        let context = eval::build_context("test.shady".to_string(), code.to_string(), ast);

        let args = vec!["./test.shady".to_string(), "hello".to_string()];
        let result = run_fn(&context, &args);

        assert!(result.is_ok(), "Should work with seq");
    }

    #[test]
    fn test_non_proc_return_values() {
        // Test that non-Proc return values still work (int, str, etc.)
        let code = "public main $x: int = $x + 1;";
        let ast = parse_script(code).unwrap();
        let context = eval::build_context("test.shady".to_string(), code.to_string(), ast);

        let args = vec!["./test.shady".to_string(), "42".to_string()];
        let result = run_fn(&context, &args);

        assert!(
            result.is_ok(),
            "Should work with non-Proc return values (no auto-exec needed)"
        );
    }

    // Help display tests
    #[test]
    fn test_script_help_displays_subcommands() {
        // Test that ./script.shady --help shows the script's subcommands
        let code = r#"
            public main $arg: str = echo $arg;
            public greet $name: str = echo $name;
        "#;
        let ast = parse_script(code).unwrap();
        let context = eval::build_context("./test.shady".to_string(), code.to_string(), ast);

        let mut cmd = get_command(&context);
        let help_output = cmd.render_help().to_string();

        // Should show both subcommands
        assert!(
            help_output.contains("main"),
            "Help should show main command"
        );
        assert!(
            help_output.contains("greet"),
            "Help should show greet command"
        );
        // Should show the script name, not "shady <filename>"
        assert!(
            help_output.contains("./test.shady"),
            "Help should show script name"
        );
        assert!(
            !help_output.contains("shady ./test.shady"),
            "Help should not show 'shady' prefix"
        );
    }

    #[test]
    fn test_subcommand_help_shows_arguments() {
        // Test that subcommand help shows the correct arguments
        let code = "public greet $name: str $count: int = echo $name;";
        let ast = parse_script(code).unwrap();
        let context = eval::build_context("./test.shady".to_string(), code.to_string(), ast);

        let mut cmd = get_command(&context);
        let subcommand = cmd.find_subcommand("greet").unwrap();
        let help_output = subcommand.clone().render_help().to_string();

        assert!(help_output.contains("<name>"), "Should show name argument");
        assert!(
            help_output.contains("<count>"),
            "Should show count argument"
        );
    }

    #[test]
    fn test_main_help_shows_arguments() {
        // Test that main subcommand help works
        let code = "public main $something: str $bar: int = echo $something;";
        let ast = parse_script(code).unwrap();
        let context = eval::build_context("./test.shady".to_string(), code.to_string(), ast);

        let mut cmd = get_command(&context);
        let subcommand = cmd.find_subcommand("main").unwrap();
        let help_output = subcommand.clone().render_help().to_string();

        assert!(
            help_output.contains("<something>"),
            "Should show something argument"
        );
        assert!(help_output.contains("<bar>"), "Should show bar argument");
    }

    #[test]
    fn test_help_with_default_values() {
        // Test that help correctly shows optional arguments with defaults
        let code = r#"public greet $name: str ("World") = echo $name;"#;
        let ast = parse_script(code).unwrap();
        let context = eval::build_context("./test.shady".to_string(), code.to_string(), ast);

        let mut cmd = get_command(&context);
        let subcommand = cmd.find_subcommand("greet").unwrap();
        let help_output = subcommand.clone().render_help().to_string();

        // Should show the argument (exact format may vary by clap version)
        assert!(
            help_output.contains("name") || help_output.contains("<name>"),
            "Should show name argument"
        );
    }

    #[test]
    fn test_help_with_options() {
        // Test that help shows options with -- prefix
        let code = r#"public run $verbose: bool (false, option) = echo "test";"#;
        let ast = parse_script(code).unwrap();
        let context = eval::build_context("./test.shady".to_string(), code.to_string(), ast);

        let mut cmd = get_command(&context);
        let subcommand = cmd.find_subcommand("run").unwrap();
        let help_output = subcommand.clone().render_help().to_string();

        assert!(
            help_output.contains("--verbose"),
            "Should show --verbose option"
        );
    }
}
