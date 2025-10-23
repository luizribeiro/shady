use clap::Parser;
use shady::{ast, cli, eval, typecheck};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct ShadyArgs {
    #[arg(short, long)]
    ast: bool,

    filename: String,

    #[arg(allow_hyphen_values = true, trailing_var_arg = true)]
    args: Vec<String>,
}

fn main() {
    // Special handling: if args contain a filename followed by --help or -h,
    // pass --help through to the script instead of showing interpreter help
    let raw_args: Vec<String> = std::env::args().collect();
    let should_passthrough_help = raw_args.len() >= 3
        && !raw_args[1].starts_with('-')
        && (raw_args[2] == "--help" || raw_args[2] == "-h");

    let args = if should_passthrough_help {
        // Parse without the --help so it doesn't trigger interpreter help
        match ShadyArgs::try_parse_from(vec![raw_args[0].clone(), raw_args[1].clone()]) {
            Ok(mut parsed) => {
                // Add the remaining args (including --help) to script args
                parsed.args = raw_args[2..].to_vec();
                parsed
            }
            Err(e) => {
                eprintln!("{}", e);
                std::process::exit(1);
            }
        }
    } else {
        ShadyArgs::parse()
    };

    let (source, program) = if args.filename == "-" {
        use std::io::Read;
        let mut buffer = String::new();
        if let Err(e) = std::io::stdin().read_to_string(&mut buffer) {
            eprintln!("Error reading from stdin: {}", e);
            std::process::exit(1);
        }
        let source = buffer.clone();
        match ast::parse_script(&buffer) {
            Ok(p) => (source, p),
            Err(e) => {
                let report = miette::Report::new(e)
                    .with_source_code(miette::NamedSource::new("stdin", source));
                eprintln!("{:?}", report);
                std::process::exit(1);
            }
        }
    } else {
        let source = match std::fs::read_to_string(&args.filename) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error reading file: {}", e);
                std::process::exit(1);
            }
        };
        match ast::parse_script(&source) {
            Ok(p) => (source.clone(), p),
            Err(e) => {
                let report = miette::Report::new(e)
                    .with_source_code(miette::NamedSource::new(&args.filename, source));
                eprintln!("{:?}", report);
                std::process::exit(1);
            }
        }
    };

    if args.ast {
        println!("{program:#?}");
        return;
    }

    let filename = args.filename.clone();
    let source_clone = source.clone();
    let context = eval::build_context(filename.clone(), source.clone(), program);

    // Run type checking before execution
    let checker = typecheck::TypeChecker::new(&context.program, &context.builtins);
    if let Err(e) = checker.typecheck_program(&context.program) {
        let report = miette::Report::new(e)
            .with_source_code(miette::NamedSource::new(&filename, source_clone));
        eprintln!("{:?}", report);
        std::process::exit(1);
    }

    let mut script_args = vec![args.filename.clone()];
    script_args.extend(args.args);

    if let Err(e) = cli::run_fn(&context, &script_args) {
        let report = miette::Report::new(e)
            .with_source_code(miette::NamedSource::new(&filename, source_clone));
        eprintln!("{:?}", report);
        std::process::exit(1);
    }
}
