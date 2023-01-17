extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;

fn print_usage(context: ShadyContext) {
    println!("Usage: shady {}", context.args.filename);
    for fun in context.program.fn_definitions {
        let signature = &fun.signature;
        if !signature.is_public {
            continue;
        }
        print!("{}", signature.fn_name);
        for param in &signature.parameters {
            print!(" <{}>", param[1..].to_uppercase());
        }
        println!();
    }
}

use clap::Parser as ClapParser;

#[derive(ClapParser, Debug)]
#[command(author, version, about, long_about = None)]
struct ShadyArgs {
    #[arg(short, long)]
    ast: bool,

    filename: String,
}

struct ShadyContext {
    args: ShadyArgs,
    program: ast::ProgramAST,
}

fn main() {
    let args = ShadyArgs::parse();

    let program = ast::parse_script(&args.filename);
    let context = ShadyContext { args, program };

    if context.args.ast {
        println!("{:#?}", context.program);
        return;
    }

    print_usage(context);
}
