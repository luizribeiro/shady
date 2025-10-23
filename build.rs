use std::path::PathBuf;

fn main() {
    let src_dir = PathBuf::from("src");

    let mut c_config = cc::Build::new();
    c_config.include(&src_dir);
    c_config
        .flag_if_supported("-Wno-unused-parameter")
        .flag_if_supported("-Wno-unused-but-set-variable")
        .flag_if_supported("-Wno-trigraphs");

    // Compile the tree-sitter parser
    let parser_path = src_dir.join("parser.c");
    c_config.file(&parser_path);

    // If there's a scanner.c file (for external scanners), compile it too
    let scanner_path = src_dir.join("scanner.c");
    if scanner_path.exists() {
        c_config.file(&scanner_path);
    }

    c_config.compile("tree-sitter-shady");

    println!("cargo:rerun-if-changed=src/parser.c");
    println!("cargo:rerun-if-changed=src/scanner.c");
    println!("cargo:rerun-if-changed=src/tree_sitter/parser.h");
}
