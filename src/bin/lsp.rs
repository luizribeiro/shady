extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast {
    include!("../ast.rs");
}

mod error {
    include!("../error.rs");
}

mod types {
    include!("../types.rs");
}

mod lsp {
    include!("../lsp.rs");
}

#[tokio::main]
async fn main() {
    lsp::run_server().await;
}
