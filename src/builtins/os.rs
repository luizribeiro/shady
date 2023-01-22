use shady_macros::builtin;

#[builtin]
fn env(var_name: String, default: String) -> String {
    std::env::var(var_name).unwrap_or(default)
}

#[builtin]
fn os() -> String {
    std::env::consts::OS.to_string()
}
