use shady_macros::builtin;

#[builtin]
fn env(var_name: String, default: String) -> String {
    std::env::var(var_name).unwrap_or(default)
}

#[builtin]
fn os() -> String {
    std::env::consts::OS.to_string()
}

mod test {
    use super::*;

    #[test]
    fn test_env() {
        assert_eq!(
            env("WHATEVER".to_string(), "default".to_string()),
            "default"
        );
    }

    #[test]
    fn test_os() {
        assert_eq!(os(), "linux");
    }
}
