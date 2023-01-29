use shady_macros::builtin;

#[builtin("==", infix = true)]
fn str_eq_str(a: String, b: String) -> bool {
    a == b
}

#[builtin("!=", infix = true)]
fn str_neq_str(a: String, b: String) -> bool {
    a != b
}

#[builtin]
fn lines(s: String) -> Vec<String> {
    s.lines().map(|s| s.to_string()).collect()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_str_eq_str() {
        assert_eq!(str_eq_str("a".to_string(), "a".to_string()), true);
        assert_eq!(str_eq_str("a".to_string(), "b".to_string()), false);
    }

    #[test]
    fn test_str_neq_str() {
        assert_eq!(str_neq_str("a".to_string(), "a".to_string()), false);
        assert_eq!(str_neq_str("a".to_string(), "b".to_string()), true);
    }

    #[test]
    fn test_lines() {
        assert_eq!(
            lines("a\nb\nc".to_string()),
            vec!["a".to_string(), "b".to_string(), "c".to_string()]
        );
    }
}
