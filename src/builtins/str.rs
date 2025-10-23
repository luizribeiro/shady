use shady_macros::builtin;

#[builtin("==", infix = true)]
fn str_eq_str(a: String, b: String) -> bool {
    a == b
}

#[builtin("!=", infix = true)]
fn str_neq_str(a: String, b: String) -> bool {
    a != b
}

#[builtin("+", infix = true)]
fn str_add_str(a: String, b: String) -> String {
    a + &b
}

#[builtin]
fn lines(s: String) -> Vec<String> {
    s.lines().map(|s| s.to_string()).collect()
}

#[builtin]
fn to_string(i: i64) -> String {
    i.to_string()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_str_eq_str() {
        assert!(str_eq_str("a".to_string(), "a".to_string()));
        assert!(!str_eq_str("a".to_string(), "b".to_string()));
    }

    #[test]
    fn test_str_neq_str() {
        assert!(!str_neq_str("a".to_string(), "a".to_string()));
        assert!(str_neq_str("a".to_string(), "b".to_string()));
    }

    #[test]
    fn test_str_add_str() {
        assert_eq!(
            str_add_str("a".to_string(), "b".to_string()),
            "ab".to_string()
        );
    }

    #[test]
    fn test_lines() {
        assert_eq!(
            lines("a\nb\nc".to_string()),
            vec!["a".to_string(), "b".to_string(), "c".to_string()]
        );
    }

    #[test]
    fn test_to_string() {
        assert_eq!(to_string(1), "1".to_string());
        assert_eq!(to_string(0), "0".to_string());
        assert_eq!(to_string(-1), "-1".to_string());
        assert_eq!(to_string(42), "42".to_string());
    }
}
