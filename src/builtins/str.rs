use shady_macros::builtin;

#[builtin("==")]
fn str_eq_str(a: String, b: String) -> bool {
    a == b
}

#[builtin("!=")]
fn str_neq_str(a: String, b: String) -> bool {
    a != b
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
}
