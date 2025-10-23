use shady_macros::builtin;

#[builtin("&&", infix = true)]
fn bool_and_bool(a: bool, b: bool) -> bool {
    a && b
}

#[builtin("||", infix = true)]
fn bool_or_bool(a: bool, b: bool) -> bool {
    a || b
}

#[builtin("==", infix = true)]
fn bool_eq_bool(a: bool, b: bool) -> bool {
    a == b
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_bool_and_bool() {
        assert!(bool_and_bool(true, true));
        assert!(!bool_and_bool(true, false));
        assert!(!bool_and_bool(false, true));
        assert!(!bool_and_bool(false, false));
    }

    #[test]
    fn test_bool_or_bool() {
        assert!(bool_or_bool(true, true));
        assert!(bool_or_bool(true, false));
        assert!(bool_or_bool(false, true));
        assert!(!bool_or_bool(false, false));
    }

    #[test]
    fn test_bool_eq_bool() {
        assert!(bool_eq_bool(true, true));
        assert!(!bool_eq_bool(true, false));
        assert!(!bool_eq_bool(false, true));
        assert!(bool_eq_bool(false, false));
    }
}
