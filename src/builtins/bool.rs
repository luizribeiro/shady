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
        assert_eq!(bool_and_bool(true, true), true);
        assert_eq!(bool_and_bool(true, false), false);
        assert_eq!(bool_and_bool(false, true), false);
        assert_eq!(bool_and_bool(false, false), false);
    }

    #[test]
    fn test_bool_or_bool() {
        assert_eq!(bool_or_bool(true, true), true);
        assert_eq!(bool_or_bool(true, false), true);
        assert_eq!(bool_or_bool(false, true), true);
        assert_eq!(bool_or_bool(false, false), false);
    }

    #[test]
    fn test_bool_eq_bool() {
        assert_eq!(bool_eq_bool(true, true), true);
        assert_eq!(bool_eq_bool(true, false), false);
        assert_eq!(bool_eq_bool(false, true), false);
        assert_eq!(bool_eq_bool(false, false), true);
    }
}
