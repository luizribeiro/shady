use shady_macros::builtin;

#[builtin("+", infix = true)]
pub fn int_add_int(a: i64, b: i64) -> i64 {
    a + b
}

#[builtin("-", infix = true)]
pub fn int_sub_int(a: i64, b: i64) -> i64 {
    a - b
}

#[builtin("*", infix = true)]
pub fn int_mul_int(a: i64, b: i64) -> i64 {
    a * b
}

#[builtin("/", infix = true)]
pub fn int_div_int(a: i64, b: i64) -> i64 {
    a / b
}

#[builtin("%", infix = true)]
pub fn int_mod_int(a: i64, b: i64) -> i64 {
    a % b
}

#[builtin("^", infix = true)]
pub fn int_pow_int(a: i64, b: i64) -> i64 {
    a.pow(b as u32)
}

#[builtin("==", infix = true)]
pub fn int_eq_int(a: i64, b: i64) -> bool {
    a == b
}

#[builtin("!=", infix = true)]
pub fn int_neq_int(a: i64, b: i64) -> bool {
    a != b
}

#[builtin(">", infix = true)]
pub fn int_gt_int(a: i64, b: i64) -> bool {
    a > b
}

#[builtin(">=", infix = true)]
pub fn int_gte_int(a: i64, b: i64) -> bool {
    a >= b
}

#[builtin("<", infix = true)]
pub fn int_lt_int(a: i64, b: i64) -> bool {
    a < b
}

#[builtin("<=", infix = true)]
pub fn int_lte_int(a: i64, b: i64) -> bool {
    a <= b
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_int_add_int() {
        assert_eq!(int_add_int(1, 1), 2);
        assert_eq!(int_add_int(1, 2), 3);
        assert_eq!(int_add_int(2, 1), 3);
        assert_eq!(int_add_int(2, 2), 4);
    }

    #[test]
    fn test_int_sub_int() {
        assert_eq!(int_sub_int(1, 1), 0);
        assert_eq!(int_sub_int(1, 2), -1);
        assert_eq!(int_sub_int(2, 1), 1);
        assert_eq!(int_sub_int(2, 2), 0);
    }

    #[test]
    fn test_int_mul_int() {
        assert_eq!(int_mul_int(1, 1), 1);
        assert_eq!(int_mul_int(1, 2), 2);
        assert_eq!(int_mul_int(2, 1), 2);
        assert_eq!(int_mul_int(2, 2), 4);
    }

    #[test]
    fn test_int_div_int() {
        assert_eq!(int_div_int(1, 1), 1);
        assert_eq!(int_div_int(1, 2), 0);
        assert_eq!(int_div_int(2, 1), 2);
        assert_eq!(int_div_int(2, 2), 1);
    }

    #[test]
    fn test_int_mod_int() {
        assert_eq!(int_mod_int(1, 1), 0);
        assert_eq!(int_mod_int(1, 2), 1);
        assert_eq!(int_mod_int(2, 1), 0);
        assert_eq!(int_mod_int(2, 2), 0);
    }

    #[test]
    fn test_int_pow_int() {
        assert_eq!(int_pow_int(1, 1), 1);
        assert_eq!(int_pow_int(1, 2), 1);
        assert_eq!(int_pow_int(2, 1), 2);
        assert_eq!(int_pow_int(2, 2), 4);
    }

    #[test]
    fn test_int_eq_int() {
        assert!(int_eq_int(1, 1));
        assert!(!int_eq_int(1, 2));
        assert!(!int_eq_int(2, 1));
        assert!(int_eq_int(2, 2));
    }

    #[test]
    fn test_int_neq_int() {
        assert!(!int_neq_int(1, 1));
        assert!(int_neq_int(1, 2));
        assert!(int_neq_int(2, 1));
        assert!(!int_neq_int(2, 2));
    }

    #[test]
    fn test_int_gt_int() {
        assert!(!int_gt_int(1, 1));
        assert!(!int_gt_int(1, 2));
        assert!(int_gt_int(2, 1));
        assert!(!int_gt_int(2, 2));
    }

    #[test]
    fn test_int_gte_int() {
        assert!(int_gte_int(1, 1));
        assert!(!int_gte_int(1, 2));
        assert!(int_gte_int(2, 1));
        assert!(int_gte_int(2, 2));
    }

    #[test]
    fn test_int_lt_int() {
        assert!(!int_lt_int(1, 1));
        assert!(int_lt_int(1, 2));
        assert!(!int_lt_int(2, 1));
        assert!(!int_lt_int(2, 2));
    }

    #[test]
    fn test_int_lte_int() {
        assert!(int_lte_int(1, 1));
        assert!(int_lte_int(1, 2));
        assert!(!int_lte_int(2, 1));
        assert!(int_lte_int(2, 2));
    }
}
