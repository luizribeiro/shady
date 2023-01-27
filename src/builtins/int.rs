use shady_macros::builtin;

#[builtin(+)]
pub fn int_add_int(a: i64, b: i64) -> i64 {
    a + b
}

#[builtin(-)]
pub fn int_sub_int(a: i64, b: i64) -> i64 {
    a - b
}

#[builtin(*)]
pub fn int_mul_int(a: i64, b: i64) -> i64 {
    a * b
}

#[builtin(/)]
pub fn int_div_int(a: i64, b: i64) -> i64 {
    a / b
}

#[builtin(%)]
pub fn int_mod_int(a: i64, b: i64) -> i64 {
    a % b
}

#[builtin(^)]
pub fn int_pow_int(a: i64, b: i64) -> i64 {
    a.pow(b as u32)
}

#[builtin(==)]
pub fn int_eq_int(a: i64, b: i64) -> bool {
    a == b
}

#[builtin(!=)]
pub fn int_neq_int(a: i64, b: i64) -> bool {
    a != b
}

#[builtin(>)]
pub fn int_gt_int(a: i64, b: i64) -> bool {
    a > b
}

#[builtin(>=)]
pub fn int_gte_int(a: i64, b: i64) -> bool {
    a >= b
}

#[builtin(<)]
pub fn int_lt_int(a: i64, b: i64) -> bool {
    a < b
}

#[builtin(<=)]
pub fn int_lte_int(a: i64, b: i64) -> bool {
    a <= b
}

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
        assert_eq!(int_eq_int(1, 1), true);
        assert_eq!(int_eq_int(1, 2), false);
        assert_eq!(int_eq_int(2, 1), false);
        assert_eq!(int_eq_int(2, 2), true);
    }

    #[test]
    fn test_int_neq_int() {
        assert_eq!(int_neq_int(1, 1), false);
        assert_eq!(int_neq_int(1, 2), true);
        assert_eq!(int_neq_int(2, 1), true);
        assert_eq!(int_neq_int(2, 2), false);
    }

    #[test]
    fn test_int_gt_int() {
        assert_eq!(int_gt_int(1, 1), false);
        assert_eq!(int_gt_int(1, 2), false);
        assert_eq!(int_gt_int(2, 1), true);
        assert_eq!(int_gt_int(2, 2), false);
    }

    #[test]
    fn test_int_gte_int() {
        assert_eq!(int_gte_int(1, 1), true);
        assert_eq!(int_gte_int(1, 2), false);
        assert_eq!(int_gte_int(2, 1), true);
        assert_eq!(int_gte_int(2, 2), true);
    }

    #[test]
    fn test_int_lt_int() {
        assert_eq!(int_lt_int(1, 1), false);
        assert_eq!(int_lt_int(1, 2), true);
        assert_eq!(int_lt_int(2, 1), false);
        assert_eq!(int_lt_int(2, 2), false);
    }

    #[test]
    fn test_int_lte_int() {
        assert_eq!(int_lte_int(1, 1), true);
        assert_eq!(int_lte_int(1, 2), true);
        assert_eq!(int_lte_int(2, 1), false);
        assert_eq!(int_lte_int(2, 2), true);
    }
}
