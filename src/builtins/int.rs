use shady_macros::builtin;

use crate::ast::{FnSignature, Parameter};
use crate::eval::{BuiltinIndex, PrimitiveValue};

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
