use shady_macros::{builtin, setup_builtins};

use crate::ast::{FnSignature, Parameter};
use crate::eval::{BuiltinIndex, PrimitiveValue};

#[builtin(+)]
fn int_add_int(a: i64, b: i64) -> i64 {
    a + b
}

#[builtin(-)]
fn int_sub_int(a: i64, b: i64) -> i64 {
    a - b
}

#[builtin(*)]
fn int_mul_int(a: i64, b: i64) -> i64 {
    a * b
}

#[builtin(/)]
fn int_div_int(a: i64, b: i64) -> i64 {
    a / b
}

#[builtin(%)]
fn int_mod_int(a: i64, b: i64) -> i64 {
    a % b
}

#[builtin(^)]
fn int_pow_int(a: i64, b: i64) -> i64 {
    a.pow(b as u32)
}

#[builtin(==)]
fn int_eq_int(a: i64, b: i64) -> bool {
    a == b
}

#[builtin(!=)]
fn int_neq_int(a: i64, b: i64) -> bool {
    a != b
}

#[builtin(>)]
fn int_gt_int(a: i64, b: i64) -> bool {
    a > b
}

#[builtin(>=)]
fn int_gte_int(a: i64, b: i64) -> bool {
    a >= b
}

#[builtin(<)]
fn int_lt_int(a: i64, b: i64) -> bool {
    a < b
}

#[builtin(<=)]
fn int_lte_int(a: i64, b: i64) -> bool {
    a <= b
}

#[builtin(&&)]
fn bool_and_bool(a: bool, b: bool) -> bool {
    a && b
}

#[builtin(||)]
fn bool_or_bool(a: bool, b: bool) -> bool {
    a || b
}

#[builtin(==)]
fn bool_eq_bool(a: bool, b: bool) -> bool {
    a == b
}

#[builtin(==)]
fn str_eq_str(a: String, b: String) -> bool {
    a == b
}

#[builtin(!=)]
fn str_neq_str(a: String, b: String) -> bool {
    a != b
}

#[builtin]
fn env(var_name: String, default: String) -> String {
    std::env::var(var_name).unwrap_or(default)
}

pub fn setup_builtins(builtins: &mut BuiltinIndex) {
    setup_builtins!();
}
