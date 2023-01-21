use shady_macros::{builtin, setup_builtins};

use crate::ast::{FnSignature, Parameter};
use crate::eval::{BuiltinAdder, BuiltinIndex, PrimitiveValue};

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

pub fn setup_builtins(builtins: &mut BuiltinIndex) {
    setup_builtins!();

    builtins.add("&&", |a: bool, b: bool| a && b);
    builtins.add("||", |a: bool, b: bool| a || b);
    builtins.add("==", |a: String, b: String| a == b);
    builtins.add("!=", |a: String, b: String| a != b);
    builtins.add("==", |a: bool, b: bool| a == b);
    builtins.add("!=", |a: bool, b: bool| a != b);

    builtins.add("==", |a: String, b: String| a == b);
    builtins.add("!=", |a: String, b: String| a != b);

    builtins.add("env", |name: String, default: String| {
        std::env::var(name).unwrap_or(default)
    });
}
