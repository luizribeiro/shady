use shady_macros::builtin;

use crate::ast::{FnSignature, Parameter};
use crate::eval::{BuiltinIndex, PrimitiveValue};

#[builtin(==)]
fn str_eq_str(a: String, b: String) -> bool {
    a == b
}

#[builtin(!=)]
fn str_neq_str(a: String, b: String) -> bool {
    a != b
}
