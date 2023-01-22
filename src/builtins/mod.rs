use crate::eval::BuiltinIndex;
use shady_macros::setup_builtins;

mod bool;
use crate::builtins::bool::*;

mod int;
use crate::builtins::int::*;

mod str;
use crate::builtins::str::*;

mod os;
use crate::builtins::os::*;

pub fn setup_builtins(builtins: &mut BuiltinIndex) {
    setup_builtins!();
}