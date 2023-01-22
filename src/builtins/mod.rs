use crate::eval::BuiltinIndex;
use shady_macros::setup_builtins;

mod bool;
use crate::builtins::bool::*;

mod int;
use crate::builtins::int::*;

mod list;
use crate::builtins::list::*;

mod os;
use crate::builtins::os::*;

mod str;
use crate::builtins::str::*;

setup_builtins!();
