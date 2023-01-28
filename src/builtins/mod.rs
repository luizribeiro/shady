use crate::eval::BuiltinIndex;
use shady_macros::setup_builtins;

mod bool;
use crate::builtins::bool::*;

mod int;
use crate::builtins::int::*;

mod io;
use crate::builtins::io::*;

mod list;
use crate::builtins::list::*;

mod os;
use crate::builtins::os::*;

mod proc;
use crate::builtins::proc::*;

mod str;
use crate::builtins::str::*;

setup_builtins!();
