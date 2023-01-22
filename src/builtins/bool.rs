use shady_macros::builtin;

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
