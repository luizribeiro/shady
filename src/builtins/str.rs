use shady_macros::builtin;

#[builtin(==)]
fn str_eq_str(a: String, b: String) -> bool {
    a == b
}

#[builtin(!=)]
fn str_neq_str(a: String, b: String) -> bool {
    a != b
}
