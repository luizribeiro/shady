use shady_macros::builtin;

#[builtin]
pub fn print(s: String) -> i64 {
    println!("{s}");
    0
}
