use shady_macros::builtin;

#[builtin]
fn add_all(list: Vec<i64>) -> i64 {
    let mut sum = 0;
    for i in list {
        sum += i;
    }
    sum
}
