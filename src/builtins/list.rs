use crate::types::Value;
use shady_macros::builtin;

#[builtin]
fn add_all(list: Vec<i64>) -> i64 {
    let mut sum = 0;
    for i in list {
        sum += i;
    }
    sum
}

#[builtin]
fn first(list: Vec<Value>) -> Value {
    list[0].clone()
}

mod test {
    use super::*;

    #[test]
    fn test_add_all() {
        assert_eq!(add_all(vec![1, 2, 3]), 6);
    }

    #[test]
    fn test_first() {
        assert_eq!(
            first(vec![Value::Int(42), Value::Int(2), Value::Int(3)]),
            Value::Int(42)
        );
    }
}
