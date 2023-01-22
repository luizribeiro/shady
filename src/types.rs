#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Str,
    Bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Int(i64),
    Str(String),
    Bool(bool),
}

impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int,
            Value::Str(_) => Type::Str,
            Value::Bool(_) => Type::Bool,
        }
    }
}

pub trait PrimitiveValue {
    fn value_type() -> Type;
    fn from_value(value: Value) -> Self;
    fn to_value(&self) -> Value;
}

impl PrimitiveValue for i64 {
    fn value_type() -> Type {
        Type::Int
    }

    fn from_value(value: Value) -> Self {
        match value {
            Value::Int(i) => i,
            _ => panic!("Expected int value"),
        }
    }

    fn to_value(&self) -> Value {
        Value::Int(*self)
    }
}

impl PrimitiveValue for String {
    fn value_type() -> Type {
        Type::Str
    }

    fn from_value(value: Value) -> Self {
        match value {
            Value::Str(s) => s,
            _ => panic!("Expected string value"),
        }
    }

    fn to_value(&self) -> Value {
        Value::Str(self.clone())
    }
}

impl PrimitiveValue for bool {
    fn value_type() -> Type {
        Type::Bool
    }

    fn from_value(value: Value) -> Self {
        match value {
            Value::Bool(b) => b,
            _ => panic!("Expected bool value"),
        }
    }

    fn to_value(&self) -> Value {
        Value::Bool(*self)
    }
}

pub fn value_type<T: PrimitiveValue>() -> Type {
    <T>::value_type()
}

pub fn from_value<T: PrimitiveValue>(value: Value) -> T {
    <T>::from_value(value)
}

pub fn to_value<T: PrimitiveValue>(value: T) -> Value {
    <T>::to_value(&value)
}
