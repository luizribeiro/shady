use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Str,
    Bool,
    List(Box<Type>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Int(i64),
    Str(String),
    Bool(bool),
    List {
        inner_type: Type,
        values: Vec<Value>,
    },
}

impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int,
            Value::Str(_) => Type::Str,
            Value::Bool(_) => Type::Bool,
            Value::List { inner_type, .. } => Type::List(Box::new(inner_type.clone())),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let out = match self {
            Value::Int(i) => i.to_string(),
            Value::Str(s) => s.clone(),
            Value::Bool(b) => b.to_string(),
            Value::List {
                inner_type: _,
                values,
            } => {
                let mut s = String::new();
                s.push('[');
                for (i, v) in values.iter().enumerate() {
                    s.push_str(&v.to_string());
                    if i < values.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s.push(']');
                s
            }
        };
        write!(f, "{out}")
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

impl<T: PrimitiveValue> PrimitiveValue for Vec<T> {
    fn value_type() -> Type {
        Type::List(Box::new(T::value_type()))
    }

    fn from_value(value: Value) -> Self {
        match value {
            Value::List { inner_type, values } => {
                assert_eq!(inner_type, T::value_type());
                values.into_iter().map(T::from_value).collect()
            }
            _ => panic!("Expected list value"),
        }
    }

    fn to_value(&self) -> Value {
        Value::List {
            inner_type: T::value_type(),
            values: self.iter().map(T::to_value).collect(),
        }
    }
}

impl PrimitiveValue for Value {
    fn value_type() -> Type {
        Type::List(Box::new(Type::Int))
    }

    fn from_value(value: Value) -> Self {
        value
    }

    fn to_value(&self) -> Value {
        self.clone()
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

pub fn from_string(typ: &Type, s: &str) -> Value {
    match typ {
        Type::Int => Value::Int(s.parse().expect("Expected int")),
        Type::Str => Value::Str(s.to_string()),
        Type::Bool => Value::Bool(s.parse().expect("Expected bool")),
        Type::List(_) => panic!("Cannot convert string to list"),
    }
}
