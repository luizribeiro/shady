use crate::error::{Result, ShadyError};
use miette::SourceSpan;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Debug, Clone, Eq)]
pub enum Type {
    Int,
    Str,
    Bool,
    List(Box<Type>),
    Proc,
    Any,
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Type::Int => state.write_u8(0),
            Type::Str => state.write_u8(1),
            Type::Bool => state.write_u8(2),
            Type::List(t) => {
                state.write_u8(3);
                t.hash(state);
            }
            Type::Proc => state.write_u8(4),
            Type::Any => state.write_u8(5),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Int, Type::Int) => true,
            (Type::Str, Type::Str) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::List(a), Type::List(b)) => a == b,
            (Type::Proc, Type::Proc) => true,
            (Type::Any, _) => true,
            (_, Type::Any) => true,
            _ => false,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Str => write!(f, "str"),
            Type::Bool => write!(f, "bool"),
            Type::List(t) => write!(f, "[{}]", t),
            Type::Proc => write!(f, "proc"),
            Type::Any => write!(f, "any"),
        }
    }
}

#[derive(Debug)]
pub struct Proc {
    pub child: Rc<RefCell<std::process::Child>>,
    pub program: String,
    pub args: Vec<String>,
    pub stdin_writer: os_pipe::PipeWriter,
    pub stdout_reader: os_pipe::PipeReader,
    pub stderr_reader: os_pipe::PipeReader,
}

impl Eq for Proc {}

impl Clone for Proc {
    fn clone(&self) -> Self {
        Proc {
            child: self.child.clone(),
            program: self.program.clone(),
            args: self.args.clone(),
            stdin_writer: self.stdin_writer.try_clone().expect(
                "Failed to clone stdin pipe - this is likely a system resource limit issue",
            ),
            stdout_reader: self.stdout_reader.try_clone().expect(
                "Failed to clone stdout pipe - this is likely a system resource limit issue",
            ),
            stderr_reader: self.stderr_reader.try_clone().expect(
                "Failed to clone stderr pipe - this is likely a system resource limit issue",
            ),
        }
    }
}

impl PartialEq for Proc {
    fn eq(&self, other: &Self) -> bool {
        self.program == other.program && self.args == other.args
    }
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
    Proc(Proc),
}

impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int,
            Value::Str(_) => Type::Str,
            Value::Bool(_) => Type::Bool,
            Value::List { inner_type, .. } => Type::List(Box::new(inner_type.clone())),
            Value::Proc { .. } => Type::Proc,
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
            Value::Proc { .. } => "<proc object>".to_string(),
        };
        write!(f, "{out}")
    }
}

pub trait PrimitiveValue: Sized {
    fn value_type() -> Type;
    fn from_value(value: Value) -> Result<Self>;
    fn to_value(&self) -> Value;
}

impl PrimitiveValue for i64 {
    fn value_type() -> Type {
        Type::Int
    }

    fn from_value(value: Value) -> Result<Self> {
        match value {
            Value::Int(i) => Ok(i),
            _ => Err(ShadyError::TypeMismatch {
                expected: "int".to_string(),
                actual: format!("{:?}", value.get_type()),
                span: SourceSpan::from(0..0),
            }),
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

    fn from_value(value: Value) -> Result<Self> {
        match value {
            Value::Str(s) => Ok(s),
            _ => Err(ShadyError::TypeMismatch {
                expected: "str".to_string(),
                actual: format!("{:?}", value.get_type()),
                span: SourceSpan::from(0..0),
            }),
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

    fn from_value(value: Value) -> Result<Self> {
        match value {
            Value::Bool(b) => Ok(b),
            _ => Err(ShadyError::TypeMismatch {
                expected: "bool".to_string(),
                actual: format!("{:?}", value.get_type()),
                span: SourceSpan::from(0..0),
            }),
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

    fn from_value(value: Value) -> Result<Self> {
        match value {
            Value::List { inner_type, values } => {
                if inner_type != T::value_type() {
                    return Err(ShadyError::TypeMismatch {
                        expected: format!("[{}]", T::value_type()),
                        actual: format!("[{:?}]", inner_type),
                        span: SourceSpan::from(0..0),
                    });
                }
                values
                    .into_iter()
                    .map(T::from_value)
                    .collect::<Result<Vec<T>>>()
            }
            _ => Err(ShadyError::TypeMismatch {
                expected: "list".to_string(),
                actual: format!("{:?}", value.get_type()),
                span: SourceSpan::from(0..0),
            }),
        }
    }

    fn to_value(&self) -> Value {
        Value::List {
            inner_type: T::value_type(),
            values: self.iter().map(T::to_value).collect(),
        }
    }
}

impl PrimitiveValue for Proc {
    fn value_type() -> Type {
        Type::Proc
    }

    fn from_value(value: Value) -> Result<Self> {
        match value {
            Value::Proc(p) => Ok(p),
            _ => Err(ShadyError::TypeMismatch {
                expected: "proc".to_string(),
                actual: format!("{:?}", value.get_type()),
                span: SourceSpan::from(0..0),
            }),
        }
    }

    fn to_value(&self) -> Value {
        Value::Proc(self.clone())
    }
}

impl PrimitiveValue for Value {
    fn value_type() -> Type {
        Type::Any
    }

    fn from_value(value: Value) -> Result<Self> {
        Ok(value)
    }

    fn to_value(&self) -> Value {
        self.clone()
    }
}

pub fn value_type<T: PrimitiveValue>() -> Type {
    <T>::value_type()
}

pub fn from_value<T: PrimitiveValue>(value: Value) -> Result<T> {
    <T>::from_value(value)
}

pub fn to_value<T: PrimitiveValue>(value: T) -> Value {
    <T>::to_value(&value)
}

pub fn from_string(typ: &Type, s: &str) -> Result<Value> {
    match typ {
        Type::Int => s
            .parse::<i64>()
            .map(Value::Int)
            .map_err(|_| ShadyError::InvalidConversion {
                from: format!("string '{}'", s),
                to: "int".to_string(),
            }),
        Type::Str => Ok(Value::Str(s.to_string())),
        Type::Bool => {
            s.parse::<bool>()
                .map(Value::Bool)
                .map_err(|_| ShadyError::InvalidConversion {
                    from: format!("string '{}'", s),
                    to: "bool".to_string(),
                })
        }
        Type::List(inner_type) => {
            // Empty string means empty list
            if s.trim().is_empty() {
                return Ok(Value::List {
                    inner_type: (**inner_type).clone(),
                    values: vec![],
                });
            }

            // Parse comma-separated values
            let values: Result<Vec<Value>> = s
                .split(',')
                .map(|item| from_string(inner_type, item.trim()))
                .collect();

            Ok(Value::List {
                inner_type: (**inner_type).clone(),
                values: values?,
            })
        }
        Type::Proc => Err(ShadyError::InvalidConversion {
            from: "string".to_string(),
            to: "proc".to_string(),
        }),
        Type::Any => Err(ShadyError::InvalidConversion {
            from: "string".to_string(),
            to: "any".to_string(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_string_list_int() {
        let typ = Type::List(Box::new(Type::Int));
        let result = from_string(&typ, "1,2,3").unwrap();

        match result {
            Value::List { inner_type, values } => {
                assert_eq!(inner_type, Type::Int);
                assert_eq!(values.len(), 3);
                assert_eq!(values[0], Value::Int(1));
                assert_eq!(values[1], Value::Int(2));
                assert_eq!(values[2], Value::Int(3));
            }
            _ => panic!("Expected List value"),
        }
    }

    #[test]
    fn test_from_string_list_str() {
        let typ = Type::List(Box::new(Type::Str));
        let result = from_string(&typ, "hello,world,test").unwrap();

        match result {
            Value::List { inner_type, values } => {
                assert_eq!(inner_type, Type::Str);
                assert_eq!(values.len(), 3);
                assert_eq!(values[0], Value::Str("hello".to_string()));
                assert_eq!(values[1], Value::Str("world".to_string()));
                assert_eq!(values[2], Value::Str("test".to_string()));
            }
            _ => panic!("Expected List value"),
        }
    }

    #[test]
    fn test_from_string_list_with_spaces() {
        let typ = Type::List(Box::new(Type::Int));
        let result = from_string(&typ, "1, 2, 3").unwrap();

        match result {
            Value::List { inner_type, values } => {
                assert_eq!(inner_type, Type::Int);
                assert_eq!(values.len(), 3);
                assert_eq!(values[0], Value::Int(1));
                assert_eq!(values[1], Value::Int(2));
                assert_eq!(values[2], Value::Int(3));
            }
            _ => panic!("Expected List value"),
        }
    }

    #[test]
    fn test_from_string_empty_list() {
        let typ = Type::List(Box::new(Type::Int));
        let result = from_string(&typ, "").unwrap();

        match result {
            Value::List { inner_type, values } => {
                assert_eq!(inner_type, Type::Int);
                assert_eq!(values.len(), 0);
            }
            _ => panic!("Expected List value"),
        }
    }

    #[test]
    fn test_from_string_list_invalid_element() {
        let typ = Type::List(Box::new(Type::Int));
        let result = from_string(&typ, "1,invalid,3");

        assert!(result.is_err());
    }
}
