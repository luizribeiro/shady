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
