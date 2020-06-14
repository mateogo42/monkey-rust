
#[derive(Debug, PartialEq)]
pub enum Object {
    Int(i32),
    Bool(bool),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Int(value) => format!("{}", value),
            Object::Bool(value) => format!("{}", value),
            _ => String::new(),
        }
    }
}
