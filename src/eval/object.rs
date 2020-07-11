use crate::parser::{Identifier, Statement};
use crate::eval::{get_builtins, BuiltinFunc};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    outer: Option<Box<Environment>>,
    pub store: HashMap<String, Object>
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(i32),
    Bool(bool),
    Str(String),
    Return(Box<Object>),
    Function(Vec<Identifier>, Box<Statement>),
    Builtin(BuiltinFunc),
    Null,
}

impl Environment {
    pub fn new() -> Self {
        let mut env = Self {
            outer: None,
            store: HashMap::new(),
        };
        env.add_builtins();
        env
    }

    pub fn new_enclosed_by(environment: Self) -> Self {
        Self {
            outer: Some(Box::new(environment)),
            store: HashMap::new()
        }
    }

    pub fn get(&self, name: String) -> Option<Object> {
        match self.store.get(&name) {
            Some(obj) => Some(obj.clone()),
            _ => {
                if let Some(env) = &self.outer {
                    return env.get(name)
                }
                None
            }
        }
    }

    pub fn set(&mut self, key: String, value: Object) {
        self.store.insert(key, value);
    }

    fn add_builtins(&mut self) {
        for (name, func) in get_builtins() {
            self.set(name, Object::Builtin(func))            
        }      
    }
}


impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Int(value) => format!("{}", value),
            Object::Bool(value) => format!("{}", value),
            Object::Str(value) => value.clone(),
            Object::Return(value) => format!("{}", value.inspect()),
            _ => String::from("null"),
        }
    }
}
