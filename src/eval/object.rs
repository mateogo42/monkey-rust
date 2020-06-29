use crate::parser::{Identifier, Statement};
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
    Return(Box<Object>),
    Function(Vec<Identifier>, Box<Statement>, Box<Environment>),
    Null,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            outer: None,
            store: HashMap::new(),
        }
    }

    pub fn new_enclosed_by(environment: Self) -> Self {
        Self {
            outer: Some(Box::new(environment)),
            store: HashMap::new()
        }
    }

    pub fn get(&self, name: String) -> Option<Object> {
        match self.store.get(&name).as_ref() {
            Some(&Object::Int(value)) => Some(Object::Int(*value)),
            Some(&Object::Bool(value)) => Some(Object::Bool(*value)),
            Some(&Object::Function(parameters, body, environment)) => Some(Object::Function(parameters.clone(), body.clone(), environment.clone())),
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
}


impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Int(value) => format!("{}", value),
            Object::Bool(value) => format!("{}", value),
            Object::Return(value) => format!("{}", value.inspect()),
            _ => String::from("null"),
        }
    }
}
