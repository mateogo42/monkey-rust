use std::collections::HashMap;
use crate::eval::Object;

pub type BuiltinFunc = fn(Vec<Object>) -> Option<Object>;

pub fn get_builtins() -> HashMap<String, BuiltinFunc> {
    let mut builtins: HashMap<String, BuiltinFunc> = HashMap::new();
    builtins.insert("len".to_string(), builtin_len);

    builtins
}

fn builtin_len(args: Vec<Object>) -> Option<Object> {
    let obj = &args[0];
    match obj {
        Object::Str(value) => Some(Object::Int(value.len() as i32)),
        _ => None
    }
}