use std::collections::HashMap;
use crate::eval::Object;

type BuiltinResult<T> = Result<T, String>;
pub type BuiltinFunc = fn(Vec<Object>) -> BuiltinResult<Object>;

pub fn get_builtins() -> HashMap<String, BuiltinFunc> {
    let mut builtins: HashMap<String, BuiltinFunc> = HashMap::new();
    builtins.insert("len".to_string(), builtin_len);
    builtins
}

fn builtin_len(args: Vec<Object>) -> BuiltinResult<Object> {
    let obj = &args[0];
    match obj {
        Object::Str(value) => Ok(Object::Int(value.len() as i32)),
        _ => Err(format!("Len cannot be called on type {:?}", obj))
    }
}

