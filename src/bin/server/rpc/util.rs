use std::convert::TryFrom;

use jsonrpc_core::{Value, Error};

use rpc::Map;

pub fn describe_type(value: &Value) -> &'static str {
    match *value {
        Value::Null => "null",
        Value::Bool(..) => "bool",
        Value::Number(..) => "number",
        Value::String(..) => "string",
        Value::Array(..) => "array",
        Value::Object(..) => "object",
    }
}

pub fn get_usize(map: &mut Map, key: &str) -> Result<usize, Error> {
    let value = get_u64(map, key)?;
    let value = match usize::try_from(value) {
        Ok(value) => value,
        Err(_) => return Err(Error::invalid_params("value out of range")),
    };
    Ok(value)
}

pub fn get_u64(map: &mut Map, key: &str) -> Result<u64, Error> {
    let value = match map.remove(key) {
        Some(value) => value,
        None => {
            return Err(Error::invalid_params(format!("field '{}' missing", key)));
        },
    };

    let json = match value {
        Value::Number(json) => json,
        _ => {
            return Err(Error::invalid_params(format!("field '{}' must be a number (got a {})", key, describe_type(&value))));
        },
    };

    let ret = match json.as_u64() {
        Some(ret) => ret,
        None => {
            return Err(Error::invalid_params(format!("field '{}' is out of range ({})", key, json)));
        },
    };

    Ok(ret)
}

pub fn get_string(map: &mut Map, key: &str) -> Result<String, Error> {
    let value = match map.remove(key) {
        Some(value) => value,
        None => {
            return Err(Error::invalid_params(format!("field '{}' missing", key)));
        },
    };

    let ret = match value {
        Value::String(ret) => ret,
        _ => {
            return Err(Error::invalid_params(format!("field '{}' must be a string (got a {})", key, describe_type(&value))));
        },
    };

    Ok(ret)
}

