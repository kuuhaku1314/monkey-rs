use crate::error::{Error, ErrorKind};
use crate::lexer::Span;
use crate::map_key::AbleToMapKey;
use crate::token::Token;
use crate::vm::heap::VmHeap;
use crate::vm::runtime::value::{
    Value, VmListHandle, VmStructInstance, VmStructInstanceHandle, VmStructType,
};
use crate::vm::symbols::{SymbolId, SymbolInterner};
use std::collections::{HashMap, HashSet};

pub(in crate::vm) fn eval_prefix_value(
    token: &Token,
    value: Value,
    span: Span,
) -> Result<Value, Error> {
    match token {
        Token::Bang => match value {
            Value::Boolean(value) => Ok(Value::Boolean(!value)),
            _ => Err(operation_type_error(span)),
        },
        Token::Minus => match value {
            Value::Integer(value) => Ok(Value::Integer(-value)),
            Value::Float(value) => Ok(Value::Float(-value)),
            _ => Err(operation_type_error(span)),
        },
        _ => Err(operation_type_error(span)),
    }
}

pub(in crate::vm) fn eval_infix_value(
    heap: &mut VmHeap,
    token: &Token,
    left: Value,
    right: Value,
    span: Span,
) -> Result<Value, Error> {
    match (left, right) {
        (Value::Integer(left), Value::Integer(right)) => {
            eval_value_integer_infix(token, left, right, span)
        }
        (Value::Float(left), Value::Float(right)) => {
            eval_value_float_infix(token, left, right, span)
        }
        (Value::Integer(left), Value::Float(right)) => {
            eval_value_mixed_numeric_infix(token, left as f64, right, span)
        }
        (Value::Float(left), Value::Integer(right)) => {
            eval_value_mixed_numeric_infix(token, left, right as f64, span)
        }
        (Value::String(left), Value::String(right)) => eval_value_string_infix(
            heap,
            token,
            heap.string(left).to_string(),
            heap.string(right).to_string(),
            span,
        ),
        (Value::String(left), Value::Integer(right)) if matches!(token, Token::Plus) => {
            Ok(heap.alloc_string(format!("{}{}", heap.string(left), right)))
        }
        (Value::Integer(left), Value::String(right)) if matches!(token, Token::Plus) => {
            Ok(heap.alloc_string(format!("{}{}", left, heap.string(right))))
        }
        (Value::String(left), Value::Float(right)) if matches!(token, Token::Plus) => {
            Ok(heap.alloc_string(format!("{}{}", heap.string(left), right)))
        }
        (Value::Float(left), Value::String(right)) if matches!(token, Token::Plus) => {
            Ok(heap.alloc_string(format!("{}{}", left, heap.string(right))))
        }
        (Value::Boolean(left), Value::Boolean(right)) => {
            eval_value_compare(token, left, right, span)
        }
        (Value::Null, Value::Null) => match token {
            Token::EQ => Ok(Value::Boolean(true)),
            Token::NotEq => Ok(Value::Boolean(false)),
            _ => Err(operation_type_error(span)),
        },
        _ => Err(operation_type_error(span)),
    }
}

fn eval_value_integer_infix(
    token: &Token,
    left: i64,
    right: i64,
    span: Span,
) -> Result<Value, Error> {
    let value = match token {
        Token::Plus => Value::Integer(left + right),
        Token::Minus => Value::Integer(left - right),
        Token::Asterisk => Value::Integer(left * right),
        Token::Slash => {
            if right == 0 {
                return Err(Error::with_kind_span(
                    ErrorKind::Runtime,
                    "division by zero".to_string(),
                    span,
                ));
            }
            Value::Integer(left / right)
        }
        Token::EQ => Value::Boolean(left == right),
        Token::NotEq => Value::Boolean(left != right),
        Token::LT => Value::Boolean(left < right),
        Token::Lte => Value::Boolean(left <= right),
        Token::GT => Value::Boolean(left > right),
        Token::Gte => Value::Boolean(left >= right),
        _ => return Err(operation_type_error(span)),
    };
    Ok(value)
}

fn eval_value_float_infix(
    token: &Token,
    left: f64,
    right: f64,
    span: Span,
) -> Result<Value, Error> {
    let value = match token {
        Token::Plus => Value::Float(left + right),
        Token::Minus => Value::Float(left - right),
        Token::Asterisk => Value::Float(left * right),
        Token::Slash => {
            if right == 0.0 {
                return Err(Error::with_kind_span(
                    ErrorKind::Runtime,
                    "division by zero".to_string(),
                    span,
                ));
            }
            Value::Float(left / right)
        }
        Token::EQ => Value::Boolean(left == right),
        Token::NotEq => Value::Boolean(left != right),
        Token::LT => Value::Boolean(left < right),
        Token::Lte => Value::Boolean(left <= right),
        Token::GT => Value::Boolean(left > right),
        Token::Gte => Value::Boolean(left >= right),
        _ => return Err(operation_type_error(span)),
    };
    Ok(value)
}

fn eval_value_mixed_numeric_infix(
    token: &Token,
    left: f64,
    right: f64,
    span: Span,
) -> Result<Value, Error> {
    let value = match token {
        Token::Plus => Value::Float(left + right),
        Token::Minus => Value::Float(left - right),
        Token::Asterisk => Value::Float(left * right),
        Token::Slash => {
            if right == 0.0 {
                return Err(Error::with_kind_span(
                    ErrorKind::Runtime,
                    "division by zero".to_string(),
                    span,
                ));
            }
            Value::Float(left / right)
        }
        _ => return Err(operation_type_error(span)),
    };
    Ok(value)
}

fn eval_value_string_infix(
    heap: &mut VmHeap,
    token: &Token,
    left: String,
    right: String,
    span: Span,
) -> Result<Value, Error> {
    match token {
        Token::Plus => Ok(heap.alloc_string(left + right.as_str())),
        Token::EQ => Ok(Value::Boolean(left == right)),
        Token::NotEq => Ok(Value::Boolean(left != right)),
        Token::LT => Ok(Value::Boolean(left < right)),
        Token::Lte => Ok(Value::Boolean(left <= right)),
        Token::GT => Ok(Value::Boolean(left > right)),
        Token::Gte => Ok(Value::Boolean(left >= right)),
        _ => Err(operation_type_error(span)),
    }
}

fn eval_value_compare<T>(token: &Token, left: T, right: T, span: Span) -> Result<Value, Error>
where
    T: PartialEq + PartialOrd,
{
    match token {
        Token::EQ => Ok(Value::Boolean(left == right)),
        Token::NotEq => Ok(Value::Boolean(left != right)),
        Token::LT => Ok(Value::Boolean(left < right)),
        Token::Lte => Ok(Value::Boolean(left <= right)),
        Token::GT => Ok(Value::Boolean(left > right)),
        Token::Gte => Ok(Value::Boolean(left >= right)),
        _ => Err(operation_type_error(span)),
    }
}

pub(in crate::vm) fn eval_index(
    heap: &mut VmHeap,
    left: Value,
    index: Value,
    span: Span,
) -> Result<Value, Error> {
    match left {
        Value::Slice(values) => match index {
            Value::Integer(index) => get_vm_list(heap, values, index, span),
            _ => Err(operation_type_error(span)),
        },
        Value::Map(values) => {
            let key = value_to_map_key(heap, index, span)?;
            Ok(heap.map(values).get(&key).cloned().unwrap_or(Value::Null))
        }
        Value::StructInstance(instance) => {
            let key = value_to_map_key(heap, index, span)?;
            vm_struct_get(heap, instance, key, span)
        }
        Value::String(value) => match index {
            Value::Integer(index) => Ok(heap.alloc_string(
                heap.string(value)
                    .chars()
                    .nth(index as usize)
                    .ok_or_else(|| {
                        Error::with_kind_span(
                            ErrorKind::Index,
                            "index out of bound".to_string(),
                            span,
                        )
                    })?
                    .to_string(),
            )),
            _ => Err(operation_type_error(span)),
        },
        _ => Err(operation_type_error(span)),
    }
}

fn get_vm_list(
    heap: &VmHeap,
    values: VmListHandle,
    index: i64,
    span: Span,
) -> Result<Value, Error> {
    let values = heap.list(values);
    if index < 0 || values.len() <= index as usize {
        return Err(Error::with_kind_span(
            ErrorKind::Index,
            "index out of bound".to_string(),
            span,
        ));
    }
    Ok(values[index as usize].clone())
}

pub(in crate::vm) fn eval_member_value_with_key(
    heap: &VmHeap,
    left: Value,
    property: SymbolId,
    property_name: &str,
    property_key: &AbleToMapKey,
    span: Span,
) -> Result<Value, Error> {
    match left {
        Value::Module(module) => heap
            .module(module)
            .indexes
            .get(&property)
            .and_then(|index| heap.module(module).values.get(*index))
            .cloned()
            .ok_or_else(|| {
                Error::with_kind_span(
                    ErrorKind::Name,
                    format!("unknown module member '{property_name}'"),
                    span,
                )
            }),
        Value::Map(map) => Ok(heap
            .map(map)
            .get(property_key)
            .cloned()
            .unwrap_or(Value::Null)),
        Value::StructInstance(instance) => {
            vm_struct_get(heap, instance, property_key.clone(), span)
        }
        _ => Err(operation_type_error(span)),
    }
}

pub(in crate::vm) fn pop_struct_type(
    heap: &VmHeap,
    stack: &mut Vec<Value>,
    span: Span,
) -> Result<VmStructType, Error> {
    let value = stack.pop().ok_or_else(|| {
        Error::with_kind_span(ErrorKind::Runtime, "vm stack underflow".to_string(), span)
    })?;
    match value {
        Value::StructType(struct_type) => Ok(heap.struct_type(struct_type).clone()),
        value => Err(Error::with_kind_span(
            ErrorKind::Type,
            format!("{} is not a struct", value.type_name()),
            span,
        )),
    }
}

pub(in crate::vm) fn make_struct_positional(
    heap: &mut VmHeap,
    struct_type: VmStructType,
    values: Vec<Value>,
    span: Span,
) -> Result<Value, Error> {
    if values.len() != struct_type.fields.len() {
        return Err(Error::with_kind_span(
            ErrorKind::Type,
            format!(
                "struct {} expects {} values, got {}",
                struct_type.name,
                struct_type.fields.len(),
                values.len()
            ),
            span,
        ));
    }
    let mut fields = HashMap::new();
    for (field, value) in struct_type.fields.iter().zip(values) {
        fields.insert(field.to_string().into(), value);
    }
    Ok(heap.alloc_struct_instance(VmStructInstance {
        type_name: struct_type.name.to_string(),
        fields: struct_type.fields.clone(),
        values: fields,
    }))
}

pub(in crate::vm) fn make_struct_named(
    heap: &mut VmHeap,
    struct_type: VmStructType,
    fields: &[SymbolId],
    values: Vec<Value>,
    interner: &SymbolInterner,
    span: Span,
) -> Result<Value, Error> {
    let mut result = HashMap::new();
    for field in &struct_type.fields {
        result.insert(field.to_string().into(), Value::Null);
    }
    let mut assigned_fields = HashSet::new();
    for (field_id, value) in fields.iter().zip(values) {
        let field = interner.name(*field_id);
        if !struct_type.fields.iter().any(|allowed| allowed == field) {
            return Err(Error::with_kind_span(
                ErrorKind::Name,
                format!("unknown field '{}' for struct {}", field, struct_type.name),
                span,
            ));
        }
        if !assigned_fields.insert(*field_id) {
            return Err(Error::with_kind_span(
                ErrorKind::Name,
                format!(
                    "duplicate field '{}' in struct literal {}",
                    field, struct_type.name
                ),
                span,
            ));
        }
        result.insert(field.to_string().into(), value);
    }
    Ok(heap.alloc_struct_instance(VmStructInstance {
        type_name: struct_type.name.to_string(),
        fields: struct_type.fields.clone(),
        values: result,
    }))
}

pub(in crate::vm) fn set_index(
    heap: &mut VmHeap,
    left: Value,
    index: Value,
    value: Value,
    span: Span,
) -> Result<(), Error> {
    match left {
        Value::Slice(values) => match index {
            Value::Integer(index) => set_vm_list(heap, values, index, value, span),
            _ => Err(operation_type_error(span)),
        },
        Value::Map(values) => {
            let key = value_to_map_key(heap, index, span)?;
            heap.map_mut(values).insert(key, value);
            Ok(())
        }
        Value::StructInstance(instance) => {
            let key = value_to_map_key(heap, index, span)?;
            vm_struct_set(heap, instance, key, value, span)
        }
        _ => Err(operation_type_error(span)),
    }
}

fn set_vm_list(
    heap: &mut VmHeap,
    values: VmListHandle,
    index: i64,
    value: Value,
    span: Span,
) -> Result<(), Error> {
    let mut values = heap.list_mut(values);
    if index < 0 || values.len() <= index as usize {
        return Err(Error::with_kind_span(
            ErrorKind::Index,
            "index out of bound".to_string(),
            span,
        ));
    }
    values[index as usize] = value;
    Ok(())
}

pub(in crate::vm) fn set_member(
    heap: &mut VmHeap,
    left: Value,
    property: &str,
    value: Value,
    span: Span,
) -> Result<(), Error> {
    match left {
        Value::Map(map) => {
            heap.map_mut(map).insert(property.into(), value);
            Ok(())
        }
        Value::StructInstance(instance) => {
            vm_struct_set(heap, instance, property.into(), value, span)
        }
        _ => Err(operation_type_error(span)),
    }
}

fn value_to_map_key(heap: &VmHeap, value: Value, span: Span) -> Result<AbleToMapKey, Error> {
    match value {
        Value::Integer(value) => Ok(AbleToMapKey::Integer(value)),
        Value::Boolean(value) => Ok(AbleToMapKey::Boolean(value)),
        Value::String(value) => Ok(AbleToMapKey::String(heap.string(value).to_string())),
        _ => Err(Error::with_kind_span(
            ErrorKind::Type,
            "unable as map key".to_string(),
            span,
        )),
    }
}

fn vm_struct_get(
    heap: &VmHeap,
    instance: VmStructInstanceHandle,
    key: AbleToMapKey,
    span: Span,
) -> Result<Value, Error> {
    let instance = heap.struct_instance(instance);
    let field = match &key {
        AbleToMapKey::String(field) => field,
        _ => {
            return Err(Error::with_kind_span(
                ErrorKind::Type,
                "struct field must be string".to_string(),
                span,
            ));
        }
    };
    if !instance.fields.iter().any(|allowed| allowed == field) {
        return Err(Error::with_kind_span(
            ErrorKind::Name,
            format!(
                "unknown field '{}' for struct {}",
                field, instance.type_name
            ),
            span,
        ));
    }
    Ok(instance.values.get(&key).cloned().unwrap_or(Value::Null))
}

fn vm_struct_set(
    heap: &mut VmHeap,
    instance: VmStructInstanceHandle,
    key: AbleToMapKey,
    value: Value,
    span: Span,
) -> Result<(), Error> {
    let mut instance = heap.struct_instance_mut(instance);
    let field = match &key {
        AbleToMapKey::String(field) => field,
        _ => {
            return Err(Error::with_kind_span(
                ErrorKind::Type,
                "struct field must be string".to_string(),
                span,
            ));
        }
    };
    if !instance.fields.iter().any(|allowed| allowed == field) {
        return Err(Error::with_kind_span(
            ErrorKind::Name,
            format!(
                "unknown field '{}' for struct {}",
                field, instance.type_name
            ),
            span,
        ));
    }
    instance.values.insert(key, value);
    Ok(())
}

pub(in crate::vm) fn bool_condition(value: Value, span: Span) -> Result<bool, Error> {
    match value {
        Value::Boolean(value) => Ok(value),
        _ => Err(operation_type_error(span)),
    }
}

pub(in crate::vm) fn operation_type_error(span: Span) -> Error {
    Error::with_kind_span(ErrorKind::Type, "operation type invalid".to_string(), span)
}
