use crate::ast::{
    BlockStatement, CallExpression, Expression, ExpressionStatement, ForInStatement,
    FunctionLiteral, Identifier, IfExpression, IndexAssignStatement, IndexExpression,
    InfixExpression, LetStatement, MapLiteral, PrefixExpression, Program, ReturnStatement,
    SliceLiteral, Statement, WhileStatement,
};
use crate::buildin::new_builtin_function_map;
use crate::error::Error;
use crate::lexer::Position;
use crate::object::{enter_env, from_env, to_env, Env, Object};
use crate::token::Token;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::LazyLock;

pub type BuildInFn = fn(&[Object]) -> Result<Object, Error>;
pub static BUILD_IN_FUNCTIONS: LazyLock<
    HashMap<&'static str, BuildInFn>,
> = LazyLock::new(|| new_builtin_function_map());

pub fn eval(program: &Program, env: Env) -> Result<Object, Error> {
    eval_statements(&program.statements, true, env)
}

fn eval_statement(stmt: &Statement, env: Env) -> Result<Object, Error> {
    match stmt {
        Statement::Block(stmt) => eval_block_statement(stmt, enter_env(Rc::clone(&env))),
        Statement::Empty(_) => Ok(Object::Empty),
        Statement::Expression(stmt) => eval_expression_statement(stmt, Rc::clone(&env)),
        Statement::Let(stmt) => eval_let_statement(stmt, Rc::clone(&env)),
        Statement::Return(stmt) => eval_return_statement(stmt, Rc::clone(&env)),
        Statement::While(stmt) => eval_while_statement(stmt, Rc::clone(&env)),
        Statement::IndexAssign(stmt) => eval_index_assign_statement(stmt, Rc::clone(&env)),
        Statement::ForIn(stmt) => eval_for_in_statement(stmt, enter_env(Rc::clone(&env))),
    }
}

fn eval_expression(exp: &Expression, env: Env) -> Result<Object, Error> {
    match exp {
        Expression::Bool(v) => Ok(Object::Boolean(v.value)),
        Expression::Float(v) => Ok(Object::Float(v.value)),
        Expression::Integer(v) => Ok(Object::Integer(v.value)),
        Expression::String(v) => Ok(Object::String(v.value.to_owned())),
        Expression::Function(v) => eval_function_literal(v, Rc::clone(&env)),
        Expression::Call(v) => eval_call_expression(v, Rc::clone(&env)),
        Expression::Identifier(v) => eval_identifier(v, Rc::clone(&env)),
        Expression::If(v) => eval_if_expression(v, Rc::clone(&env)),
        Expression::Infix(v) => eval_infix_expression(v, Rc::clone(&env)),
        Expression::Prefix(v) => eval_prefix_expression(v, Rc::clone(&env)),
        Expression::Slice(v) => eval_slice_literal(v, Rc::clone(&env)),
        Expression::Map(v) => eval_map_literal(v, Rc::clone(&env)),
        Expression::Index(v) => eval_index_expression(v, Rc::clone(&env)),
    }
}

fn eval_let_statement(stmt: &LetStatement, env: Env) -> Result<Object, Error> {
    let value = eval_expression(&stmt.value, Rc::clone(&env))?;
    env.borrow_mut()
        .store
        .insert(stmt.name.name.to_owned(), value);
    Ok(Object::Empty)
}

fn eval_return_statement(stmt: &ReturnStatement, env: Env) -> Result<Object, Error> {
    Ok(Object::ReturnValue(Box::new(eval_expression(
        &stmt.value,
        env,
    )?)))
}

fn eval_expression_statement(stmt: &ExpressionStatement, env: Env) -> Result<Object, Error> {
    eval_expression(&stmt.expression, env)
}

fn eval_block_statement(block: &BlockStatement, env: Env) -> Result<Object, Error> {
    eval_statements(&block.statements, false, env)
}

fn eval_index_assign_statement(exp: &IndexAssignStatement, env: Env) -> Result<Object, Error> {
    let index = eval_expression(&exp.index, Rc::clone(&env))?;
    match &exp.left {
        Expression::Identifier(name) => {
            if let Some(left) = from_env(&env, &name.name) {
                match left {
                    Object::Map(map) => {
                        map.borrow_mut().insert(
                            index.try_into()?,
                            eval_expression(&exp.right, Rc::clone(&env))?,
                        );
                    }
                    Object::Slice(vec) => match index {
                        Object::Integer(i) => {
                            if vec.borrow().len() <= i as usize {
                                return Err(Error {
                                    msg: format!(
                                        "index out of bound At {}:{}",
                                        exp.position.0, exp.position.1
                                    )
                                        .to_string(),
                                });
                            }
                            vec.borrow_mut()[i as usize] =
                                eval_expression(&exp.right, Rc::clone(&env))?;
                        }
                        _ => return Err(operation_type_error(&exp.position)),
                    },
                    _ => return Err(operation_type_error(&exp.position)),
                }
            } else {
                return Err(Error {
                    msg: format!(
                        "not found variable name '{}' At {}:{}",
                        name.name, name.position.0, name.position.1
                    )
                        .to_owned(),
                });
            }
        }
        _ => unreachable!(),
    }
    Ok(Object::Empty)
}

fn eval_while_statement(exp: &WhileStatement, env: Env) -> Result<Object, Error> {
    loop {
        if let Object::Boolean(ok) = eval_expression(&exp.condition, Rc::clone(&env))? {
            if ok {
                eval_block_statement(&exp.consequence, Rc::clone(&env))?;
                continue;
            } else {
                return Ok(Object::Empty);
            }
        } else {
            return Err(operation_type_error(&exp.position));
        }
    }
}

fn eval_for_in_statement(exp: &ForInStatement, env: Env) -> Result<Object, Error> {
    let collection = eval_expression(&exp.collection, Rc::clone(&env))?;
    match collection {
        Object::Map(map) => {
            for (k, v) in map.borrow().iter() {
                {
                    let store = &mut env.borrow_mut().store;
                    store.insert(exp.key.name.to_owned(), k.to_owned().into());
                    if let Some(value) = &exp.value {
                        store.insert(value.name.to_owned(), v.to_owned());
                    }
                }
                eval_block_statement(&exp.body, enter_env(Rc::clone(&env)))?;
            }
        }
        Object::Slice(vec) => {
            for (i, element) in vec.borrow().iter().enumerate() {
                {
                    let store = &mut env.borrow_mut().store;
                    store.insert(exp.key.name.to_owned(), Object::Integer(i as i64));
                    if let Some(value) = &exp.value {
                        store.insert(value.name.to_owned(), element.to_owned());
                    }
                }
                eval_block_statement(&exp.body, enter_env(Rc::clone(&env)))?;
            }
        }
        _ => return Err(operation_type_error(&exp.position)),
    };
    Ok(Object::Empty)
}

fn eval_statements(stmts: &[Statement], is_root: bool, env: Env) -> Result<Object, Error> {
    let mut result = Object::Empty;
    let mut result_is_empty = false;
    if !stmts.is_empty() {
        if let Statement::Expression(ref expression_statement) = stmts[stmts.len() - 1] {
            result_is_empty = expression_statement.end_of_semicolon
        }
    }
    for stmt in stmts.iter() {
        result = eval_statement(stmt, Rc::clone(&env))?;
        if let Object::ReturnValue(v) = result {
            return if is_root {
                Ok(*v)
            } else {
                Ok(Object::ReturnValue(v))
            };
        }
    }
    if result_is_empty {
        Ok(Object::Empty)
    } else {
        Ok(result)
    }
}

fn eval_prefix_expression(exp: &PrefixExpression, env: Env) -> Result<Object, Error> {
    match exp.token {
        Token::Bang => eval_bang_operator_expression(&exp.right, Rc::clone(&env)),
        Token::Minus => eval_minus_operator_expression(&exp.right, Rc::clone(&env)),
        _ => unreachable!(),
    }
}

fn eval_infix_expression(exp: &InfixExpression, env: Env) -> Result<Object, Error> {
    if exp.token.is_assign() {
        return eval_assign_expression(
            exp,
            eval_expression(&exp.right, Rc::clone(&env))?,
            Rc::clone(&env),
        );
    }
    let left = eval_expression(&exp.left, Rc::clone(&env))?;
    let right = eval_expression(&exp.right, Rc::clone(&env))?;
    match exp.token {
        Token::EQ | Token::NotEq | Token::LT | Token::Lte | Token::GT | Token::Gte => {
            eval_compare_operator_expression(exp, left, right)
        }
        Token::Plus | Token::Minus | Token::Slash | Token::Asterisk => {
            eval_compute_operator_expression(exp, left, right)
        }
        _ => unreachable!(),
    }
}

fn eval_identifier(exp: &Identifier, env: Env) -> Result<Object, Error> {
    let value = from_env(&env, &exp.name);
    if let Some(value) = value {
        Ok(value)
    } else if BUILD_IN_FUNCTIONS.contains_key(exp.name.as_str()) {
        Ok(Object::BuildIn(exp.name.to_owned()))
    } else {
        Err(Error {
            msg: format!(
                "not found variable name '{}' At {}:{}",
                exp.name, exp.position.0, exp.position.1
            )
                .to_owned(),
        })
    }
}

fn eval_if_expression(exp: &IfExpression, env: Env) -> Result<Object, Error> {
    if let Object::Boolean(ok) = eval_expression(&exp.condition, Rc::clone(&env))? {
        if ok {
            eval_block_statement(&exp.consequence, Rc::clone(&env))
        } else if let Some(stmt) = &exp.alternative {
            eval_block_statement(stmt, Rc::clone(&env))
        } else if let Some(exp) = &exp.optional {
            eval_expression(exp, Rc::clone(&env))
        } else {
            Ok(Object::Empty)
        }
    } else {
        Err(operation_type_error(&exp.position))
    }
}

fn eval_function_literal(exp: &FunctionLiteral, env: Env) -> Result<Object, Error> {
    let func = Object::Function(Rc::new(exp.to_owned()));
    if exp.name.is_some() {
        env.borrow_mut().store.insert(
            exp.name.to_owned().unwrap().name,
            Object::Function(Rc::new(exp.to_owned())),
        );
    }
    Ok(func)
}

fn eval_map_literal(exp: &MapLiteral, env: Env) -> Result<Object, Error> {
    let mut map = HashMap::new();
    for (k, v) in exp.kv_pair.iter() {
        let key = eval_expression(k, Rc::clone(&env))?;
        let value = eval_expression(v, Rc::clone(&env))?;
        map.insert(key.try_into()?, value);
    }
    Ok(Object::Map(Rc::new(RefCell::new(map))))
}

fn eval_slice_literal(exp: &SliceLiteral, env: Env) -> Result<Object, Error> {
    let mut vec = Vec::new();
    for exp in exp.elements.iter() {
        vec.push(eval_expression(exp, Rc::clone(&env))?)
    }
    Ok(Object::Slice(Rc::new(RefCell::new(vec))))
}

fn eval_call_expression(exp: &CallExpression, env: Env) -> Result<Object, Error> {
    let func = eval_expression(&exp.function, Rc::clone(&env))?;
    let mut params = vec![];
    for x in exp.arguments.iter() {
        let result = eval_expression(x, Rc::clone(&env))?;
        params.push(result)
    }
    match func {
        Object::Function(body) => {
            if body.parameters.len() != params.len() {
                return Err(Error {
                    msg: format!(
                        "the number of parameter not match At{}:{}",
                        exp.position.0, exp.position.1
                    )
                        .to_owned(),
                });
            }
            let env = enter_env(Rc::clone(&env));
            for (i, param) in params.into_iter().enumerate() {
                env.borrow_mut()
                    .store
                    .insert(body.parameters[i].name.to_owned(), param);
            }
            Ok(eval_statements(&body.body.statements, true, env)?)
        }
        Object::BuildIn(name) => match BUILD_IN_FUNCTIONS.get(name.as_str()).unwrap()(&params) {
            Ok(result) => Ok(result),
            Err(err) => Err(Error {
                msg: format!(
                    "call function {} error At{}:{}, cause:{}",
                    name,
                    exp.position.0,
                    exp.position.1.to_owned(),
                    err
                ),
            }),
        },
        _ => Err(operation_type_error(&exp.position)),
    }
}

fn eval_index_expression(exp: &IndexExpression, env: Env) -> Result<Object, Error> {
    let index = match eval_expression(&exp.index, Rc::clone(&env))? {
        Object::Integer(v) => Object::Integer(v),
        Object::Boolean(v) => Object::Boolean(v),
        Object::String(v) => Object::String(v),
        _ => return Err(operation_type_error(&exp.position)),
    };
    let left = match eval_expression(&exp.left, Rc::clone(&env))? {
        Object::String(v) => Object::String(v),
        Object::Map(v) => Object::Map(v),
        Object::Slice(v) => Object::Slice(v),
        _ => return Err(operation_type_error(&exp.position)),
    };
    Ok(match (left, index) {
        (Object::String(ref result), Object::Integer(i)) => Object::String(
            result
                .chars()
                .nth(i as usize)
                .ok_or(Error {
                    msg: format!(
                        "index out of bound At {}:{}",
                        exp.position.0, exp.position.1
                    )
                        .to_string(),
                })?
                .to_string(),
        ),
        (Object::Slice(ref result), Object::Integer(i)) => {
            if result.borrow().len() < i as usize {
                return Err(Error {
                    msg: format!(
                        "index out of bound At {}:{}",
                        exp.position.0, exp.position.1
                    )
                        .to_string(),
                });
            }
            result.borrow_mut()[i as usize].to_owned()
        }
        (Object::Map(result), key) => result
            .borrow()
            .get(&key.try_into()?)
            .cloned()
            .unwrap_or(Object::Empty),
        _ => return Err(operation_type_error(&exp.position)),
    })
}

fn eval_bang_operator_expression(exp: &Expression, env: Env) -> Result<Object, Error> {
    match eval_expression(exp, env)? {
        Object::Boolean(v) => Ok(Object::Boolean(!v)),
        _ => Err(operation_type_error(&exp.position())),
    }
}

fn eval_minus_operator_expression(exp: &Expression, env: Env) -> Result<Object, Error> {
    match eval_expression(exp, env)? {
        Object::Integer(v) => Ok(Object::Integer(-v)),
        Object::Float(v) => Ok(Object::Float(-v)),
        _ => Err(operation_type_error(&exp.position())),
    }
}

fn eval_compute_operator_expression(
    exp: &InfixExpression,
    left: Object,
    right: Object,
) -> Result<Object, Error> {
    let operator = exp.token.to_owned();
    let value = (left, right);
    Ok(match value {
        (Object::Integer(l), Object::Integer(r)) => match operator {
            Token::Plus => Object::Integer(l + r),
            Token::Minus => Object::Integer(l - r),
            Token::Asterisk => Object::Integer(l * r),
            Token::Slash => Object::Integer(l / r),
            _ => return Err(operation_type_error(&exp.position)),
        },
        (Object::Float(l), Object::Float(r)) => match operator {
            Token::Plus => Object::Float(l + r),
            Token::Minus => Object::Float(l - r),
            Token::Asterisk => Object::Float(l * r),
            Token::Slash => Object::Float(l / r),
            _ => return Err(operation_type_error(&exp.position)),
        },
        (Object::String(l), Object::String(r)) => match operator {
            Token::Plus => Object::String(l + r.as_str()),
            _ => return Err(operation_type_error(&exp.position)),
        },
        (Object::String(l), Object::Integer(r)) => match operator {
            Token::Plus => Object::String(l + r.to_string().as_str()),
            _ => return Err(operation_type_error(&exp.position)),
        },
        (Object::Integer(l), Object::String(r)) => match operator {
            Token::Plus => Object::String(l.to_string() + r.as_str()),
            _ => return Err(operation_type_error(&exp.position)),
        },
        (Object::String(l), Object::Float(r)) => match operator {
            Token::Plus => Object::String(l + r.to_string().as_str()),
            _ => return Err(operation_type_error(&exp.position)),
        },
        (Object::Float(l), Object::String(r)) => match operator {
            Token::Plus => Object::String(l.to_string() + r.as_str()),
            _ => return Err(operation_type_error(&exp.position)),
        },
        (Object::Float(l), Object::Integer(r)) => match operator {
            Token::Plus => Object::Float(l + r as f64),
            _ => return Err(operation_type_error(&exp.position)),
        },
        (Object::Integer(l), Object::Float(r)) => match operator {
            Token::Plus => Object::Float(l as f64 + r),
            _ => return Err(operation_type_error(&exp.position)),
        },
        _ => return Err(operation_type_error(&exp.position)),
    })
}

fn eval_assign_expression(exp: &InfixExpression, right: Object, env: Env) -> Result<Object, Error> {
    let left = match exp.left {
        Expression::Identifier(ref v) => v,
        _ => return Err(operation_type_error(&exp.position)),
    };
    let name = left.name.to_owned();
    if let Some(left) = from_env(&env, &name) {
        let value = (&left, &right);
        Ok(match value {
            (Object::Integer(_), Object::Integer(v)) => {
                to_env(&env, name, Object::Integer(*v));
                right
            }
            (Object::Float(_), Object::Float(v)) => {
                to_env(&env, name, Object::Float(*v));
                right
            }
            (Object::Boolean(_), Object::Boolean(v)) => {
                to_env(&env, name, Object::Boolean(*v));
                right
            }
            (Object::String(_), Object::String(v)) => {
                to_env(&env, name, Object::String(v.to_owned()));
                right
            }
            (Object::Map(_), Object::Map(v)) => {
                to_env(&env, name, Object::Map(Rc::clone(v)));
                right
            }
            (Object::Slice(_), Object::Slice(v)) => {
                to_env(&env, name, Object::Slice(Rc::clone(v)));
                right
            }
            (Object::Function(_), Object::Function(v)) => {
                to_env(&env, name, Object::Function(Rc::clone(v)));
                right
            }
            _ => return Err(operation_type_error(&exp.position)),
        })
    } else {
        Err(operation_type_error(&exp.position))
    }
}

fn operation_type_error(position: &Position) -> Error {
    Error {
        msg: format!("operation type invalid At {}:{}", position.0, position.1).to_owned(),
    }
}

macro_rules! eval_compare {
    ($operator:expr, $l:expr, $r:expr) => {
        match $operator {
            Token::EQ => Object::Boolean($l == $r),
            Token::NotEq => Object::Boolean($l != $r),
            Token::LT => Object::Boolean($l < $r),
            Token::Lte => Object::Boolean($l <= $r),
            Token::GT => Object::Boolean($l > $r),
            Token::Gte => Object::Boolean($l >= $r),
            _ => unreachable!(),
        }
    };
}

fn eval_compare_operator_expression(
    exp: &InfixExpression,
    left: Object,
    right: Object,
) -> Result<Object, Error> {
    let operator = exp.token.to_owned();
    let value = (left, right);
    Ok(match value {
        (Object::Integer(l), Object::Integer(r)) => eval_compare!(operator, l, r),
        (Object::Float(l), Object::Float(r)) => eval_compare!(operator, l, r),
        (Object::Boolean(l), Object::Boolean(r)) => eval_compare!(operator, l, r),
        (Object::String(l), Object::String(r)) => eval_compare!(operator, l, r),
        _ => return Err(operation_type_error(&exp.position)),
    })
}
