use super::program::BuiltinOp;

pub(crate) fn builtin_names() -> Vec<&'static str> {
    BuiltinOp::all().iter().map(|op| op.name()).collect()
}

pub(crate) fn global_builtin_names() -> Vec<&'static str> {
    BuiltinOp::all()
        .iter()
        .copied()
        .filter(|op| op.is_global())
        .map(|op| op.name())
        .collect()
}
