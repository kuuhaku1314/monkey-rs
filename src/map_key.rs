#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum AbleToMapKey {
    Integer(i64),
    Boolean(bool),
    String(String),
}

impl From<String> for AbleToMapKey {
    fn from(value: String) -> Self {
        AbleToMapKey::String(value)
    }
}

impl From<&str> for AbleToMapKey {
    fn from(value: &str) -> Self {
        AbleToMapKey::String(value.to_string())
    }
}
