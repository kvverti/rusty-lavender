use crate::value::func::LvFunc;

/// Contains Lavender's function values.
pub mod func;

/// A Lavender value.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LvValue {
    /// The Unit, or singleton, value.
    Unit,
    /// A 64-bit integer value.
    Integer(i64),
    /// A string value.
    String(String),
    /// A function, i.e. callable value.
    Function(LvFunc),
}

impl Default for LvValue {
    fn default() -> Self {
        Self::Unit
    }
}

impl From<()> for LvValue {
    fn from(_: ()) -> Self {
        LvValue::Unit
    }
}

impl From<i64> for LvValue {
    fn from(v: i64) -> Self {
        LvValue::Integer(v)
    }
}

impl From<String> for LvValue {
    fn from(v: String) -> Self {
        LvValue::String(v)
    }
}

impl From<LvFunc> for LvValue {
    fn from(v: LvFunc) -> Self {
        LvValue::Function(v)
    }
}
