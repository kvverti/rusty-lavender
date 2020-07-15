use crate::value::LvValue;

/// A vector (or tuple) whose elements are stored in reverse order.
#[derive(Clone, Debug, PartialEq)]
pub struct LvVect {
    /// The values stored in the vector. These values may be lazy, especially the tail of the
    /// vector (`values[0]`), which may itself be a vector that should logically be appended
    /// to this value.
    pub values: Vec<LvValue>,
    /// Whether the tail of this vector should be appended or not. Vects representing tuples
    /// will never have this value `true`, but vects representing vectors may have a lazy
    /// tail that should be prepended.
    pub append_tail: bool,
}
