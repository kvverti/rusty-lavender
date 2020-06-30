/// Reference to a symbol.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct SymbolicReference {
    /// The index in the symbol table the symbol is stored at.
    pub(super) idx: usize,
}

impl SymbolicReference {
    /// Creates a data reference from a raw index.
    #[inline]
    pub(super) fn from_raw(idx: usize) -> Self {
        Self { idx }
    }
}

/// Reference to a text subset.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct TextLabel {
    /// The index in the symbol table the symbol representing the text block is stored at.
    pub(super) idx: usize,
}

impl TextLabel {
    /// Creates a text reference from a raw index.
    #[inline]
    pub(super) fn from_raw(idx: usize) -> Self {
        Self { idx }
    }
}
