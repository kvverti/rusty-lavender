use std::ops::{AddAssign, Sub, Add, SubAssign};

use crate::code::Opcode;
use crate::runtime::SymbolTable;
use crate::value::LvValue;

/// Reference to a data symbol.
#[derive(Copy, Clone)]
pub struct DataReference {
    idx: usize,
}

impl DataReference {
    /// Creates a data reference from a raw index.
    #[inline]
    pub unsafe fn from_raw(idx: usize) -> Self {
        Self { idx }
    }

    /// Returns the value this reference refers to.
    #[inline]
    pub fn as_ref<'b>(&self, tbl: &'b SymbolTable) -> &'b LvValue {
        &tbl.data[self.idx]
    }
}

/// Reference to a text subset.
#[derive(Copy, Clone, Debug)]
pub struct TextReference {
    idx: usize,
}

impl TextReference {
    /// Creates a text reference from a raw index.
    #[inline]
    pub unsafe fn from_raw(idx: usize) -> Self {
        Self { idx }
    }

    /// Returns the sequence of opcodes this reference refers to.
    #[inline]
    pub fn as_ref<'b>(&self, tbl: &'b SymbolTable) -> &'b Opcode {
        &tbl.text[self.idx]
    }
}

impl Default for TextReference {
    #[inline]
    fn default() -> Self {
        Self { idx: 0 }
    }
}

impl Add<i32> for TextReference {
    type Output = Self;

    #[inline]
    fn add(mut self, rhs: i32) -> Self::Output {
        self.add_assign(rhs);
        self
    }
}

impl AddAssign<i32> for TextReference {
    #[inline]
    fn add_assign(&mut self, rhs: i32) {
        if rhs < 0 {
            self.idx -= -rhs as usize;
        } else {
            self.idx += rhs as usize;
        }
    }
}

impl Sub<i32> for TextReference {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: i32) -> Self::Output {
        self + -rhs
    }
}

impl SubAssign<i32> for TextReference {
    fn sub_assign(&mut self, rhs: i32) {
        *self += -rhs;
    }
}
