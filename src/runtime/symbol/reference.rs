use std::ops::{AddAssign, Sub, Add, SubAssign};

/// Reference to a data symbol.
#[derive(Copy, Clone)]
pub struct DataReference {
    pub(super) idx: usize,
}

impl DataReference {
    /// Creates a data reference from a raw index.
    #[inline]
    pub unsafe fn from_raw(idx: usize) -> Self {
        Self { idx }
    }
}

/// Reference to a text subset.
#[derive(Copy, Clone, Debug)]
pub struct TextReference {
    pub(super) idx: usize,
}

impl TextReference {
    /// Creates a text reference from a raw index.
    #[inline]
    pub unsafe fn from_raw(idx: usize) -> Self {
        Self { idx }
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
