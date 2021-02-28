use crate::value::LvValue;

pub const LOCAL_SIZE: usize = 8;

/// A stack frame.
pub struct StackFrame {
    /// Small local value stack.
    pub locals: [LvValue; LOCAL_SIZE],
    /// The return address from the previous stack frame.
    pub ret: usize,
    /// The base of the frame.
    pub fp: usize,
}
