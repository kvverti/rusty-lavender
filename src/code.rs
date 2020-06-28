use crate::runtime::SymbolicReference;

/// An instruction that represents a fundamental operation in Lavender.
#[allow(dead_code)]
#[derive(Copy, Clone)]
pub enum Opcode {
    /// Load the unit value.
    Unit,
    /// Load a literal value.
    Value(SymbolicReference),
    /// A move of the given argument (or function local) to the stack.
    MoveArg(u8),
    /// A copy of the given argument (or function local) to the stack.
    CopyArg(u8),
    /// Integer addition.
    AddInt,
    /// Function application.
    Apply,
    /// A return from a function to the caller.
    Return,
    /// A debug instruction to print out the current stack top value.
    DebugTop,
}
