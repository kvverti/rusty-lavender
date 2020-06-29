use crate::runtime::SymbolicReference;

/// An instruction that represents a fundamental operation in Lavender.
#[allow(dead_code)]
#[derive(Copy, Clone)]
pub enum Opcode {
    /// Load the unit value.
    UnitValue,
    /// Load a literal value.
    Value(SymbolicReference),
    /// Load an integer value in a single-byte range. Useful not to overload the symbol table.
    IntValue(i8),
    /// A move of the given argument (or function local) to the stack.
    MoveArg(u8),
    /// A copy of the given argument (or function local) to the stack.
    CopyArg(u8),
    /// Integer addition.
    AddInt,
    /// Integer subtraction.
    SubInt,
    /// Integer multiplication.
    MulInt,
    /// Integer division.
    DivInt,
    /// Integer remainder.
    RemInt,
    /// Bitwise and.
    AndInt,
    /// Bitwise xor.
    XorInt,
    /// Bitwise or.
    OrInt,
    /// Left shift.
    SllInt,
    /// Logical right shift.
    SrlInt,
    /// Arithmetic right shift.
    SraInt,
    /// Function application.
    Apply,
    /// Fully evaluate the top expression. Used sparingly.
    Eval,
    /// A return from a function to the caller.
    Return,
    /// A debug instruction to print out the current stack top value.
    DebugTop,
}
