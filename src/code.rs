use crate::runtime::SymbolicReference;
use crate::value::LvValue;

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
    /// Function application.
    Apply,
    /// Fully evaluate the top expression. Used sparingly.
    Eval,
    /// A return from a function to the caller.
    Return,
    /// An "escape hatch" into the native environment. Used to implement intrinsic operations
    /// such as integer addition. This opcode shall never appear in serialized forms.
    Intrinsic(u8, fn(&mut [LvValue]) -> LvValue),
    /// A debug instruction to print out the current stack top value.
    DebugTop,
}
