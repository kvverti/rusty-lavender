use crate::runtime::SymbolicReference;
use crate::value::LvValue;

/// An instruction that represents a fundamental operation in Lavender.
#[allow(dead_code)]
#[derive(Copy, Clone)]
pub enum Opcode {
    // immediate opcodes
    /// Load the unit value.
    UnitValue,
    /// Load the value `True`.
    TrueValue,
    /// Load the value `False`.
    FalseValue,
    /// Load a literal value.
    Value(SymbolicReference),
    /// Load an integer value in a single-byte range. Useful not to overload the symbol table.
    IntValue(i8),

    // value marshalling opcodes
    /// A move of the given argument (or function local) to the stack.
    MoveArg(u8),
    /// A copy of the given argument (or function local) to the stack.
    CopyArg(u8),
    /// A move from the stack to the given argument (or function local).
    MoveArgTo(u8),

    // function application opcodes
    /// Function application.
    Apply,
    /// Fully evaluate the top expression. Used sparingly.
    Eval,
    /// A return from a function to the caller.
    Return,

    // value destructuring opcodes
    /// Destructures a vect or tuple.
    MatchTuple,

    // jump opcodes
    /// Relative jump to the given offset.
    Jump(i32),
    /// Branch if false.
    BranchFalse(i32),

    // intrinsic opcodes
    /// An "escape hatch" into the native environment. Used to implement intrinsic operations
    /// such as integer addition. This opcode shall never appear in serialized forms.
    Intrinsic(u8, fn(&mut [LvValue]) -> LvValue),
    /// An "escape hatch" into the native environment. Used to implement intrinsic operations
    /// that should not pop their arguments from the stack. This can prevent needless copying.
    // note: determine whether this opcode is necessary for more things than vector length
    IntrinsicNoModify(u8, fn(&[LvValue]) -> LvValue),
    /// A debug instruction to print out the current stack top value.
    DebugTop,
}
