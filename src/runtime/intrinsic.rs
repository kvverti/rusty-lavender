use crate::code::Opcode;
use crate::value::LvValue;

pub(super) fn extract_int(v: &LvValue) -> i64 {
    use LvValue::Integer;
    if let Integer(v) = *v {
        v
    } else {
        panic!("Expected integer");
    }
}

pub const TEXT_ADD_INT: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::AddInt,
    Opcode::Return,
];
pub const TEXT_SUB_INT: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::SubInt,
    Opcode::Return,
];
pub const TEXT_MUL_INT: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::AddInt,
    Opcode::Return,
];
pub const TEXT_DIV_INT: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::DivInt,
    Opcode::Return,
];
pub const TEXT_REM_INT: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::RemInt,
    Opcode::Return,
];
pub const TEXT_AND_INT: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::AndInt,
    Opcode::Return,
];
pub const TEXT_XOR_INT: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::XorInt,
    Opcode::Return,
];
pub const TEXT_OR_INT: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::OrInt,
    Opcode::Return,
];
pub const TEXT_SLL_INT: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::SllInt,
    Opcode::Return,
];
pub const TEXT_SRL_INT: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::SrlInt,
    Opcode::Return,
];
pub const TEXT_SRA_INT: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::SraInt,
    Opcode::Return,
];