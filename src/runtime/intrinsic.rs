use crate::code::Opcode;
use crate::value::LvValue;

fn extract_int(v: &LvValue) -> i64 {
    use LvValue::Integer;
    if let Integer(v) = *v {
        v
    } else {
        panic!("Expected integer");
    }
}

fn addi(args: &mut [LvValue]) -> LvValue {
    let a = extract_int(&args[0]);
    let b = extract_int(&args[1]);
    LvValue::from(a.wrapping_add(b))
}

pub const TEXT_ADD_INT: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, addi),
    Opcode::Return,
];

fn subi(args: &mut [LvValue]) -> LvValue {
    let a = extract_int(&args[0]);
    let b = extract_int(&args[1]);
    LvValue::from(a.wrapping_sub(b))
}

pub const TEXT_SUB_INT: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, subi),
    Opcode::Return,
];

fn muli(args: &mut [LvValue]) -> LvValue {
    let a = extract_int(&args[0]);
    let b = extract_int(&args[1]);
    LvValue::from(a.wrapping_mul(b))
}

pub const TEXT_MUL_INT: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, muli),
    Opcode::Return,
];

fn divi(args: &mut [LvValue]) -> LvValue {
    let a = extract_int(&args[0]);
    let b = extract_int(&args[1]);
    LvValue::from(a.wrapping_div(b))
}

pub const TEXT_DIV_INT: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, divi),
    Opcode::Return,
];

fn remi(args: &mut [LvValue]) -> LvValue {
    let a = extract_int(&args[0]);
    let b = extract_int(&args[1]);
    LvValue::from(a.wrapping_rem(b))
}

pub const TEXT_REM_INT: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, remi),
    Opcode::Return,
];

fn andi(args: &mut [LvValue]) -> LvValue {
    let a = extract_int(&args[0]);
    let b = extract_int(&args[1]);
    LvValue::from(a & b)
}

pub const TEXT_AND_INT: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, andi),
    Opcode::Return,
];

fn xori(args: &mut [LvValue]) -> LvValue {
    let a = extract_int(&args[0]);
    let b = extract_int(&args[1]);
    LvValue::from(a ^ b)
}

pub const TEXT_XOR_INT: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, xori),
    Opcode::Return,
];

fn ori(args: &mut [LvValue]) -> LvValue {
    let a = extract_int(&args[0]);
    let b = extract_int(&args[1]);
    LvValue::from(a | b)
}

pub const TEXT_OR_INT: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, ori),
    Opcode::Return,
];

fn sll(args: &mut [LvValue]) -> LvValue {
    let a = extract_int(&args[0]);
    let b = extract_int(&args[1]);
    LvValue::from(a << (b & 63))
}

pub const TEXT_SLL: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, sll),
    Opcode::Return,
];

fn srl(args: &mut [LvValue]) -> LvValue {
    let a = extract_int(&args[0]);
    let b = extract_int(&args[1]);
    LvValue::from(((a as u64) >> (b & 63) as u64) as i64)
}

pub const TEXT_SRL: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, srl),
    Opcode::Return,
];

fn sra(args: &mut [LvValue]) -> LvValue {
    let a = extract_int(&args[0]);
    let b = extract_int(&args[1]);
    LvValue::from(a >> (b & 63))
}

pub const TEXT_SRA: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, sra),
    Opcode::Return,
];
