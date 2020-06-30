use crate::code::Opcode;
use crate::value::LvValue;

// integer operations

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

pub const TEXT_ADDI: [Opcode; 6] = [
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

pub const TEXT_SUBI: [Opcode; 6] = [
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

pub const TEXT_MULI: [Opcode; 6] = [
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

pub const TEXT_DIVI: [Opcode; 6] = [
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

pub const TEXT_REMI: [Opcode; 6] = [
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

pub const TEXT_ANDI: [Opcode; 6] = [
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

pub const TEXT_XORI: [Opcode; 6] = [
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

pub const TEXT_ORI: [Opcode; 6] = [
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

fn eqi(args: &mut [LvValue]) -> LvValue {
    let a = extract_int(&args[0]);
    let b = extract_int(&args[1]);
    LvValue::from(a == b)
}

pub const TEXT_EQI: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, eqi),
    Opcode::Return,
];

fn lti(args: &mut [LvValue]) -> LvValue {
    let a = extract_int(&args[0]);
    let b = extract_int(&args[1]);
    LvValue::from(a < b)
}

pub const TEXT_LTI: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, lti),
    Opcode::Return,
];

// float operations

fn extract_float(v: &LvValue) -> f64 {
    use LvValue::Float;
    if let Float(v) = *v {
        v
    } else {
        panic!("Expected float");
    }
}

fn addf(args: &mut [LvValue]) -> LvValue {
    let a = extract_float(&args[0]);
    let b = extract_float(&args[1]);
    LvValue::from(a + b)
}

pub const TEXT_ADDF: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, addf),
    Opcode::Return,
];

fn subf(args: &mut [LvValue]) -> LvValue {
    let a = extract_float(&args[0]);
    let b = extract_float(&args[1]);
    LvValue::from(a - b)
}

pub const TEXT_SUBF: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, subf),
    Opcode::Return,
];

fn mulf(args: &mut [LvValue]) -> LvValue {
    let a = extract_float(&args[0]);
    let b = extract_float(&args[1]);
    LvValue::from(a * b)
}

pub const TEXT_MULF: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, mulf),
    Opcode::Return,
];

fn divf(args: &mut [LvValue]) -> LvValue {
    let a = extract_float(&args[0]);
    let b = extract_float(&args[1]);
    LvValue::from(a / b)
}

pub const TEXT_DIVF: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, divf),
    Opcode::Return,
];

fn remf(args: &mut [LvValue]) -> LvValue {
    let a = extract_float(&args[0]);
    let b = extract_float(&args[1]);
    LvValue::from(a % b)
}

pub const TEXT_REMF: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, remf),
    Opcode::Return,
];

fn eqf(args: &mut [LvValue]) -> LvValue {
    let a = extract_float(&args[0]);
    let b = extract_float(&args[1]);
    LvValue::from(a == b)
}

pub const TEXT_EQF: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, eqf),
    Opcode::Return,
];

fn ltf(args: &mut [LvValue]) -> LvValue {
    let a = extract_float(&args[0]);
    let b = extract_float(&args[1]);
    LvValue::from(a < b)
}

pub const TEXT_LTF: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, ltf),
    Opcode::Return,
];

fn gef(args: &mut [LvValue]) -> LvValue {
    let a = extract_float(&args[0]);
    let b = extract_float(&args[1]);
    LvValue::from(a >= b)
}

pub const TEXT_GEF: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, gef),
    Opcode::Return,
];

// boolean operations

fn extract_bool(v: &LvValue) -> bool {
    use LvValue::Bool;
    if let Bool(v) = *v {
        v
    } else {
        panic!("Expected bool");
    }
}

fn andz(args: &mut [LvValue]) -> LvValue {
    let a = extract_bool(&args[0]);
    let b = extract_bool(&args[1]);
    LvValue::from(a && b)
}

pub const TEXT_ANDZ: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, andz),
    Opcode::Return,
];

fn xorz(args: &mut [LvValue]) -> LvValue {
    let a = extract_bool(&args[0]);
    let b = extract_bool(&args[1]);
    LvValue::Bool(a ^ b)
}

pub const TEXT_XORZ: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, xorz),
    Opcode::Return,
];

fn orz(args: &mut [LvValue]) -> LvValue {
    let a = extract_bool(&args[0]);
    let b = extract_bool(&args[1]);
    LvValue::from(a || b)
}

pub const TEXT_ORZ: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, orz),
    Opcode::Return,
];

// string operations

fn extract_str(v: &LvValue) -> &str {
    use LvValue::String;
    if let String(v) = v {
        v
    } else {
        panic!("Expected string");
    }
}

fn lens(args: &mut [LvValue]) -> LvValue {
    let a = extract_str(&args[0]);
    LvValue::from(a.len() as i64)
}

pub const TEXT_LENS: [Opcode; 4] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::Intrinsic(1, lens),
    Opcode::Return,
];

fn cats(args: &mut [LvValue]) -> LvValue {
    let a = extract_str(&args[0]);
    let b = extract_str(&args[1]);
    LvValue::from(a.to_owned() + b)
}

pub const TEXT_CATS: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, cats),
    Opcode::Return,
];

fn eqs(args: &mut [LvValue]) -> LvValue {
    let a = extract_str(&args[0]);
    let b = extract_str(&args[1]);
    LvValue::from(a == b)
}

pub const TEXT_EQS: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, eqs),
    Opcode::Return,
];

fn lts(args: &mut [LvValue]) -> LvValue {
    let a = extract_str(&args[0]);
    let b = extract_str(&args[1]);
    LvValue::from(a < b)
}

pub const TEXT_LTS: [Opcode; 6] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::MoveArg(1),
    Opcode::Eval,
    Opcode::Intrinsic(2, lts),
    Opcode::Return,
];

// conversions

fn i2f(args: &mut [LvValue]) -> LvValue {
    let a = extract_int(&args[0]);
    LvValue::from(a as f64)
}

pub const TEXT_I2F: [Opcode; 4] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::Intrinsic(1, i2f),
    Opcode::Return,
];

fn f2i(args: &mut [LvValue]) -> LvValue {
    let a = extract_float(&args[0]);
    LvValue::from(a as i64)
}

pub const TEXT_F2I: [Opcode; 4] = [
    Opcode::MoveArg(0),
    Opcode::Eval,
    Opcode::Intrinsic(1, f2i),
    Opcode::Return,
];
