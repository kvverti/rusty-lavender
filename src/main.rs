use crate::code::Opcode;
use crate::runtime::RuntimeContext;
use crate::value::func::LvFunc;
use crate::value::LvValue;

mod code;
mod runtime;
mod value;

fn main() {
    println!("Hello, world!");
    let mut runtime = RuntimeContext::new();
    // an infinite loop
    // def f a = a a
    // def main = f f
    let f = runtime.symbols.push_text(&[
        Opcode::CopyArg(0),
        Opcode::MoveArg(0),
        Opcode::Apply,
        Opcode::Return,
    ]);
    let f = runtime.symbols.push_data(LvFunc::new(f, 1));
    let main = runtime.symbols.push_text(&[
        Opcode::Value(f),
        Opcode::Value(f),
        Opcode::Apply,
        Opcode::Return,
    ]);
    let main = LvFunc::new(main, 0);
    // evaluating main returns a thunk, because lazy eval
    let mut r = runtime.exec(main);
    // after evaluating main, draw the rest of the owl
    while let LvValue::Function(f) = r {
        if f.arity == 0 {
            r = runtime.exec(f);
        } else {
            r = LvValue::from(f);
        }
    }
    println!("{:?}", r);
}
