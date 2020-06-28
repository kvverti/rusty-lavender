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
    // def f a = f a
    // def main = f Unit
    let f = runtime.symbols.create_symbol("f");
    let f_text = runtime.symbols.create_label("f");
    let main_text = runtime.symbols.create_label("main");
    runtime.symbols.define_label(f_text,&[
        Opcode::MoveArg(0),
        Opcode::Value(f),
        Opcode::Apply,
        Opcode::DebugTop,
        Opcode::Return,
    ]);
    // let f = runtime.symbols.push_data(LvFunc::new(f, 1));
    runtime.symbols.define_label(main_text, &[
        Opcode::Unit,
        Opcode::Value(f),
        Opcode::Apply,
        Opcode::Return,
    ]);
    runtime.symbols.define_symbol(f, LvValue::from(LvFunc::new(f_text, 1)));
    let main = LvFunc::new(main_text, 0);
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
