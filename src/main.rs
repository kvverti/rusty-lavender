use crate::code::Opcode;
use crate::runtime::RuntimeContext;
use crate::value::func::LvFunc;
use crate::value::LvValue;
use crate::runtime::intrinsic;

mod code;
mod runtime;
mod value;

fn main() {
    println!("Hello, world!");
    let mut runtime = RuntimeContext::new();
    // addition
    // def f a b = <intrinsic>
    // def main = f (f 1 2) 3   (evaluates to 6)
    let f = runtime.symbols.symbol("f");
    let f_text = runtime.symbols.label("f");
    let main_text = runtime.symbols.label("main");
    runtime.symbols.define_label(f_text,&intrinsic::TEXT_ADD_INT);
    runtime.symbols.define_label(main_text, &[
        Opcode::IntValue(3),
        Opcode::IntValue(2),
        Opcode::IntValue(1),
        Opcode::Value(f),
        Opcode::Apply,
        Opcode::Apply,
        Opcode::Value(f),
        Opcode::Apply,
        Opcode::Apply,
        Opcode::Return,
    ]);
    runtime.symbols.define_symbol(f, LvValue::from(LvFunc::new(f_text, 2)));
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
