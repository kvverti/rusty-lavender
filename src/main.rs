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
    // addition
    // def f a b = <intrinsic>
    // def main = f (f 1 2) 3   (evaluates to 6)
    let one = runtime.symbols.create_symbol("1");
    let two = runtime.symbols.create_symbol("2");
    let three = runtime.symbols.create_symbol("3");
    runtime.symbols.define_symbol(one, LvValue::from(1));
    runtime.symbols.define_symbol(two, LvValue::from(2));
    runtime.symbols.define_symbol(three, LvValue::from(3));
    let f = runtime.symbols.create_symbol("f");
    let f_text = runtime.symbols.create_label("f");
    let main_text = runtime.symbols.create_label("main");
    runtime.symbols.define_label(f_text,&[
        Opcode::MoveArg(0),
        Opcode::Eval,
        Opcode::MoveArg(1),
        Opcode::Eval,
        Opcode::AddInt,
        Opcode::Return,
    ]);
    runtime.symbols.define_label(main_text, &[
        Opcode::Value(three),
        Opcode::Value(two),
        Opcode::Value(one),
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
