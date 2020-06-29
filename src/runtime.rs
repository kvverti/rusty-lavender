use crate::code::Opcode;
use crate::runtime::stack::StackFrame;
use crate::runtime::symbol::SymbolTable;
use crate::value::func::LvFunc;
use crate::value::LvValue;

/// Contains intrinsic function implementations and helpers.
#[allow(dead_code)]
pub mod intrinsic;
/// Contains the stack frame implementation.
mod stack;
/// Contains the symbol table.
mod symbol;

pub type SymbolicReference = symbol::reference::SymbolicReference;
pub type TextLabel = symbol::reference::TextLabel;

/// A complete runtime context for Lavender.
pub struct RuntimeContext {
    /// The symbol table, where data and text are stored.
    pub symbols: SymbolTable,
    /// A stack where frame information is stored.
    frames: Vec<StackFrame>,
    /// A stack where intermediate values are stored.
    values: Vec<LvValue>,
    /// The program counter.
    pc: usize,
}

impl RuntimeContext {
    /// Creates a new, empty runtime context.
    pub fn new() -> Self {
        Self {
            symbols: SymbolTable::new(),
            frames: Vec::new(),
            values: Vec::new(),
            pc: 0,
        }
    }

    /// Evaluates the given function and produces a result. The evaluation is only a single
    /// level deep.
    pub fn exec(&mut self, f: LvFunc) -> LvValue {
        let init_count = self.frames.len();
        self.step_into(f);
        while self.frames.len() > init_count {
            self.step();
        }
        self.values.pop().unwrap()
    }

    /// Pushes a new frame onto the stack.
    fn step_into(&mut self, f: LvFunc) {
        assert_eq!(f.arity, 0);
        let mut frame = StackFrame {
            locals: Default::default(),
            ret: self.pc,
            fp: self.values.len(),
        };
        // move arguments to locals
        let mut args = f.args;
        if args.len() <= stack::LOCAL_SIZE {
            let len = args.len();
            args.swap_with_slice(&mut frame.locals[..len]);
        } else {
            let (first, rest) = args.split_at_mut(stack::LOCAL_SIZE);
            frame.locals.swap_with_slice(first);
            self.values.extend_from_slice(rest);
        }
        self.frames.push(frame);
        self.pc = self.symbols.resolve_label(f.text);
    }

    /// Executes the given operation and updates state as appropriate.
    fn step(&mut self) {
        use Opcode::*;
        let op = self.symbols.opcode_at(self.pc);
        self.pc += 1;
        match op {
            UnitValue => self.values.push(LvValue::Unit),
            Value(d) => self.values.push(self.symbols.resolve_symbol(d).clone()),
            IntValue(n) => self.values.push(LvValue::from(i64::from(n))),
            MoveArg(idx) => {
                let idx = usize::from(idx);
                let mut arg = LvValue::Unit;
                let frame = self.frames.last_mut().expect("No stack frame from which to move argument");
                let top = if idx < stack::LOCAL_SIZE {
                    &mut frame.locals[idx]
                } else {
                    &mut self.values[frame.fp + (idx - stack::LOCAL_SIZE)]
                };
                std::mem::swap(&mut arg, top);
                self.values.push(arg);
            }
            CopyArg(idx) => {
                let idx = usize::from(idx);
                let frame = self.frames.last().expect("No stack frame from which to copy argument");
                let arg = if idx < stack::LOCAL_SIZE {
                    frame.locals[idx].clone()
                } else {
                    self.values[frame.fp + (idx - stack::LOCAL_SIZE)].clone()
                };
                self.values.push(arg);
            }
            AddInt => {
                let b = intrinsic::extract_int(&self.values.pop().expect("No stack value for AddInt"));
                let a = intrinsic::extract_int(&self.values.pop().expect("No stack value for AddInt"));
                self.values.push(LvValue::from(a.wrapping_add(b)));
            }
            SubInt => {
                let b = intrinsic::extract_int(&self.values.pop().expect("No stack value for SubInt"));
                let a = intrinsic::extract_int(&self.values.pop().expect("No stack value for SubInt"));
                self.values.push(LvValue::from(a.wrapping_sub(b)));
            }
            MulInt => {
                let b = intrinsic::extract_int(&self.values.pop().expect("No stack value for MulInt"));
                let a = intrinsic::extract_int(&self.values.pop().expect("No stack value for MulInt"));
                self.values.push(LvValue::from(a.wrapping_mul(b)));
            }
            DivInt => {
                let b = intrinsic::extract_int(&self.values.pop().expect("No stack value for DivInt"));
                let a = intrinsic::extract_int(&self.values.pop().expect("No stack value for DivInt"));
                self.values.push(LvValue::from(a.wrapping_div(b)));
            }
            RemInt => {
                let b = intrinsic::extract_int(&self.values.pop().expect("No stack value for RemInt"));
                let a = intrinsic::extract_int(&self.values.pop().expect("No stack value for RemInt"));
                self.values.push(LvValue::from(a.wrapping_rem(b)));
            }
            AndInt => {
                let b = intrinsic::extract_int(&self.values.pop().expect("No stack value for AndInt"));
                let a = intrinsic::extract_int(&self.values.pop().expect("No stack value for AndInt"));
                self.values.push(LvValue::from(a & b));
            }
            XorInt => {
                let b = intrinsic::extract_int(&self.values.pop().expect("No stack value for XorInt"));
                let a = intrinsic::extract_int(&self.values.pop().expect("No stack value for XorInt"));
                self.values.push(LvValue::from(a ^ b));
            }
            OrInt => {
                let b = intrinsic::extract_int(&self.values.pop().expect("No stack value for OrInt"));
                let a = intrinsic::extract_int(&self.values.pop().expect("No stack value for OrInt"));
                self.values.push(LvValue::from(a | b));
            }
            SllInt => {
                let b = intrinsic::extract_int(&self.values.pop().expect("No stack value for SllInt"));
                let a = intrinsic::extract_int(&self.values.pop().expect("No stack value for SllInt"));
                self.values.push(LvValue::from(a << (b & 63)));
            }
            SrlInt => {
                let b = intrinsic::extract_int(&self.values.pop().expect("No stack value for SrlInt"));
                let a = intrinsic::extract_int(&self.values.pop().expect("No stack value for SrlInt"));
                self.values.push(LvValue::from(((a as u64) >> (b & 63) as u64) as i64));
            }
            SraInt => {
                let b = intrinsic::extract_int(&self.values.pop().expect("No stack value for SraInt"));
                let a = intrinsic::extract_int(&self.values.pop().expect("No stack value for SraInt"));
                self.values.push(LvValue::from(a >> (b & 63)));
            }
            Apply => {
                use LvValue::Function;
                let func = self.values.pop().expect("No stack value for Apply");
                if let Function(mut func) = func {
                    if func.arity != 0 {
                        // apply argument to function
                        func.apply(self.values.pop().expect("No stack value for Apply"));
                        self.values.push(LvValue::from(func));
                    } else {
                        // necessary to evaluate function first. Automatically added here
                        // because Apply always needs the function to be evaluated and it's
                        // a simple case.
                        self.pc -= 1;
                        self.step_into(func);
                    }
                } else {
                    panic!("Expected function");
                }
            }
            Return => {
                let frame = self.frames.pop().expect("No stack frame to pop");
                let ret_val = self.values.pop().expect("Missing return value");
                self.pc = frame.ret;
                while self.values.len() > frame.fp {
                    self.values.pop();
                }
                self.values.push(ret_val);
            }
            Eval => {
                use LvValue::Function;
                let top = self.values.pop().expect("No stack value for Eval");
                if let Function(func) = top {
                    if func.arity == 0 {
                        // evaluate function, apply when it returns (to the same instruction)
                        self.pc -= 1;
                        self.step_into(func);
                    } else {
                        // already evaluated
                        self.values.push(LvValue::from(func));
                    }
                } else {
                    // already evaluated
                    self.values.push(top);
                }
            }
            DebugTop => println!("Stack {:#}, Frame {:#}, Pc {:?} \n{:?}",
                                 self.values.len(),
                                 self.frames.len(),
                                 self.pc,
                                 self.values.last().unwrap()),
        }
    }
}
