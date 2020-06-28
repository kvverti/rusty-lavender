use crate::code::Opcode;
use crate::runtime::stack::StackFrame;
use crate::value::func::LvFunc;
use crate::value::LvValue;

/// Contains implementations of the opcodes.
mod intrinsic;
/// Contains reference structs.
mod reference;
/// Contains the stack frame implementation.
mod stack;

pub type DataReference = reference::DataReference;
pub type TextReference = reference::TextReference;

/// Symbol table for runtime. Stores constant values and code.
pub struct SymbolTable {
    data: Vec<LvValue>,
    text: Vec<Opcode>,
}

impl SymbolTable {
    /// Creates a new, empty symbol table.
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            text: Vec::new(),
        }
    }

    /// Appends the given value to the data stack, and returns a reference to that data.
    pub fn push_data<T: Into<LvValue>>(&mut self, v: T) -> DataReference {
        let idx = self.data.len();
        self.data.push(v.into());
        unsafe { DataReference::from_raw(idx) }
    }

    /// Appends the given code to the text stack, and returns a reference to that text.
    pub fn push_text(&mut self, t: &[Opcode]) -> TextReference {
        let idx = self.text.len();
        self.text.append(&mut t.to_vec());
        unsafe { TextReference::from_raw(idx) }
    }
}

/// A complete runtime context for Lavender.
pub struct RuntimeContext {
    /// The symbol table, where data and text are stored.
    pub symbols: SymbolTable,
    /// A stack where frame information is stored.
    frames: Vec<StackFrame>,
    /// A stack where intermediate values are stored.
    values: Vec<LvValue>,
    /// The program counter.
    pc: TextReference,
}

impl RuntimeContext {
    /// Creates a new, empty runtime context.
    pub fn new() -> Self {
        Self {
            symbols: SymbolTable::new(),
            frames: Vec::new(),
            values: Vec::new(),
            pc: Default::default(),
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
        self.pc = f.text;
    }

    /// Executes the given operation and updates state as appropriate.
    fn step(&mut self) {
        use Opcode::*;
        let op = self.pc.as_ref(&self.symbols);
        self.pc += 1;
        match op {
            Unit => self.values.push(LvValue::Unit),
            Value(d) => self.values.push(d.as_ref(&self.symbols).clone()),
            MoveArg(idx) => {
                let idx = usize::from(*idx);
                let mut arg = LvValue::Unit;
                let frame = self.frames.last_mut().unwrap();
                let top = if idx < stack::LOCAL_SIZE {
                    &mut frame.locals[idx]
                } else {
                    &mut self.values[frame.fp + (idx - stack::LOCAL_SIZE)]
                };
                std::mem::swap(&mut arg, top);
                self.values.push(arg);
            }
            CopyArg(idx) => {
                let idx = usize::from(*idx);
                let frame = self.frames.last().unwrap();
                let arg = if idx < stack::LOCAL_SIZE {
                    frame.locals[idx].clone()
                } else {
                    self.values[frame.fp + (idx - stack::LOCAL_SIZE)].clone()
                };
                self.values.push(arg);
            }
            AddInt => {
                use LvValue::Integer;
                let b = self.values.pop().unwrap();
                let a = self.values.pop().unwrap();
                if let (Integer(a), Integer(b)) = (a, b) {
                    let c = intrinsic::addi(a, b);
                    self.values.push(LvValue::from(c));
                } else {
                    panic!("Expected integers");
                }
            }
            Apply => {
                use LvValue::Function;
                let func = self.values.pop().unwrap();
                if let Function(mut func) = func {
                    if func.arity != 0 {
                        // apply argument to function
                        func.apply(self.values.pop().unwrap());
                        self.values.push(LvValue::from(func));
                    } else {
                        // evaluate function, apply when it returns (to the same instruction)
                        self.pc -= 1;
                        self.step_into(func);
                    }
                } else {
                    panic!("Expected function");
                }
            }
            Return => {
                let frame = self.frames.pop().unwrap();
                let ret_val = self.values.pop().unwrap();
                self.pc = frame.ret;
                while self.values.len() > frame.fp {
                    self.values.pop();
                }
                self.values.push(ret_val);
            }
            DebugTop => println!("Stack {:#}, Frame {:#}, Pc {:?} \n{:?}",
                                 self.values.len(),
                                 self.frames.len(),
                                 self.pc,
                                 self.values.last().unwrap()),
        }
    }
}
