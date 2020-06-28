use crate::code::Opcode;
use crate::runtime::stack::StackFrame;
use crate::value::func::LvFunc;
use crate::value::LvValue;
use crate::runtime::symbol::SymbolTable;

/// Contains implementations of the opcodes.
mod intrinsic;
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
            Unit => self.values.push(LvValue::Unit),
            Value(d) => self.values.push(self.symbols.resolve_symbol(d).clone()),
            MoveArg(idx) => {
                let idx = usize::from(idx);
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
                let idx = usize::from(idx);
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
