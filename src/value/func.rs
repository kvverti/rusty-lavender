use crate::runtime::TextLabel;
use crate::value::LvValue;

/// A function with some number of bound and free variables.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LvFunc {
    /// The text that defines this function.
    pub text: TextLabel,
    /// The arity of this function, i.e. the number of bound variables.
    pub arity: u8,
    /// The applied arguments to this function, i.e. the free variables.
    pub args: Vec<LvValue>,
}

impl LvFunc {
    /// Constructs a function defined from the given text and that takes the given number
    /// of parameters. The returned function has no applied parameters.
    pub fn new(text: TextLabel, arity: u8) -> Self {
        Self {
            text,
            arity,
            args: Vec::with_capacity(usize::from(arity)),
        }
    }

    /// Applies an argument to this function.
    pub fn apply(&mut self, arg: LvValue) {
        assert_ne!(self.arity, 0);
        self.args.push(arg);
        self.arity -= 1;
    }
}
