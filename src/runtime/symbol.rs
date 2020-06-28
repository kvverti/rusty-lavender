use crate::code::Opcode;
use crate::value::LvValue;
use crate::runtime::symbol::reference::{DataReference, TextReference};

/// Contains reference structs.
pub mod reference;

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

    pub fn get_data(&self, r: &DataReference) -> &LvValue {
        &self.data[r.idx]
    }

    pub fn get_text(&self, r: &TextReference) -> &Opcode {
        &self.text[r.idx]
    }
}