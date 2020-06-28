use crate::code::Opcode;
use crate::runtime::symbol::reference::{SymbolicReference, TextLabel};
use crate::value::LvValue;

/// Contains reference structs.
pub mod reference;

/// A symbol which refers to a constant value.
struct Symbol {
    /// The declared name of the symbol.
    name: String,
    /// The half-open range of indices of the value this symbol is associated with.
    address: Option<usize>,
}

/// Symbol table for runtime. Stores constant values and code.
pub struct SymbolTable {
    symbols: Vec<Symbol>,
    data: Vec<LvValue>,
    text: Vec<Opcode>,
}

impl SymbolTable {
    /// Creates a new, empty symbol table.
    pub fn new() -> Self {
        Self {
            symbols: Vec::new(),
            data: Vec::new(),
            text: Vec::new(),
        }
    }

    /// Creates a new symbolic reference for some as yet undefined value.
    pub fn create_symbol(&mut self, name: &str) -> SymbolicReference {
        let sym = Symbol {
            name: name.to_owned(),
            address: None,
        };
        let idx = self.symbols.len();
        self.symbols.push(sym);
        unsafe { SymbolicReference::from_raw(idx) }
    }

    /// Associates a symbolic reference with the given value.
    pub fn define_symbol(&mut self, sym_ref: SymbolicReference, v: LvValue) {
        let sym = &mut self.symbols[sym_ref.idx];
        let address = self.data.len();
        self.data.push(v);
        sym.address = Some(address);
    }

    /// Creates a new label for some as yet undefined location in code.
    pub fn create_label(&mut self, name: &str) -> TextLabel {
        let sym = Symbol {
            name: name.to_owned(),
            address: None,
        };
        let idx = self.symbols.len();
        self.symbols.push(sym);
        unsafe { TextLabel::from_raw(idx) }
    }

    /// Associates the given label with the given code block.
    pub fn define_label(&mut self, label: TextLabel, code: &[Opcode]) {
        let sym = &mut self.symbols[label.idx];
        let address = self.text.len();
        self.text.extend_from_slice(code);
        sym.address = Some(address);
    }

    /// Dereferences a symbolic reference to yield the referred to value.
    pub fn resolve_symbol(&self, r: SymbolicReference) -> &LvValue {
        let sym = &self.symbols[r.idx];
        let idx = sym.address.expect(&sym.name);
        &self.data[idx]
    }

    /// Dereferences a text reference to yield the underlying code block.
    pub fn resolve_label(&self, r: TextLabel) -> usize {
        let sym = &self.symbols[r.idx];
        sym.address.expect(&sym.name)
    }

    #[inline]
    pub fn opcode_at(&self, i: usize) -> Opcode {
        self.text[i]
    }
}