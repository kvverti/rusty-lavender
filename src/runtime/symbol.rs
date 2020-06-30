use std::collections::HashMap;

use crate::code::Opcode;
use crate::runtime::symbol::reference::{SymbolicReference, TextLabel};
use crate::value::LvValue;

/// Contains reference structs.
pub mod reference;

/// A symbol which refers to a constant value.
struct Symbol {
    /// The declared name of the symbol.
    name: String,
    /// The index of the value this symbol is associated with.
    address: Option<usize>,
}

#[derive(Eq, PartialEq, Hash)]
enum SymbolNamespace {
    Value,
    Label,
}

/// Symbol table for runtime. Stores constant values and code.
pub struct SymbolTable {
    symbol_map: HashMap<(SymbolNamespace, String), usize>,
    symbols: Vec<Symbol>,
    data: Vec<LvValue>,
    text: Vec<Opcode>,
}

impl SymbolTable {
    /// Creates a new, empty symbol table.
    pub fn new() -> Self {
        Self {
            symbol_map: HashMap::new(),
            symbols: Vec::new(),
            data: Vec::new(),
            text: Vec::new(),
        }
    }

    /// Creates or retrieves a symbolic reference to some value.
    pub fn symbol(&mut self, name: &str) -> SymbolicReference {
        let symbol_map = &mut self.symbol_map;
        let symbols = &mut self.symbols;
        let idx = *symbol_map.entry((SymbolNamespace::Value, name.to_owned())).or_insert_with(|| {
            let sym = Symbol {
                name: name.to_owned(),
                address: None,
            };
            let idx = symbols.len();
            symbols.push(sym);
            idx
        });
        SymbolicReference::from_raw(idx)
    }

    /// Returns whether the given symbol is defined.
    pub fn is_symbol_defined(&self, sym_ref: SymbolicReference) -> bool {
        self.symbols[sym_ref.idx].address.is_some()
    }

    /// Associates a symbolic reference with the given value.
    pub fn define_symbol(&mut self, sym_ref: SymbolicReference, v: LvValue) {
        let sym = &mut self.symbols[sym_ref.idx];
        if sym.address.is_some() {
            panic!("Symbol already defined");
        }
        let address = self.data.len();
        self.data.push(v);
        sym.address = Some(address);
    }

    /// Creates a new label for some as yet undefined location in code.
    pub fn label(&mut self, name: &str) -> TextLabel {
        let symbol_map = &mut self.symbol_map;
        let symbols = &mut self.symbols;
        let idx = *symbol_map.entry((SymbolNamespace::Label, name.to_owned())).or_insert_with(|| {
            let sym = Symbol {
                name: name.to_owned(),
                address: None,
            };
            let idx = symbols.len();
            symbols.push(sym);
            idx
        });
        TextLabel::from_raw(idx)
    }

    /// Returns whether the given symbol is defined.
    pub fn is_label_defined(&self, sym_ref: TextLabel) -> bool {
        self.symbols[sym_ref.idx].address.is_some()
    }

    /// Associates the given label with the given code block.
    pub fn define_label(&mut self, label: TextLabel, code: &[Opcode]) {
        let sym = &mut self.symbols[label.idx];
        if sym.address.is_some() {
            panic!("Label already defined");
        }
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

#[cfg(test)]
mod tests {
    use crate::code::Opcode;
    use crate::runtime::symbol::SymbolTable;
    use crate::value::LvValue;

    #[test]
    fn symbolic_reference() {
        let mut symbols = SymbolTable::new();
        let reference = symbols.symbol("test");
        let reference2 = symbols.symbol("test2");
        assert_ne!(reference, reference2, "distinct symbols are unequal");
        assert!(!symbols.is_symbol_defined(reference), "reference is not defined");
        assert!(!symbols.is_symbol_defined(reference2), "reference2 is not defined");
        symbols.define_symbol(reference, LvValue::from(42));
        assert!(symbols.is_symbol_defined(reference), "reference is defined");
        assert!(!symbols.is_symbol_defined(reference2), "reference2 is not defined");
        let forty_two = symbols.resolve_symbol(reference).clone();
        assert_eq!(LvValue::Integer(42), forty_two, "resolved symbols are equal");
        symbols.define_symbol(reference2, LvValue::from("hello".to_owned()));
        assert!(symbols.is_symbol_defined(reference2), "reference2 is defined");
        let hello = symbols.resolve_symbol(reference2).clone();
        assert_eq!(LvValue::String("hello".to_owned()), hello, "resolved symbols are equal");
        let reference3 = symbols.symbol("test");
        assert_eq!(reference, reference3, "duplicate symbols are equal");
        let forty_two = symbols.resolve_symbol(reference3).clone();
        assert_eq!(LvValue::Integer(42), forty_two, "resolved value for symbol test is still the same");
    }

    #[test]
    fn text_label() {
        let mut symbols = SymbolTable::new();
        let label = symbols.label("test");
        let label2 = symbols.label("test2");
        assert_ne!(label, label2, "distinct labels are unequal");
        assert!(!symbols.is_label_defined(label), "label is not defined");
        assert!(!symbols.is_label_defined(label2), "label2 is not defined");
        let code = [Opcode::DebugTop; 30];
        symbols.define_label(label, &code);
        assert!(symbols.is_label_defined(label), "label is defined");
        assert!(!symbols.is_label_defined(label2), "label2 is not defined");
        let add = symbols.resolve_label(label);
        for i in 0..code.len() {
            if let (Opcode::DebugTop, Opcode::DebugTop) = (code[i], symbols.opcode_at(add + i)) {} else {
                panic!("Unequal opcodes");
            }
        }
    }
}