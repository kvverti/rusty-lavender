//! The AST module contains the intermediate representation for Lavender after parsing produces
//! a parse tree. The AST resolves symbols, transforms infix application into prefix application
//! with the appropriate fixity, and performs type checking and inference, as well as various
//! inlining and optimizations. The AST is then used to generate bytecode which can be executed
//! by the runtime.

use nom::lib::std::collections::HashSet;

use crate::ast::symbol::AstSymbol;

pub mod symbol;
pub mod types;

/// Extracts a collection of values from some type. This trait is used for walking the parse
/// tree and extracting names, types, definitions, etc.
pub trait Extract {
    /// Extracts a collection of values.
    fn extract(&self, data: &mut SemanticData, ctx: &SemanticContext);
}

/// Contextual information from parent nodes in the AST.
pub struct SemanticContext {
    pub enclosing_scope: AstSymbol,
    pub enclosing_definition: AstSymbol,
}

pub struct SemanticData {
    declared_symbols: HashSet<AstSymbol>,
    unbound_symbols: HashSet<AstSymbol>,
}

impl SemanticData {
    pub fn declare_symbol(&mut self, symb: AstSymbol) {
        self.declared_symbols.insert(symb);
    }

    pub fn declare_unbound_symbol(&mut self, symb: AstSymbol) {
        self.unbound_symbols.insert(symb);
    }
}
