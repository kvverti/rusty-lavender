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
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SemanticContext {
    /// The immediately enclosing scope. The namespace will always be Value.
    pub enclosing_scope: AstSymbol,
    /// The closest enclosing definition. The namespace will always be Value.
    pub enclosing_definition: AstSymbol,
}

/// Semantic data extracted from the parse tree and associated with the AST.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SemanticData {
    /// The declared symbols in the tree.
    declared_symbols: HashSet<AstSymbol>,
    /// The yet unbound symbols in the tree, which will be resolved against the declared
    /// symbols.
    unbound_symbols: HashSet<(AstSymbol, AstSymbol)>,
}

impl SemanticData {
    pub fn new() -> Self {
        SemanticData {
            declared_symbols: HashSet::new(),
            unbound_symbols: HashSet::new(),
        }
    }

    /// Constructs a semantic data from parts, used in unit testing.
    #[cfg(test)]
    pub(crate) fn from_parts(declared_symbols: HashSet<AstSymbol>, unbound_symbols: HashSet<(AstSymbol, AstSymbol)>) -> Self {
        Self { declared_symbols, unbound_symbols }
    }

    /// Declares a symbol.
    pub fn declare_symbol(&mut self, symb: AstSymbol) {
        self.declared_symbols.insert(symb);
    }

    /// Marks an unbound symbol found in the given scope.
    pub fn declare_unbound_symbol(&mut self, scope: AstSymbol, symb: AstSymbol) {
        self.unbound_symbols.insert((scope, symb));
    }
}
