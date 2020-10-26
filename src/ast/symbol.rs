mod fixity;

use std::collections::HashSet;
use std::fmt::{Display, Formatter};

mod pattern;
mod typedecl;
mod value;

/// The namespace a symbol is in. Namespaces separate otherwise identical symbols.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum SymbolSpace {
    /// The namespace for values and definitions.
    Value,
    /// The namespace for types.
    Type,
    /// The namespace for patterns.
    Pattern,
}

impl Display for SymbolSpace {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value => write!(f, "value"),
            Self::Type => write!(f, "type"),
            Self::Pattern => write!(f, "pattern"),
        }
    }
}

/// A symbol is a scoped name associated with a value or type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct AstSymbol {
    /// The namespace of symbols this symbol is in.
    nspace: SymbolSpace,
    /// The list of scopes associated with this symbol.
    scopes: Vec<String>,
}

impl AstSymbol {
    /// Creates a new unscoped symbol.
    pub fn new(nspace: SymbolSpace, name: &str) -> Self {
        Self { nspace, scopes: vec![name.into()] }
    }

    /// Creates a symbol in the scope of the given symbol.
    pub fn in_scope(nspace: SymbolSpace, scope: &AstSymbol, name: &str) -> Self {
        let mut scopes = scope.scopes.clone();
        scopes.push(name.into());
        Self {
            nspace,
            scopes,
        }
    }

    /// Creates a symbol from the given namespace and scopes.
    pub fn from_scopes(nspace: SymbolSpace, scopes: &[&str]) -> Self {
        Self {
            nspace,
            scopes: scopes.iter().map(|&s| s.into()).collect(),
        }
    }
}

impl Display for AstSymbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let prefix = write!(f, "{}/", self.nspace);
        self.scopes[1..].iter()
            .fold(prefix.and(write!(f, "{}", self.scopes[0])),
                  |res, scope| res.and(write!(f, "::{}", scope)))
    }
}

/// Extracts a collection of values from some type. This trait is used for walking the parse
/// tree and extracting names, types, definitions, etc.
pub trait ExtractSymbol {
    /// Extracts a collection of values.
    fn extract(&self, data: &mut SymbolData, ctx: &SymbolContext);
}

/// Contextual information from parent nodes in the AST.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SymbolContext {
    /// The immediately enclosing scope. The namespace will always be Value.
    pub enclosing_scope: AstSymbol,
    /// The closest enclosing definition. The namespace will always be Value.
    pub enclosing_definition: AstSymbol,
}

impl SymbolData {
    pub fn new() -> Self {
        SymbolData {
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

/// Semantic data extracted from the parse tree and associated with the AST.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SymbolData {
    /// The declared symbols in the tree.
    declared_symbols: HashSet<AstSymbol>,
    /// The yet unbound symbols in the tree, which will be resolved against the declared
    /// symbols.
    unbound_symbols: HashSet<(AstSymbol, AstSymbol)>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_in_scope() {
        let scope = AstSymbol::from_scopes(SymbolSpace::Value, &["x", "y"]);
        let input = "a";
        let expected = "value/x::y::a";
        let symbol = AstSymbol::in_scope(SymbolSpace::Value, &scope, input);
        let result = format!("{}", symbol);
        assert_eq!(result, expected);
    }
}
