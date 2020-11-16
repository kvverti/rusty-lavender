use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};

use crate::parser::tagged::Tagged;

mod definition;
mod fixity;
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

/// The global scope
pub static GLOBAL_SCOPE: AstSymbol = AstSymbol { nspace: SymbolSpace::Value, scopes: vec![] };

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
    fn extract(&self, data: &mut SymbolData, ctx: SymbolContext);
}

/// Contextual information from parent nodes in the AST.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct SymbolContext<'a> {
    /// The immediately enclosing scope. The namespace will always be Value.
    pub enclosing_scope: &'a AstSymbol,
    /// The closest enclosing definition. The namespace will always be Value.
    pub enclosing_definition: &'a AstSymbol,
    /// An index used for anonymous scopes (such as in lambdas)
    pub scope_idx: u32,
}

impl<'a> SymbolContext<'a> {
    /// Creates a new symbol context with the minimum scope index and with no enclosing scopes.
    pub fn new() -> Self {
        Self {
            enclosing_scope: &GLOBAL_SCOPE,
            enclosing_definition: &GLOBAL_SCOPE,
            scope_idx: 0,
        }
    }

    /// Returns a new context with the enclosing scope replaced.
    pub fn with_enclosing_scope(mut self, enclosing_scope: &'a AstSymbol) -> Self {
        self.enclosing_scope = enclosing_scope;
        self
    }

    /// Returns a new context with the enclosing definition replaced.
    pub fn with_enclosing_definition(mut self, enclosing_definition: &'a AstSymbol) -> Self {
        self.enclosing_definition = enclosing_definition;
        self
    }

    /// Returns a new context with the scope index replaced.
    pub fn with_scope_idx(mut self, scope_idx: u32) -> Self {
        self.scope_idx = scope_idx;
        self
    }
}

/// Semantic data extracted from the parse tree and associated with the AST.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SymbolData {
    /// The declared symbols in the tree.
    declared_symbols: HashMap<AstSymbol, Tagged<()>>,
    /// The yet unbound symbols in the tree, which will be resolved against the declared
    /// symbols.
    unbound_symbols: HashSet<(AstSymbol, AstSymbol)>,
}

impl SymbolData {
    pub fn new() -> Self {
        SymbolData {
            declared_symbols: HashMap::new(),
            unbound_symbols: HashSet::new(),
        }
    }

    /// Constructs a semantic data from parts, used in unit testing.
    #[cfg(test)]
    pub(crate) fn from_parts(declared_symbols: HashMap<AstSymbol, Tagged<()>>, unbound_symbols: HashSet<(AstSymbol, AstSymbol)>) -> Self {
        Self { declared_symbols, unbound_symbols }
    }

    /// Declares a symbol. If the symbol has been previously declared, no action is taken.
    pub fn declare_symbol(&mut self, symb: Tagged<AstSymbol>) {
        let Tagged { value, idx, len } = symb;
        self.declared_symbols.entry(value).or_insert(Tagged { value: (), idx, len });
    }

    /// Marks an unbound symbol found in the given scope.
    pub fn declare_unbound_symbol(&mut self, scope: AstSymbol, symb: AstSymbol) {
        self.unbound_symbols.insert((scope, symb));
    }

    /// Resolves an unbound symbol in some scope to a bound symbol in this symbol data.
    pub fn resolve_symbol(&self, scope: &AstSymbol, symbol: &AstSymbol) -> Option<AstSymbol> {
        let mut env_scopes = scope.scopes.clone();
        let mut env_len = env_scopes.len();
        let nspace = symbol.nspace;
        env_scopes.extend(symbol.scopes.iter().cloned());
        loop {
            let candidate_symbol = AstSymbol { nspace, scopes: env_scopes };
            if self.declared_symbols.contains_key(&candidate_symbol) {
                return Some(candidate_symbol);
            } else {
                env_scopes = candidate_symbol.scopes;
                if env_len == 0 {
                    // no symbol found
                    return None;
                } else {
                    env_len -= 1;
                    env_scopes.remove(env_len);
                }
            }
        }
    }
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

    #[test]
    fn test_symbol_resolution() {
        let data = SymbolData {
            declared_symbols: vec![
                (AstSymbol::from_scopes(SymbolSpace::Value, &["a", "b", "c"]), Tagged::new(())),
                (AstSymbol::from_scopes(SymbolSpace::Type, &["a", "b", "d"]), Tagged::new(())),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["a", "d"]), Tagged::new(())),
            ].into_iter().collect(),
            unbound_symbols: Default::default(),
        };
        let scope = AstSymbol::from_scopes(SymbolSpace::Value, &["a", "b"]);
        let sym1 = AstSymbol::new(SymbolSpace::Value, "c");
        let sym2 = AstSymbol::new(SymbolSpace::Value, "d");
        let sym3 = AstSymbol::from_scopes(SymbolSpace::Value, &["b", "c"]);
        let sym_n = AstSymbol::new(SymbolSpace::Type, "c");
        let res1 = data.resolve_symbol(&scope, &sym1);
        let res2 = data.resolve_symbol(&scope, &sym2);
        let res3 = data.resolve_symbol(&scope, &sym3);
        let res_n = data.resolve_symbol(&scope, &sym_n);
        assert!(res_n.is_none());
        let res1 = format!("{}", res1.unwrap());
        let res2 = format!("{}", res2.unwrap());
        let res3 = format!("{}", res3.unwrap());
        assert_eq!(&res1, "value/a::b::c");
        assert_eq!(&res2, "value/a::d");
        assert_eq!(&res3, "value/a::b::c");
    }
}
