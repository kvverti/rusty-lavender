#[cfg(test)]
use std::cell::RefCell;
use std::fmt::{Display, Formatter};

use crate::ast::symbol::lookup::Lookup;
use crate::parser::item::Fixity;
use crate::parser::tagged::Tagged;

mod definition;
mod fixity;
mod lookup;
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

impl Default for SymbolSpace {
    fn default() -> Self {
        Self::Value
    }
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
pub static GLOBAL_SCOPE: AstSymbol = AstSymbol {
    nspace: SymbolSpace::Value,
    scopes: vec![],
};

/// A symbol is a scoped name associated with a value or type.
#[derive(Clone, Debug, Default, Eq, PartialEq, Hash)]
pub struct AstSymbol {
    /// The namespace of symbols this symbol is in.
    nspace: SymbolSpace,
    /// The list of scopes associated with this symbol.
    scopes: Vec<String>,
}

impl AstSymbol {
    /// Creates a new unscoped symbol.
    pub fn new(nspace: SymbolSpace, name: &str) -> Self {
        Self {
            nspace,
            scopes: vec![name.into()],
        }
    }

    /// Creates a symbol in the scope of the given symbol.
    pub fn in_scope(nspace: SymbolSpace, scope: &AstSymbol, name: &str) -> Self {
        let mut scopes = scope.scopes.clone();
        scopes.push(name.into());
        Self { nspace, scopes }
    }

    /// Creates a symbol from the given namespace and scopes.
    pub fn from_scopes(nspace: SymbolSpace, scopes: &[&str]) -> Self {
        Self {
            nspace,
            scopes: scopes.iter().map(|&s| s.into()).collect(),
        }
    }

    /// Returns the list of scopes, from outermost to innermost.
    pub fn as_scopes(&self) -> &[String] {
        &self.scopes
    }
}

impl Display for AstSymbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let prefix = write!(f, "{}/", self.nspace);
        self.scopes[1..]
            .iter()
            .fold(prefix.and(write!(f, "{}", self.scopes[0])), |res, scope| {
                res.and(write!(f, "::{}", scope))
            })
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
    pub implicit_scope: &'a AstSymbol,
}

impl<'a> SymbolContext<'a> {
    /// Creates a new symbol context with the minimum scope index and with no enclosing scopes.
    pub fn new() -> Self {
        Self {
            enclosing_scope: &GLOBAL_SCOPE,
            enclosing_definition: &GLOBAL_SCOPE,
            implicit_scope: &GLOBAL_SCOPE,
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
    pub fn with_implicit_scope(mut self, implicit_scope: &'a AstSymbol) -> Self {
        self.implicit_scope = implicit_scope;
        self
    }
}

impl<'a> Default for SymbolContext<'a> {
    fn default() -> Self {
        Self::new()
    }
}

/// Struct to use in AST construction / symbol resolution tests.
#[cfg(test)]
#[derive(Clone, Default, Debug, Eq, PartialEq)]
struct SymbolTally {
    /// Symbols expected to be resolved when constructing the AST.
    expected_symbols: Vec<(AstSymbol, AstSymbol)>,
    /// Symbols that are erroneously passed to be resolved.
    erroneous_symbols: Vec<(AstSymbol, AstSymbol)>,
}

#[derive(Clone, Default, Debug, Eq, PartialEq)]
struct SymbolLookup {
    symbols: Vec<(Tagged<AstSymbol>, Fixity)>,
    lookup: Lookup,
}

impl SymbolLookup {
    const fn new() -> Self {
        Self {
            symbols: Vec::new(),
            lookup: Lookup::new(),
        }
    }

    fn insert(&mut self, symb: Tagged<AstSymbol>, fix: Fixity) {
        if self.lookup.insert(symb.value.as_scopes()) {
            self.symbols.push((symb, fix));
        }
    }
}

/// Semantic data extracted from the parse tree and associated with the AST.
#[derive(Clone, Default, Debug, Eq, PartialEq)]
pub struct SymbolData {
    /// Lookup for value symbols.
    values: SymbolLookup,
    /// Lookup for type symbols.
    types: SymbolLookup,
    /// Lookup for pattern symbols.
    patterns: SymbolLookup,
    /// Symbols expected to be resolved when constructing the AST.
    #[cfg(test)]
    tally: RefCell<SymbolTally>,
}

impl SymbolData {
    pub fn new() -> Self {
        SymbolData {
            values: SymbolLookup::new(),
            types: SymbolLookup::new(),
            patterns: SymbolLookup::new(),
            #[cfg(test)]
            tally: RefCell::default(),
        }
    }

    /// Constructs a semantic data from parts, used in unit testing.
    #[cfg(test)]
    pub(crate) fn from_parts(
        declared_symbols: Vec<(AstSymbol, Tagged<Fixity>)>,
        expected_symbols: Vec<(AstSymbol, AstSymbol)>,
    ) -> Self {
        let mut data = Self {
            values: SymbolLookup::new(),
            types: SymbolLookup::new(),
            patterns: SymbolLookup::new(),
            tally: RefCell::new(SymbolTally {
                expected_symbols,
                erroneous_symbols: vec![],
            }),
        };
        for (symb, fix) in declared_symbols {
            data.select_lookup_mut(symb.nspace)
                .insert(fix.map(|_| symb), fix.value);
        }
        data
    }

    /// Asserts the expected symbol resolution took place.
    #[cfg(test)]
    pub(crate) fn assert_resolved(&self) {
        let tally = self.tally.borrow();
        assert!(
            tally.erroneous_symbols.is_empty(),
            "Unexpected symbols: {:#?}",
            tally.erroneous_symbols
        );
        assert!(
            tally.expected_symbols.is_empty(),
            "Expected symbols: {:#?}",
            tally.expected_symbols
        );
    }

    /// Declares a symbol. If the symbol has been previously declared, no action is taken.
    pub fn declare(&mut self, symb: Tagged<AstSymbol>) {
        self.declare_with_data(symb, Fixity::None);
    }

    pub fn declare_with_data(&mut self, symb: Tagged<AstSymbol>, fixity: Fixity) {
        self.select_lookup_mut(symb.value.nspace)
            .insert(symb, fixity);
    }

    /// Asserts and returns a symbol previously declared.
    pub fn get<S, I>(&self, nspace: SymbolSpace, symbol: I) -> &AstSymbol
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        let sl = self.select_lookup(nspace);
        let key = sl.lookup.get(symbol).expect("Declared symbol not found");
        &sl.symbols[key.index()].0.value
    }

    /// Resolves an unbound symbol in some scope to a bound symbol in this symbol data.
    pub fn resolve(&self, scope: &AstSymbol, symbol: AstSymbol) -> Option<(&AstSymbol, Fixity)> {
        #[cfg(test)]
        {
            // tally resolved symbols to make sure they are expected
            let mut tally = self.tally.borrow_mut();
            let removed = tally
                .expected_symbols
                .iter()
                .position(|(sc, sy)| sc == scope && sy == &symbol);
            if let Some(idx) = removed {
                tally.expected_symbols.remove(idx);
            } else {
                tally
                    .erroneous_symbols
                    .push((scope.clone(), symbol.clone()));
            }
        }
        let sl = self.select_lookup(symbol.nspace);
        let key = sl.lookup.resolve(scope.as_scopes(), symbol.as_scopes())?;
        let (ref symb, fix) = sl.symbols[key.index()];
        Some((&symb.value, fix))
    }

    fn select_lookup(&self, nspace: SymbolSpace) -> &SymbolLookup {
        match nspace {
            SymbolSpace::Value => &self.values,
            SymbolSpace::Type => &self.types,
            SymbolSpace::Pattern => &self.patterns,
        }
    }

    fn select_lookup_mut(&mut self, nspace: SymbolSpace) -> &mut SymbolLookup {
        match nspace {
            SymbolSpace::Value => &mut self.values,
            SymbolSpace::Type => &mut self.types,
            SymbolSpace::Pattern => &mut self.patterns,
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
        let scope = AstSymbol::from_scopes(SymbolSpace::Value, &["a", "b"]);
        let sym1 = AstSymbol::new(SymbolSpace::Value, "c");
        let sym2 = AstSymbol::new(SymbolSpace::Value, "d");
        let sym3 = AstSymbol::from_scopes(SymbolSpace::Value, &["b", "c"]);
        let sym_n = AstSymbol::new(SymbolSpace::Type, "c");
        let data = SymbolData::from_parts(
            vec![
                (
                    AstSymbol::from_scopes(SymbolSpace::Value, &["a", "b", "c"]),
                    Tagged::new(Fixity::None),
                ),
                (
                    AstSymbol::from_scopes(SymbolSpace::Type, &["a", "b", "d"]),
                    Tagged::new(Fixity::None),
                ),
                (
                    AstSymbol::from_scopes(SymbolSpace::Value, &["a", "d"]),
                    Tagged::new(Fixity::None),
                ),
            ]
            .into_iter()
            .collect(),
            vec![
                (scope.clone(), sym1.clone()),
                (scope.clone(), sym2.clone()),
                (scope.clone(), sym3.clone()),
                (scope.clone(), sym_n.clone()),
            ]
            .into_iter()
            .collect(),
        );
        let res1 = data.resolve(&scope, sym1);
        let res2 = data.resolve(&scope, sym2);
        let res3 = data.resolve(&scope, sym3);
        let res_n = data.resolve(&scope, sym_n);
        assert!(res_n.is_none());
        let res1 = format!("{}", res1.unwrap().0);
        let res2 = format!("{}", res2.unwrap().0);
        let res3 = format!("{}", res3.unwrap().0);
        assert_eq!(&res1, "value/a::b::c");
        assert_eq!(&res2, "value/a::d");
        assert_eq!(&res3, "value/a::b::c");
        data.assert_resolved();
    }
}
