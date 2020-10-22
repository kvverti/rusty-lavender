use std::fmt::{Display, Formatter};
use std::mem::swap;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum SymbolSpace {
    Value,
    Type,
}

impl Display for SymbolSpace {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value => write!(f, "value"),
            Self::Type => write!(f, "type"),
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

    /// Adds the given scope name as the outermost scope for this symbol. The namespace of the
    /// symbol is ignored.
    #[deprecated]
    fn place_in_scope(&mut self, scope_name: AstSymbol) {
        let mut sc = scope_name.scopes;
        sc.append(&mut self.scopes);
        swap(&mut sc, &mut self.scopes);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[allow(deprecated)]
    fn test_display() {
        let mut symb = AstSymbol::new(SymbolSpace::Value, "name");
        symb.place_in_scope(AstSymbol::new(SymbolSpace::Value, "c"));
        symb.place_in_scope(AstSymbol::new(SymbolSpace::Value, "b"));
        symb.place_in_scope(AstSymbol::new(SymbolSpace::Value, "a"));
        let display = format!("{}", symb);
        assert_eq!(display, "value/a::b::c::name");
    }
}
