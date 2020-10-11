use std::fmt::{Display, Formatter};

use crate::ast::Extract;

/// A symbol is a scoped name associated with a value or type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct AstSymbol {
    /// The list of scopes associated with this symbol.
    scopes: Vec<String>,
}

impl AstSymbol {
    /// Creates a new unscoped symbol.
    pub fn new(name: &str) -> Self {
        Self { scopes: vec![name.into()] }
    }

    /// Creates a new symbol from the given list of scopes, the last of which being the
    /// simple name.
    pub fn new_scoped(scopes: &[&str]) -> Self {
        Self { scopes: scopes.iter().map(|&s| s.into()).collect() }
    }

    /// Adds the given scope name as the outermost scope for this symbol.
    pub fn place_in_scope(&mut self, scope_name: &str) {
        self.scopes.insert(0, scope_name.into());
    }
}

impl Display for AstSymbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.scopes[1..].iter()
            .fold(write!(f, "{}", self.scopes[0]),
                  |res, scope| res.and(write!(f, "::{}", scope)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display() {
        let mut symb = AstSymbol::new("name");
        symb.place_in_scope("c");
        symb.place_in_scope("b");
        symb.place_in_scope("a");
        let display = format!("{}", symb);
        assert_eq!(display, "a::b::c::name");
    }
}
