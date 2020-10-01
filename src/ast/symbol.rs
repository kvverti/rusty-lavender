use std::fmt::{Display, Formatter};

/// A symbol is a scoped name associated with a value or type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct AstSymbol {
    /// The list of scopes associated with this symbol.
    scopes: Vec<String>,
}

impl AstSymbol {
    /// Creates a new unscoped symbol.
    pub fn new(name: String) -> Self {
        Self { scopes: vec![name] }
    }

    /// Adds the given scope name as the outermost scope for this symbol.
    pub fn place_in_scope(&mut self, scope_name: String) {
        self.scopes.insert(0, scope_name);
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
        let mut symb = AstSymbol::new("name".to_owned());
        symb.place_in_scope("c".to_owned());
        symb.place_in_scope("b".to_owned());
        symb.place_in_scope("a".to_owned());
        let display = format!("{}", symb);
        assert_eq!(display, "a::b::c::name");
    }
}
