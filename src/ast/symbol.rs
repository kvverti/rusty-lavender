/// A symbol is a scoped name associated with a value or type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct AstSymbol {
    /// The list of scopes associated with this symbol.
    scopes: Vec<String>,
}
