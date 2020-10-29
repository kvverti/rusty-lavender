use crate::ast::symbol::{AstSymbol, ExtractSymbol, SymbolContext, SymbolData, SymbolSpace};
use crate::ast::symbol::fixity::InfixNamespace;
use crate::parser::value::{ValueExpression, ValuePrimary};

impl InfixNamespace for ValuePrimary {
    const NAMESPACE: SymbolSpace = SymbolSpace::Value;
}

impl ExtractSymbol for ValuePrimary {
    fn extract(&self, data: &mut SymbolData, ctx: SymbolContext) {
        match self {
            // literals declare no symbols
            Self::Literal(_) => {}
            // scoped IDs in value expressions are unbound symbols
            Self::Identifier(id) => {
                let symbol = AstSymbol::from_scopes(SymbolSpace::Value, &id.to_scopes());
                data.declare_unbound_symbol(ctx.enclosing_scope.clone(), symbol);
            }
            // subexpressions pass through symbols
            Self::SubExpression(expr) => expr.extract(data, ctx),
        }
    }
}

impl ExtractSymbol for ValueExpression {
    fn extract(&self, data: &mut SymbolData, ctx: SymbolContext) {
        match self {
            Self::Application(expr) => expr.extract(data, ctx),
            Self::Lambda(lambda) => lambda.extract(data, ctx),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::parser::token::{Token, TokenStream};

    use super::*;

    #[test]
    fn extracts_names() {
        let input = "a @ (lam (Some b, c). b + c + (lam _. a))";
        let input = Token::parse_sequence(input);
        let expr = ValueExpression::parse(TokenStream(&input)).unwrap().1;
        let mut data = SymbolData::new();
        let ctx = SymbolContext::new();
        let expected = SymbolData::from_parts(
            vec![
                AstSymbol::from_scopes(SymbolSpace::Value, &["1", "b"]),
                AstSymbol::from_scopes(SymbolSpace::Value, &["1", "c"]),
            ].into_iter().collect(),
            vec![
                (AstSymbol::from_scopes(SymbolSpace::Value, &[]), AstSymbol::from_scopes(SymbolSpace::Value, &["a"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &[]), AstSymbol::from_scopes(SymbolSpace::Value, &["@"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["1"]), AstSymbol::from_scopes(SymbolSpace::Pattern, &["Some"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["1"]), AstSymbol::from_scopes(SymbolSpace::Pattern, &[","])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["1"]), AstSymbol::from_scopes(SymbolSpace::Value, &["b"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["1"]), AstSymbol::from_scopes(SymbolSpace::Value, &["+"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["1"]), AstSymbol::from_scopes(SymbolSpace::Value, &["c"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["1", "3"]), AstSymbol::from_scopes(SymbolSpace::Value, &["a"])),
            ].into_iter().collect(),
        );
        expr.extract(&mut data, ctx);
        assert_eq!(data, expected);
    }
}
