use crate::ast::symbol::{AstSymbol, ExtractSymbol, SymbolContext, SymbolData, SymbolSpace};
use crate::ast::symbol::fixity::InfixNamespace;
use crate::parser::typedecl::{TypeExpression, TypePrimary};

impl ExtractSymbol for TypePrimary {
    fn extract(&self, data: &mut SymbolData, ctx: SymbolContext) {
        match self {
            // type IDs are unbound symbols (at first)
            Self::TypeIdentifier(id) => {
                let symbol = AstSymbol::from_scopes(SymbolSpace::Type, &id.to_scopes());
                data.declare_unbound_symbol(ctx.enclosing_scope.clone(), symbol);
            }
            // type variables are bound to definition scope
            Self::TypeVariable(name) => {
                let symbol = AstSymbol::in_scope(SymbolSpace::Type, ctx.enclosing_definition, &name.0);
                data.declare_symbol(symbol);
            }
            // subexpressions pass through
            Self::TypeSubExpression(expr) => expr.extract(data, ctx),
            // a hole declares no symbols
            Self::TypeHole => {}
        }
    }
}

impl InfixNamespace for TypePrimary {
    const NAMESPACE: SymbolSpace = SymbolSpace::Type;
}

impl ExtractSymbol for TypeExpression {
    fn extract(&self, data: &mut SymbolData, ctx: SymbolContext) {
        match self {
            Self::TypeApplication(app) => app.extract(data, ctx),
            Self::TypeLambda(lambda) => lambda.extract(data, ctx),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::token::{Token, TokenStream};

    use super::*;

    #[test]
    fn extracts_names() {
        let input = "'a -> (for b. b -> 'a)";
        let input = Token::parse_sequence(input);
        let expr = TypeExpression::parse(TokenStream(&input)).unwrap().1;
        let mut data = SymbolData::new();
        let ctx = SymbolContext::new();
        let expected = SymbolData::from_parts(
            vec![
                AstSymbol::from_scopes(SymbolSpace::Type, &["a"]),
                AstSymbol::from_scopes(SymbolSpace::Type, &["1", "b"]),
            ].into_iter().collect(),
            vec![
                (AstSymbol::from_scopes(SymbolSpace::Value, &[]), AstSymbol::from_scopes(SymbolSpace::Type, &["->"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["1"]), AstSymbol::from_scopes(SymbolSpace::Type, &["->"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["1"]), AstSymbol::from_scopes(SymbolSpace::Type, &["b"])),
            ].into_iter().collect(),
        );
        expr.extract(&mut data, ctx);
        assert_eq!(data, expected);
    }

    #[test]
    fn extracts_inner_infix_scopes() {
        let input = "(for b. b -> 'a) -> (for b. b -> 'a)";
        let input = Token::parse_sequence(input);
        let expr = TypeExpression::parse(TokenStream(&input)).unwrap().1;
        let mut data = SymbolData::new();
        let ctx = SymbolContext::new();
        let expected = SymbolData::from_parts(
            vec![
                AstSymbol::from_scopes(SymbolSpace::Type, &["a"]),
                AstSymbol::from_scopes(SymbolSpace::Type, &["0", "b"]),
                AstSymbol::from_scopes(SymbolSpace::Type, &["1", "b"]),
            ].into_iter().collect(),
            vec![
                (AstSymbol::from_scopes(SymbolSpace::Value, &[]), AstSymbol::from_scopes(SymbolSpace::Type, &["->"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["0"]), AstSymbol::from_scopes(SymbolSpace::Type, &["->"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["0"]), AstSymbol::from_scopes(SymbolSpace::Type, &["b"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["1"]), AstSymbol::from_scopes(SymbolSpace::Type, &["->"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["1"]), AstSymbol::from_scopes(SymbolSpace::Type, &["b"])),
            ].into_iter().collect(),
        );
        expr.extract(&mut data, ctx);
        assert_eq!(data, expected);
    }

    #[test]
    fn extracts_inner_prefix_scopes() {
        let input = "(for b. b -> 'a) (for b. b -> 'a)";
        let input = Token::parse_sequence(input);
        let expr = TypeExpression::parse(TokenStream(&input)).unwrap().1;
        let mut data = SymbolData::new();
        let ctx = SymbolContext::new();
        let expected = SymbolData::from_parts(
            vec![
                AstSymbol::from_scopes(SymbolSpace::Type, &["a"]),
                AstSymbol::from_scopes(SymbolSpace::Type, &["0", "b"]),
                AstSymbol::from_scopes(SymbolSpace::Type, &["1", "b"]),
            ].into_iter().collect(),
            vec![
                (AstSymbol::from_scopes(SymbolSpace::Value, &["0"]), AstSymbol::from_scopes(SymbolSpace::Type, &["->"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["0"]), AstSymbol::from_scopes(SymbolSpace::Type, &["b"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["1"]), AstSymbol::from_scopes(SymbolSpace::Type, &["->"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["1"]), AstSymbol::from_scopes(SymbolSpace::Type, &["b"])),
            ].into_iter().collect(),
        );
        expr.extract(&mut data, ctx);
        assert_eq!(data, expected);
    }
}
