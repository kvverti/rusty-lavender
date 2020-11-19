use crate::ast::symbol::{AstSymbol, ExtractSymbol, SymbolContext, SymbolData, SymbolSpace};
use crate::parser::typedecl::{TypeExpression, TypePrimary};

impl ExtractSymbol for TypePrimary {
    fn extract(&self, data: &mut SymbolData, ctx: SymbolContext) {
        match self {
            // type IDs and holes declare no symbols
            Self::TypeIdentifier(_) | Self::TypeHole(_) => {}
            // type variables are bound to definition scope
            Self::TypeVariable(name) => {
                let symbol = name.as_ref().map(|name| AstSymbol::in_scope(SymbolSpace::Type, ctx.enclosing_definition, &name.0));
                data.declare_symbol(symbol);
            }
            // subexpressions pass through
            Self::TypeSubExpression(expr) => expr.extract(data, ctx),
        }
    }
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
    use std::collections::HashSet;

    use crate::parser::item::Fixity;
    use crate::parser::tagged::Tagged;
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
                (AstSymbol::from_scopes(SymbolSpace::Type, &["a"]), Tagged { value: Fixity::None, idx: 0, len: 2 }),
                (AstSymbol::from_scopes(SymbolSpace::Type, &["1", "b"]), Tagged { value: Fixity::None, idx: 11, len: 1 }),
            ].into_iter().collect(),
            HashSet::new(),
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
                (AstSymbol::from_scopes(SymbolSpace::Type, &["a"]), Tagged { value: Fixity::None, idx: 13, len: 2 }),
                (AstSymbol::from_scopes(SymbolSpace::Type, &["0", "b"]), Tagged { value: Fixity::None, idx: 5, len: 1 }),
                (AstSymbol::from_scopes(SymbolSpace::Type, &["1", "b"]), Tagged { value: Fixity::None, idx: 25, len: 1 }),
            ].into_iter().collect(),
            HashSet::new(),
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
                (AstSymbol::from_scopes(SymbolSpace::Type, &["a"]), Tagged { value: Fixity::None, idx: 13, len: 2 }),
                (AstSymbol::from_scopes(SymbolSpace::Type, &["0", "b"]), Tagged { value: Fixity::None, idx: 5, len: 1 }),
                (AstSymbol::from_scopes(SymbolSpace::Type, &["1", "b"]), Tagged { value: Fixity::None, idx: 22, len: 1 }),
            ].into_iter().collect(),
            HashSet::new(),
        );
        expr.extract(&mut data, ctx);
        assert_eq!(data, expected);
    }
}
