use crate::ast::symbol::{AstSymbol, ExtractSymbol, SymbolContext, SymbolData, SymbolSpace};
use crate::ast::symbol::fixity::InfixNamespace;
use crate::parser::pattern::{Pattern, PatternPrimary};

impl InfixNamespace for PatternPrimary {
    const NAMESPACE: SymbolSpace = SymbolSpace::Pattern;
}

impl ExtractSymbol for PatternPrimary {
    fn extract(&self, data: &mut SymbolData, ctx: SymbolContext) {
        match self {
            // literals and blanks declare no symbols
            Self::Literal(_) | Self::Blank => {}
            // identifiers declare unbound (at first) symbols
            Self::Identifier(id) => {
                // patterns are scoped identifiers or begin with an uppercase letter
                let is_pattern = !id.value.scopes.is_empty() || id.value.name.value()
                    .chars()
                    .next()
                    .expect("Expected nonempty name")
                    .is_uppercase();
                if is_pattern {
                    let symbol = AstSymbol::from_scopes(SymbolSpace::Pattern, &id.value.to_scopes());
                    data.declare_unbound_symbol(ctx.enclosing_scope.clone(), symbol);
                } else {
                    let symbol = id.as_ref().map(|id| AstSymbol::in_scope(SymbolSpace::Value, ctx.enclosing_scope, id.name.value()));
                    data.declare_symbol(symbol);
                }
            }
            // subpatterns pass through
            Self::SubPattern(pattern) => pattern.extract(data, ctx),
        }
    }
}

impl ExtractSymbol for Pattern {
    fn extract(&self, data: &mut SymbolData, ctx: SymbolContext) {
        let Self::Application(fix) = self;
        fix.extract(data, ctx);
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::primary::Primary;
    use crate::parser::tagged::Tagged;
    use crate::parser::token::{Token, TokenStream};

    use super::*;

    #[test]
    fn extracts_names() {
        let input = "(a, Some b c::d _ 3)";
        let input = Token::parse_sequence(input);
        let input = PatternPrimary::parse(TokenStream(&input)).unwrap().1;
        let mut result = SymbolData::new();
        let ctx = SymbolContext::new();
        let expected = SymbolData::from_parts(
            vec![
                (AstSymbol::from_scopes(SymbolSpace::Value, &["a"]), Tagged { value: (), idx: 1, len: 1 }),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["b"]), Tagged { value: (), idx: 9, len: 1 }),
            ].into_iter().collect(),
            vec![
                (AstSymbol::from_scopes(SymbolSpace::Value, &[]), AstSymbol::from_scopes(SymbolSpace::Pattern, &[","])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &[]), AstSymbol::from_scopes(SymbolSpace::Pattern, &["Some"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &[]), AstSymbol::from_scopes(SymbolSpace::Pattern, &["c", "d"])),
            ].into_iter().collect(),
        );
        input.extract(&mut result, ctx);
        assert_eq!(result, expected);
    }
}

