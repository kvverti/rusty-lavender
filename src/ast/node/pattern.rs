use crate::ast::node::fixity::InfixNamespace;
use crate::ast::node::{AstPatternExpression, ExtractAstNode};
use crate::ast::symbol::{AstSymbol, SymbolContext, SymbolData, SymbolSpace};
use crate::parser::pattern::{Pattern, PatternPrimary};
use std::iter;

impl ExtractAstNode for PatternPrimary {
    type Node = AstPatternExpression;

    fn construct_ast(self, data: &SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        match self {
            Self::Identifier(id) => {
                // patterns are scoped identifiers or begin with an uppercase letter
                let is_pattern = !id.value.scopes.is_empty()
                    || id
                        .value
                        .name
                        .value()
                        .chars()
                        .next()
                        .expect("Expected nonempty name")
                        .is_uppercase();
                if is_pattern {
                    let symbol =
                        AstSymbol::from_scopes(SymbolSpace::Pattern, &id.value.to_scopes());
                    data.resolve(ctx.enclosing_scope, symbol)
                        .map(AstPatternExpression::Symbol)
                        .unwrap_or_else(|| {
                            AstPatternExpression::Error(id.map(|_| "Cannot resolve pattern symbol"))
                        })
                } else {
                    let symbol = ctx
                        .enclosing_scope
                        .as_scopes()
                        .iter()
                        .map(AsRef::as_ref)
                        .chain(iter::once(id.value.name.value()));
                    AstPatternExpression::Symbol(data.get(SymbolSpace::Value, symbol))
                }
            }
            Self::Literal(lit) => AstPatternExpression::Constant(lit),
            Self::Blank => AstPatternExpression::Blank,
            Self::SubPattern(pattern) => pattern.construct_ast(data, ctx),
        }
    }
}

impl InfixNamespace for PatternPrimary {
    const NAMESPACE: SymbolSpace = SymbolSpace::Pattern;
}

impl ExtractAstNode for Pattern {
    type Node = AstPatternExpression;

    fn construct_ast(self, data: &SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        let Self::Application(fixity) = self;
        fixity.construct_ast(data, ctx)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::symbol::{ExtractSymbol, LookupKey, GLOBAL_SCOPE};
    use crate::parser::item::Fixity;
    use crate::parser::primary::Primary;
    use crate::parser::tagged::Tagged;
    use crate::parser::token::literal::{IntLiteral, Literal};
    use crate::parser::token::{Token, TokenStream};

    use super::*;

    #[test]
    fn constructs() {
        let input = "(Some x ~> list::Nil 3 ~> _)";
        // symbol table
        // p/0 = Some
        // p/1 = ~>
        // p/2 = list::Nil
        // v/0 = x
        let some = AstSymbol::from_scopes(SymbolSpace::Pattern, &["Some"]);
        let arrow = AstSymbol::from_scopes(SymbolSpace::Pattern, &["~>"]);
        let list_nil = AstSymbol::from_scopes(SymbolSpace::Pattern, &["list", "Nil"]);
        // let x = AstSymbol::new(SymbolSpace::Value, "x");
        let mut data = SymbolData::from_parts(
            vec![
                (some.clone(), Tagged::new(Fixity::None)),
                (arrow.clone(), Tagged::new(Fixity::Right)),
                (list_nil.clone(), Tagged::new(Fixity::None)),
            ]
            .into_iter()
            .collect(),
            vec![
                (GLOBAL_SCOPE.clone(), some),
                (GLOBAL_SCOPE.clone(), arrow),
                (GLOBAL_SCOPE.clone(), list_nil),
            ],
        );
        let some = LookupKey::new(0);
        let arrow = LookupKey::new(1);
        let list_nil = LookupKey::new(2);
        let x = LookupKey::new(0);
        let expected = AstPatternExpression::Application(
            Box::new(AstPatternExpression::Application(
                Box::new(AstPatternExpression::Symbol(arrow)),
                Box::new(AstPatternExpression::Application(
                    Box::new(AstPatternExpression::Symbol(some)),
                    Box::new(AstPatternExpression::Symbol(x)),
                )),
            )),
            Box::new(AstPatternExpression::Application(
                Box::new(AstPatternExpression::Application(
                    Box::new(AstPatternExpression::Symbol(arrow)),
                    Box::new(AstPatternExpression::Application(
                        Box::new(AstPatternExpression::Symbol(list_nil)),
                        Box::new(AstPatternExpression::Constant(Literal::Int(IntLiteral(3)))),
                    )),
                )),
                Box::new(AstPatternExpression::Blank),
            )),
        );
        let input = Token::parse_sequence(input);
        let input = PatternPrimary::parse(TokenStream(&input)).unwrap().1;
        input.extract(&mut data, SymbolContext::new());
        let ast = input.construct_ast(&data, SymbolContext::new());
        data.assert_resolved();
        assert_eq!(ast, expected);
    }

    #[test]
    fn unresolved() {
        let input = "(Some x)";
        let x = LookupKey::new(0);
        let mut data = SymbolData::from_parts(
            Vec::new(),
            vec![(
                GLOBAL_SCOPE.clone(),
                AstSymbol::new(SymbolSpace::Pattern, "Some"),
            )],
        );
        let expected = AstPatternExpression::Application(
            Box::new(AstPatternExpression::Error(Tagged {
                value: "Cannot resolve pattern symbol",
                idx: 1,
                len: 4,
            })),
            Box::new(AstPatternExpression::Symbol(x)),
        );
        let input = Token::parse_sequence(input);
        let input = PatternPrimary::parse(TokenStream(&input)).unwrap().1;
        input.extract(&mut data, SymbolContext::new());
        let ast = input.construct_ast(&data, SymbolContext::new());
        data.assert_resolved();
        assert_eq!(ast, expected);
    }
}
