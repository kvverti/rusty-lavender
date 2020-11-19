use crate::ast::node::{AstPatternExpression, ExtractAstNode};
use crate::ast::node::fixity::InfixNamespace;
use crate::ast::symbol::{AstSymbol, SymbolContext, SymbolData, SymbolSpace};
use crate::parser::pattern::{Pattern, PatternPrimary};

impl<'a> ExtractAstNode<'a> for PatternPrimary {
    type Node = AstPatternExpression<'a>;

    fn construct_ast(self, data: &'a SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        match self {
            Self::Identifier(id) => {
                // patterns are scoped identifiers or begin with an uppercase letter
                let is_pattern = !id.value.scopes.is_empty() || id.value.name.value()
                    .chars()
                    .next()
                    .expect("Expected nonempty name")
                    .is_uppercase();
                if is_pattern {
                    let symbol = AstSymbol::from_scopes(SymbolSpace::Pattern, &id.value.to_scopes());
                    data.resolve_symbol(ctx.enclosing_scope, symbol)
                        .map(|(s, _)| AstPatternExpression::Symbol(s))
                        .unwrap_or_else(|| AstPatternExpression::Error(id.map(|_| "Cannot resolve pattern symbol")))
                } else {
                    let symbol = AstSymbol::in_scope(SymbolSpace::Value, ctx.enclosing_scope, id.value.name.value());
                    AstPatternExpression::Symbol(data.get_declared_symbol(symbol))
                }
            }
            Self::Literal(lit) => AstPatternExpression::Constant(lit),
            Self::Blank => AstPatternExpression::Blank,
            Self::SubPattern(pattern) => pattern.construct_ast(data, ctx)
        }
    }
}

impl InfixNamespace for PatternPrimary {
    const NAMESPACE: SymbolSpace = SymbolSpace::Pattern;
}

impl<'a> ExtractAstNode<'a> for Pattern {
    type Node = AstPatternExpression<'a>;

    fn construct_ast(self, data: &'a SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        let Self::Application(fixity) = self;
        fixity.construct_ast(data, ctx)
    }
}

#[cfg(test)]
mod tests {
    use nom::lib::std::collections::HashMap;

    use crate::ast::symbol::{ExtractSymbol, GLOBAL_SCOPE};
    use crate::parser::item::Fixity;
    use crate::parser::primary::Primary;
    use crate::parser::tagged::Tagged;
    use crate::parser::token::{Token, TokenStream};
    use crate::parser::token::literal::{IntLiteral, Literal};

    use super::*;

    #[test]
    fn constructs() {
        let input = "(Some x ~> list::Nil 3 ~> _)";
        let some = AstSymbol::from_scopes(SymbolSpace::Pattern, &["Some"]);
        let arrow = AstSymbol::from_scopes(SymbolSpace::Pattern, &["~>"]);
        let list_nil = AstSymbol::from_scopes(SymbolSpace::Pattern, &["list", "Nil"]);
        let x = AstSymbol::new(SymbolSpace::Value, "x");
        let mut data = SymbolData::from_parts(
            vec![
                (some.clone(), Tagged::new(Fixity::None)),
                (arrow.clone(), Tagged::new(Fixity::Right)),
                (list_nil.clone(), Tagged::new(Fixity::None)),
            ].into_iter().collect(),
            vec![
                (GLOBAL_SCOPE.clone(), some.clone()),
                (GLOBAL_SCOPE.clone(), arrow.clone()),
                (GLOBAL_SCOPE.clone(), list_nil.clone()),
            ],
        );
        let expected = AstPatternExpression::Application(
            Box::new(AstPatternExpression::Application(
                Box::new(AstPatternExpression::Symbol(&arrow)),
                Box::new(AstPatternExpression::Application(
                    Box::new(AstPatternExpression::Symbol(&some)),
                    Box::new(AstPatternExpression::Symbol(&x)),
                )),
            )),
            Box::new(AstPatternExpression::Application(
                Box::new(AstPatternExpression::Application(
                    Box::new(AstPatternExpression::Symbol(&arrow)),
                    Box::new(AstPatternExpression::Application(
                        Box::new(AstPatternExpression::Symbol(&list_nil)),
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
        let x = AstSymbol::new(SymbolSpace::Value, "x");
        let mut data = SymbolData::from_parts(
            HashMap::new(),
            vec![
                (GLOBAL_SCOPE.clone(), AstSymbol::new(SymbolSpace::Pattern, "Some"))
            ],
        );
        let expected = AstPatternExpression::Application(
            Box::new(AstPatternExpression::Error(Tagged {
                value: "Cannot resolve pattern symbol",
                idx: 1,
                len: 4,
            })),
            Box::new(AstPatternExpression::Symbol(&x)),
        );
        let input = Token::parse_sequence(input);
        let input = PatternPrimary::parse(TokenStream(&input)).unwrap().1;
        input.extract(&mut data, SymbolContext::new());
        let ast = input.construct_ast(&data, SymbolContext::new());
        data.assert_resolved();
        assert_eq!(ast, expected);
    }
}
