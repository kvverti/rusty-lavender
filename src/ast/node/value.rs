use crate::ast::node::fixity::InfixNamespace;
use crate::ast::node::{AstValueExpression, ExtractAstNode};
use crate::ast::symbol::{AstSymbol, SymbolContext, SymbolData, SymbolSpace, GLOBAL_SCOPE};
use crate::parser::value::lambda::LambdaExpression;
use crate::parser::value::{ValueExpression, ValuePrimary};

impl<'a> ExtractAstNode<'a> for LambdaExpression {
    type Node = AstValueExpression<'a>;

    fn construct_ast(self, data: &'a SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        match self {
            Self::Error { .. } => unreachable!(),
            Self::Value { params, body } => {
                let inner_scope = AstSymbol::in_scope(
                    SymbolSpace::Value,
                    ctx.enclosing_scope,
                    &ctx.implicit_scope.as_scopes().join("/"),
                );
                let inner_ctx = ctx
                    .with_enclosing_scope(&inner_scope)
                    .with_implicit_scope(&GLOBAL_SCOPE);
                let params = params
                    .into_iter()
                    .map(|pattern| pattern.construct_ast(data, inner_ctx));
                let body_node = body.construct_ast(data, inner_ctx);
                AstValueExpression::Abstraction(params.collect(), Box::new(body_node))
            }
        }
    }
}

impl<'a> ExtractAstNode<'a> for ValuePrimary {
    type Node = AstValueExpression<'a>;

    fn construct_ast(self, data: &'a SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        match self {
            Self::Literal(lit) => AstValueExpression::Constant(lit),
            Self::Identifier(id) => {
                let symbol = AstSymbol::from_scopes(SymbolSpace::Value, &id.value.to_scopes());
                data.resolve_symbol(ctx.enclosing_scope, symbol)
                    .map(|(symbol, _)| AstValueExpression::Symbol(symbol))
                    .unwrap_or_else(|| {
                        AstValueExpression::Error(id.map(|_| "Cannot resolve value"))
                    })
            }
            Self::SubExpression(expr) => expr.construct_ast(data, ctx),
        }
    }
}

impl InfixNamespace for ValuePrimary {
    const NAMESPACE: SymbolSpace = SymbolSpace::Value;
}

impl<'a> ExtractAstNode<'a> for ValueExpression {
    type Node = AstValueExpression<'a>;

    fn construct_ast(self, data: &'a SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        match self {
            Self::Lambda(lambda) => lambda.construct_ast(data, ctx),
            Self::Application(fixity) => fixity.construct_ast(data, ctx),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::node::{AstPatternExpression, AstValueExpression, ExtractAstNode};
    use crate::ast::symbol::{
        AstSymbol, ExtractSymbol, SymbolContext, SymbolData, SymbolSpace, GLOBAL_SCOPE,
    };
    use crate::parser::item::Fixity;
    use crate::parser::tagged::Tagged;
    use crate::parser::token::literal::{IntLiteral, Literal};
    use crate::parser::token::{Token, TokenStream};
    use crate::parser::value::ValueExpression;

    #[test]
    fn constructs() {
        let input = "a @ (for (Some b, c). b + c + (for _. a))";
        let a = AstSymbol::from_scopes(SymbolSpace::Value, &["a"]);
        let at = AstSymbol::from_scopes(SymbolSpace::Value, &["@"]);
        let some = AstSymbol::from_scopes(SymbolSpace::Pattern, &["Some"]);
        let comma = AstSymbol::from_scopes(SymbolSpace::Pattern, &[","]);
        let plus = AstSymbol::from_scopes(SymbolSpace::Value, &["+"]);
        let b = AstSymbol::from_scopes(SymbolSpace::Value, &["1", "b"]);
        let c = AstSymbol::from_scopes(SymbolSpace::Value, &["1", "c"]);
        let mut data = SymbolData::from_parts(
            vec![
                (a.clone(), Tagged::new(Fixity::None)),
                (at.clone(), Tagged::new(Fixity::None)),
                (plus.clone(), Tagged::new(Fixity::Left)),
                (some.clone(), Tagged::new(Fixity::None)),
                (comma.clone(), Tagged::new(Fixity::None)),
            ]
            .into_iter()
            .collect(),
            vec![
                (GLOBAL_SCOPE.clone(), a.clone()),
                (GLOBAL_SCOPE.clone(), at.clone()),
                (
                    AstSymbol::from_scopes(SymbolSpace::Value, &["1"]),
                    some.clone(),
                ),
                (
                    AstSymbol::from_scopes(SymbolSpace::Value, &["1"]),
                    comma.clone(),
                ),
                (
                    AstSymbol::from_scopes(SymbolSpace::Value, &["1"]),
                    AstSymbol::from_scopes(SymbolSpace::Value, &["b"]),
                ),
                (
                    AstSymbol::from_scopes(SymbolSpace::Value, &["1"]),
                    AstSymbol::from_scopes(SymbolSpace::Value, &["c"]),
                ),
                (
                    AstSymbol::from_scopes(SymbolSpace::Value, &["1"]),
                    plus.clone(),
                ),
                (
                    AstSymbol::from_scopes(SymbolSpace::Value, &["1", "2"]),
                    a.clone(),
                ),
            ],
        );
        let expected = AstValueExpression::Application(
            Box::new(AstValueExpression::Application(
                Box::new(AstValueExpression::Symbol(&at)),
                Box::new(AstValueExpression::Symbol(&a)),
            )),
            Box::new(AstValueExpression::Abstraction(
                vec![AstPatternExpression::Application(
                    Box::new(AstPatternExpression::Application(
                        Box::new(AstPatternExpression::Symbol(&comma)),
                        Box::new(AstPatternExpression::Application(
                            Box::new(AstPatternExpression::Symbol(&some)),
                            Box::new(AstPatternExpression::Symbol(&b)),
                        )),
                    )),
                    Box::new(AstPatternExpression::Symbol(&c)),
                )],
                Box::new(AstValueExpression::Application(
                    Box::new(AstValueExpression::Application(
                        Box::new(AstValueExpression::Symbol(&plus)),
                        Box::new(AstValueExpression::Application(
                            Box::new(AstValueExpression::Application(
                                Box::new(AstValueExpression::Symbol(&plus)),
                                Box::new(AstValueExpression::Symbol(&b)),
                            )),
                            Box::new(AstValueExpression::Symbol(&c)),
                        )),
                    )),
                    Box::new(AstValueExpression::Abstraction(
                        vec![AstPatternExpression::Blank],
                        Box::new(AstValueExpression::Symbol(&a)),
                    )),
                )),
            )),
        );
        let input = Token::parse_sequence(input);
        let input = ValueExpression::parse(TokenStream(&input)).unwrap().1;
        input.extract(&mut data, SymbolContext::new());
        let ast = input.construct_ast(&data, SymbolContext::new());
        data.assert_resolved();
        assert_eq!(ast, expected);
    }

    #[test]
    fn unresolved() {
        let input = "a @ 3";
        let mut data = SymbolData::from_parts(
            Vec::new(),
            vec![
                (
                    GLOBAL_SCOPE.clone(),
                    AstSymbol::from_scopes(SymbolSpace::Value, &["a"]),
                ),
                (
                    GLOBAL_SCOPE.clone(),
                    AstSymbol::from_scopes(SymbolSpace::Value, &["@"]),
                ),
            ],
        );
        let expected = AstValueExpression::Application(
            Box::new(AstValueExpression::Application(
                Box::new(AstValueExpression::Error(Tagged {
                    value: "Cannot resolve symbol",
                    idx: 2,
                    len: 1,
                })),
                Box::new(AstValueExpression::Error(Tagged {
                    value: "Cannot resolve value",
                    idx: 0,
                    len: 1,
                })),
            )),
            Box::new(AstValueExpression::Constant(Literal::Int(IntLiteral(3)))),
        );
        let input = Token::parse_sequence(input);
        let input = ValueExpression::parse(TokenStream(&input)).unwrap().1;
        input.extract(&mut data, SymbolContext::new());
        let ast = input.construct_ast(&data, SymbolContext::new());
        data.assert_resolved();
        assert_eq!(ast, expected);
    }
}
