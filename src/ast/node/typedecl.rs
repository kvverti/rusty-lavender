use crate::ast::node::fixity::InfixNamespace;
use crate::ast::node::{AstTypeExpression, ExtractAstNode};
use crate::ast::symbol::{AstSymbol, SymbolContext, SymbolData, SymbolSpace, GLOBAL_SCOPE};
use crate::parser::typedecl::typelambda::TypeLambda;
use crate::parser::typedecl::{TypeExpression, TypePrimary};
use std::iter;

impl ExtractAstNode for TypeLambda {
    type Node = AstTypeExpression;

    fn construct_ast(self, data: &SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        match self {
            Self::Error { .. } => unreachable!(),
            Self::Value { params, body } => {
                let inner_scope = AstSymbol::in_scope(
                    SymbolSpace::Value,
                    ctx.enclosing_scope,
                    &ctx.implicit_scope.as_scopes().join("/"),
                );
                let params = params
                    .iter()
                    .map(|name| {
                        inner_scope
                            .as_scopes()
                            .iter()
                            .chain(iter::once(&name.value.0))
                    })
                    .map(|s| data.get(SymbolSpace::Type, s));
                let ctx = ctx
                    .with_enclosing_scope(&inner_scope)
                    .with_implicit_scope(&GLOBAL_SCOPE);
                let body_node = body.construct_ast(data, ctx);
                AstTypeExpression::Abstraction(params.collect(), Box::new(body_node))
            }
        }
    }
}

impl ExtractAstNode for TypePrimary {
    type Node = AstTypeExpression;

    fn construct_ast(self, data: &SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        match self {
            Self::TypeHole(_) => AstTypeExpression::Hole,
            Self::TypeIdentifier(id) => {
                let symbol = AstSymbol::from_scopes(SymbolSpace::Type, &id.value.to_scopes());
                let opt = data.resolve(ctx.enclosing_scope, symbol);
                opt.map(AstTypeExpression::Symbol).unwrap_or_else(|| {
                    AstTypeExpression::Error(id.map(|_| "Cannot resolve type symbol"))
                })
            }
            Self::TypeVariable(name) => {
                let symbol = ctx
                    .enclosing_definition
                    .as_scopes()
                    .iter()
                    .chain(iter::once(&name.value.0));
                AstTypeExpression::Symbol(data.get(SymbolSpace::Type, symbol))
            }
            Self::TypeSubExpression(expr) => expr.construct_ast(data, ctx),
        }
    }
}

impl InfixNamespace for TypePrimary {
    const NAMESPACE: SymbolSpace = SymbolSpace::Type;
}

impl ExtractAstNode for TypeExpression {
    type Node = AstTypeExpression;

    fn construct_ast(self, data: &SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        match self {
            Self::TypeLambda(lambda) => lambda.construct_ast(data, ctx),
            Self::TypeApplication(fixity) => fixity.construct_ast(data, ctx),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::symbol::{ExtractSymbol, LookupKey, GLOBAL_SCOPE};
    use crate::parser::item::Fixity;
    use crate::parser::tagged::Tagged;
    use crate::parser::token::{Token, TokenStream};

    use super::*;

    #[test]
    fn constructs() {
        let input = "'a -> (for b. b -> 'a)";
        let arrow = AstSymbol::new(SymbolSpace::Type, "->");
        let mut data = SymbolData::from_parts(
            vec![(arrow.clone(), Tagged::new(Fixity::Right))]
                .into_iter()
                .collect(),
            vec![
                (GLOBAL_SCOPE.clone(), arrow.clone()),
                (
                    AstSymbol::from_scopes(SymbolSpace::Value, &["1"]),
                    AstSymbol::from_scopes(SymbolSpace::Type, &["b"]),
                ),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["1"]), arrow),
            ],
        );
        let arrow = LookupKey::new(0);
        let a = LookupKey::new(1);
        let b = LookupKey::new(2);
        let expected = AstTypeExpression::Application(
            Box::new(AstTypeExpression::Application(
                Box::new(AstTypeExpression::Symbol(arrow)),
                Box::new(AstTypeExpression::Symbol(a)),
            )),
            Box::new(AstTypeExpression::Abstraction(
                vec![b],
                Box::new(AstTypeExpression::Application(
                    Box::new(AstTypeExpression::Application(
                        Box::new(AstTypeExpression::Symbol(arrow)),
                        Box::new(AstTypeExpression::Symbol(b)),
                    )),
                    Box::new(AstTypeExpression::Symbol(a)),
                )),
            )),
        );
        let input = Token::parse_sequence(input);
        let input = TypeExpression::parse(TokenStream(&input)).unwrap().1;
        input.extract(&mut data, SymbolContext::new());
        let ast = input.construct_ast(&data, SymbolContext::new());
        data.assert_resolved();
        assert_eq!(ast, expected);
    }

    #[test]
    fn unresolved() {
        let input = "for a b. c";
        let a = LookupKey::new(0);
        let b = LookupKey::new(1);
        let mut data = SymbolData::from_parts(
            Vec::new(),
            vec![(
                AstSymbol::from_scopes(SymbolSpace::Value, &[""]),
                AstSymbol::from_scopes(SymbolSpace::Type, &["c"]),
            )],
        );
        let expected = AstTypeExpression::Abstraction(
            vec![a, b],
            Box::new(AstTypeExpression::Error(Tagged {
                value: "Cannot resolve type symbol",
                idx: 9,
                len: 1,
            })),
        );
        let input = Token::parse_sequence(input);
        let input = TypeExpression::parse(TokenStream(&input)).unwrap().1;
        input.extract(&mut data, SymbolContext::new());
        let ast = input.construct_ast(&data, SymbolContext::new());
        data.assert_resolved();
        assert_eq!(ast, expected);
    }
}
