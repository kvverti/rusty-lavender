use crate::ast::node::{AstTypeExpression, ExtractAstNode};
use crate::ast::node::fixity::InfixNamespace;
use crate::ast::symbol::{AstSymbol, GLOBAL_SCOPE, SymbolContext, SymbolData, SymbolSpace};
use crate::parser::typedecl::{TypeExpression, TypePrimary};
use crate::parser::typedecl::typelambda::TypeLambda;

impl<'a> ExtractAstNode<'a> for TypeLambda {
    type Node = AstTypeExpression<'a>;

    fn construct_ast(self, data: &'a SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        match self {
            Self::Error { .. } => unreachable!(),
            Self::Value { params, body } => {
                let inner_scope = AstSymbol::in_scope(
                    SymbolSpace::Value,
                    ctx.enclosing_scope,
                    &ctx.implicit_scope.as_scopes().join("/"));
                let params = params.into_iter()
                    .map(|name| AstSymbol::in_scope(SymbolSpace::Type, &inner_scope, &name.value.0))
                    .map(|s| data.get_declared_symbol(s));
                let ctx = ctx.with_enclosing_scope(&inner_scope)
                    .with_implicit_scope(&GLOBAL_SCOPE);
                let body_node = body.construct_ast(data, ctx);
                AstTypeExpression::Abstraction(params.collect(), Box::new(body_node))
            }
        }
    }
}

impl<'a> ExtractAstNode<'a> for TypePrimary {
    type Node = AstTypeExpression<'a>;

    fn construct_ast(self, data: &'a SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        match self {
            Self::TypeHole(_) => AstTypeExpression::Hole,
            Self::TypeIdentifier(id) => {
                let symbol = AstSymbol::from_scopes(SymbolSpace::Type, &id.value.to_scopes());
                let opt = data.resolve_symbol(ctx.enclosing_scope, symbol);
                opt.map(|(symbol, _)| AstTypeExpression::Symbol(symbol))
                    .unwrap_or_else(|| AstTypeExpression::Error(id.map(|_| "Cannot resolve type symbol")))
            }
            Self::TypeVariable(name) => {
                let symbol = AstSymbol::in_scope(SymbolSpace::Type, ctx.enclosing_definition, &name.value.0);
                AstTypeExpression::Symbol(data.get_declared_symbol(symbol))
            }
            Self::TypeSubExpression(expr) => expr.construct_ast(data, ctx),
        }
    }
}

impl InfixNamespace for TypePrimary {
    const NAMESPACE: SymbolSpace = SymbolSpace::Type;
}

impl<'a> ExtractAstNode<'a> for TypeExpression {
    type Node = AstTypeExpression<'a>;

    fn construct_ast(self, data: &'a SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        match self {
            Self::TypeLambda(lambda) => lambda.construct_ast(data, ctx),
            Self::TypeApplication(fixity) => fixity.construct_ast(data, ctx),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::ast::symbol::{ExtractSymbol, GLOBAL_SCOPE};
    use crate::parser::item::Fixity;
    use crate::parser::tagged::Tagged;
    use crate::parser::token::{Token, TokenStream};

    use super::*;

    #[test]
    fn constructs() {
        let input = "'a -> (for b. b -> 'a)";
        let arrow = AstSymbol::new(SymbolSpace::Type, "->");
        let a = AstSymbol::new(SymbolSpace::Type, "a");
        let b = AstSymbol::from_scopes(SymbolSpace::Type, &["1", "b"]);
        let mut data = SymbolData::from_parts(
            vec![
                (arrow.clone(), Tagged::new(Fixity::Right)),
            ].into_iter().collect(),
            vec![
                (GLOBAL_SCOPE.clone(), arrow.clone()),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["1"]), AstSymbol::from_scopes(SymbolSpace::Type, &["b"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["1"]), arrow.clone())
            ],
        );
        let expected = AstTypeExpression::Application(
            Box::new(AstTypeExpression::Application(
                Box::new(AstTypeExpression::Symbol(&arrow)),
                Box::new(AstTypeExpression::Symbol(&a)),
            )),
            Box::new(AstTypeExpression::Abstraction(
                vec![&b],
                Box::new(AstTypeExpression::Application(
                    Box::new(AstTypeExpression::Application(
                        Box::new(AstTypeExpression::Symbol(&arrow)),
                        Box::new(AstTypeExpression::Symbol(&b)),
                    )),
                    Box::new(AstTypeExpression::Symbol(&a)),
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
        let a = AstSymbol::from_scopes(SymbolSpace::Type, &["", "a"]);
        let b = AstSymbol::from_scopes(SymbolSpace::Type, &["", "b"]);
        let mut data = SymbolData::from_parts(
            HashMap::new(),
            vec![
                (AstSymbol::from_scopes(SymbolSpace::Value, &[""]), AstSymbol::from_scopes(SymbolSpace::Type, &["c"])),
            ],
        );
        let expected = AstTypeExpression::Abstraction(
            vec![&a, &b],
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
