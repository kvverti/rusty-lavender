use nom::bytes::complete::tag;
use nom::multi::many1;

use crate::ast::symbol::{
    AstSymbol, ExtractSymbol, SymbolContext, SymbolData, SymbolSpace, GLOBAL_SCOPE,
};
use crate::parser::primary::name;
use crate::parser::tagged::{tagged, Tagged};
use crate::parser::token::fixed::Keyword;
use crate::parser::token::identifier::{Identifier, Name, Operator};
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::typedecl::TypeExpression;
use crate::parser::{until_next_sync_point, ParseResult};

/// A universal quantifier `for a b. c`.
#[derive(Clone, Debug, PartialEq)]
pub enum TypeLambda {
    Value {
        /// The declared type parameters.
        params: Vec<Tagged<Name>>,
        /// The type body.
        body: Box<TypeExpression>,
    },
    /// An error that occurred when parsing a type lambda expression.
    Error {
        /// The error message and source columns until the next sequence point.
        context: Tagged<&'static str>,
    },
}

/// Matches a parser, or returns an error lambda node if there is a parser error.
macro_rules! next {
    ($ctx:literal, $x:expr, $input:expr) => {
        match $x($input) {
            // parse success
            Ok(v) => v,
            // parse error
            Err(nom::Err::Error(_)) => {
                let (input, context) = until_next_sync_point($ctx, $input);
                return Ok((input, Self::Error { context }));
            }
            // parse failure
            Err(e) => return Err(e),
        }
    };
}

impl TypeLambda {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        let (input, _) = tag(TokenValue::from(Keyword::For))(input)?;
        let (input, params) = next!("Expected type parameters", many1(tagged(name)), input);
        let (input, _) = next!(
            "Expected '.'",
            tag(TokenValue::from(Identifier::Operator(Operator(
                ".".to_owned()
            )))),
            input
        );
        let (input, body) = next!("Expected type", TypeExpression::parse, input);
        Ok((
            input,
            Self::Value {
                params,
                body: Box::new(body),
            },
        ))
    }
}

impl ExtractSymbol for TypeLambda {
    /// Extract the lambda names and anything in the body
    fn extract(&self, data: &mut SymbolData, ctx: SymbolContext) {
        if let Self::Value { params, body } = self {
            let inner_scope = AstSymbol::in_scope(
                SymbolSpace::Value,
                ctx.enclosing_scope,
                &ctx.implicit_scope.as_scopes().join("/"),
            );
            for name in params {
                let symbol = name
                    .as_ref()
                    .map(|name| AstSymbol::in_scope(SymbolSpace::Type, &inner_scope, &name.0));
                data.declare(symbol);
            }
            let ctx = ctx
                .with_enclosing_scope(&inner_scope)
                .with_implicit_scope(&GLOBAL_SCOPE);
            body.extract(data, ctx);
        } else {
            unreachable!();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::fixity::{BasicFixity, InfixApply, InfixPrimary};
    use crate::parser::scoped::ScopedIdentifier;
    use crate::parser::tagged::Tagged;
    use crate::parser::token::Token;
    use crate::parser::typedecl::TypePrimary;

    use super::*;

    #[test]
    fn parses() {
        let expected = TypeLambda::Value {
            params: vec![
                Tagged::new(Name("x".to_owned())),
                Tagged::new(Name("y".to_owned())),
            ],
            body: Box::new(TypeExpression::TypeApplication(BasicFixity::Infix(
                InfixApply {
                    func: Tagged::new(Identifier::Operator(Operator("->".to_owned()))),
                    args: vec![
                        InfixPrimary::Primary(TypePrimary::TypeIdentifier(Tagged::new(
                            ScopedIdentifier::from(Identifier::Name(Name("x".to_owned()))),
                        ))),
                        InfixPrimary::Primary(TypePrimary::TypeIdentifier(Tagged::new(
                            ScopedIdentifier::from(Identifier::Name(Name("y".to_owned()))),
                        ))),
                    ],
                },
            ))),
        };
        // for x y. x -> y
        let tokens = [
            Token::new(TokenValue::from(Keyword::For)),
            Token::new(TokenValue::from(Identifier::Name(Name("x".to_owned())))),
            Token::new(TokenValue::from(Identifier::Name(Name("y".to_owned())))),
            Token::new(TokenValue::from(Identifier::Operator(Operator(
                ".".to_owned(),
            )))),
            Token::new(TokenValue::from(Identifier::Name(Name("x".to_owned())))),
            Token::new(TokenValue::from(Identifier::Operator(Operator(
                "->".to_owned(),
            )))),
            Token::new(TokenValue::from(Identifier::Name(Name("y".to_owned())))),
        ];
        let result = TypeLambda::parse(TokenStream(&tokens));
        assert!(result.is_ok(), "Expected ok result, got {:?}", result);
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }
}
