use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::multi::many1;
use nom::sequence::{delimited, pair};

use crate::parser::ParseResult;
use crate::parser::primary::name;
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::token::fixed::Keyword;
use crate::parser::token::identifier::{Identifier, Name, Operator};
use crate::parser::typedecl::TypeExpression;

/// A universal quantifier `for a b. c`.
#[derive(Clone, Debug, PartialEq)]
pub struct TypeLambda {
    /// The declared type parameters.
    pub params: Vec<Name>,
    /// The type body.
    pub body: Box<TypeExpression>,
}

impl TypeLambda {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        map(
            pair(
                delimited(
                    tag(TokenValue::from(Keyword::For)),
                    many1(name),
                    tag(TokenValue::from(Identifier::Operator(Operator(".".to_owned())))),
                ),
                TypeExpression::parse,
            ),
            |(params, body)| Self { params, body: Box::new(body) },
        )(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::fixity::{InfixApply, InfixPrimary};
    use crate::parser::scoped::ScopedIdentifier;
    use crate::parser::token::Token;
    use crate::parser::typedecl::TypePrimary;

    use super::*;

    #[test]
    fn parses() {
        let expected = TypeLambda {
            params: vec![
                Name("x".to_owned()),
                Name("y".to_owned()),
            ],
            body: Box::new(TypeExpression::InfixTypeApplication(InfixApply {
                func: Identifier::Operator(Operator("->".to_owned())),
                args: vec![
                    InfixPrimary::Primary(TypePrimary::TypeIdentifier(ScopedIdentifier::from(Identifier::Name(Name("x".to_owned()))))),
                    InfixPrimary::Primary(TypePrimary::TypeIdentifier(ScopedIdentifier::from(Identifier::Name(Name("y".to_owned()))))),
                ],
            })),
        };
        // for x y. x -> y
        let tokens = [
            Token::new(TokenValue::from(Keyword::For)),
            Token::new(TokenValue::from(Identifier::Name(Name("x".to_owned())))),
            Token::new(TokenValue::from(Identifier::Name(Name("y".to_owned())))),
            Token::new(TokenValue::from(Identifier::Operator(Operator(".".to_owned())))),
            Token::new(TokenValue::from(Identifier::Name(Name("x".to_owned())))),
            Token::new(TokenValue::from(Identifier::Operator(Operator("->".to_owned())))),
            Token::new(TokenValue::from(Identifier::Name(Name("y".to_owned())))),
        ];
        let result = TypeLambda::parse(TokenStream(&tokens));
        assert!(result.is_ok(), "Expected ok result, got {:?}", result);
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }
}
