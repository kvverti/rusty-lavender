use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::multi::many0;
use nom::sequence::{pair, terminated};

use crate::parser::fixity::prefix_operator;
use crate::parser::ParseResult;
use crate::parser::primary::name;
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::token::fixed::Separator;
use crate::parser::token::identifier::{Identifier, Name};

/// A scoped identifier `a::b::c`
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ScopedIdentifier {
    pub name: Identifier,
    pub scopes: Vec<Name>,
}

impl ScopedIdentifier {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        map(
            pair(
                many0(terminated(name, tag(TokenValue::from(Separator::DoubleColon)))),
                prefix_operator,
            ),
            |(scopes, name)| ScopedIdentifier { name, scopes },
        )(input)
    }
}

impl From<Identifier> for ScopedIdentifier {
    fn from(v: Identifier) -> Self {
        Self { name: v, scopes: vec![] }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::token::Token;

    use super::*;

    #[test]
    fn qualified() {
        let expected = ScopedIdentifier {
            name: Identifier::Name(Name("c".to_owned())),
            scopes: vec![
                Name("a".to_owned()),
                Name("b".to_owned()),
            ],
        };
        let input = [
            Token::new(TokenValue::from(Identifier::Name(Name("a".to_owned())))),
            Token::new(TokenValue::from(Separator::DoubleColon)),
            Token::new(TokenValue::from(Identifier::Name(Name("b".to_owned())))),
            Token::new(TokenValue::from(Separator::DoubleColon)),
            Token::new(TokenValue::from(Identifier::Name(Name("c".to_owned())))),
        ];
        let result = ScopedIdentifier::parse(TokenStream(&input));
        assert!(result.is_ok(), "Expected ok result, got {:?}", result);
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn unqualified() {
        let expected = ScopedIdentifier {
            name: Identifier::Name(Name("c".to_owned())),
            scopes: vec![],
        };
        let input = [
            Token::new(TokenValue::from(Identifier::Name(Name("c".to_owned())))),
        ];
        let result = ScopedIdentifier::parse(TokenStream(&input));
        assert!(result.is_ok(), "Expected ok result, got {:?}", result);
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn tailed() {
        let input = [
            Token::new(TokenValue::from(Identifier::Name(Name("a".to_owned())))),
            Token::new(TokenValue::from(Separator::DoubleColon)),
            Token::new(TokenValue::from(Identifier::Name(Name("b".to_owned())))),
            Token::new(TokenValue::from(Separator::DoubleColon)),
        ];
        let result = ScopedIdentifier::parse(TokenStream(&input));
        assert!(result.is_err(), "Expected err result, got {:?}", result);
    }
}