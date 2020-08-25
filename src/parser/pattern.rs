use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{map, value};
use nom::sequence::delimited;

use crate::parser::fixity::BasicFixity;
use crate::parser::ParseResult;
use crate::parser::primary::{literal, Primary};
use crate::parser::scoped::ScopedIdentifier;
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::token::fixed::{Keyword, Separator};
use crate::parser::token::literal::Literal;

/// A primary pattern, used where patterns may be placed.
#[derive(Clone, Debug, PartialEq)]
pub enum PatternPrimary {
    /// A literal like `True`, `3`.
    Literal(Literal),
    /// The blank pattern `_`.
    Blank,
    /// An identifier like `a::b` or `Some`, which may introduce
    /// a binding or refer to a constructor.
    Identifier(ScopedIdentifier),
    /// A parenthesized pattern `( a )`.
    SubPattern(Box<Pattern>),
}

impl Primary for PatternPrimary {
    fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        alt((
            map(literal, Self::Literal),
            value(Self::Blank, tag(TokenValue::from(Keyword::Underscore))),
            map(ScopedIdentifier::parse, Self::Identifier),
            map(delimited(
                tag(TokenValue::from(Separator::LeftRound)),
                Pattern::parse,
                tag(TokenValue::from(Separator::RightRound)),
            ), |e| Self::SubPattern(Box::new(e)))
        ))(input)
    }
}

/// A pattern composes pattern primaries with prefix and infix
/// application.
#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    /// Pattern application
    Application(BasicFixity<PatternPrimary>),
}

impl Pattern {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        map(BasicFixity::parse, Self::Application)(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::fixity::{InfixApply, InfixPrimary, PrefixApply};
    use crate::parser::tagged::Tagged;
    use crate::parser::token::identifier::{Identifier, Name, Operator};
    use crate::parser::token::Token;

    use super::*;

    #[test]
    fn parses() {
        let expected = PatternPrimary::SubPattern(Box::new(
            Pattern::Application(BasicFixity::Infix(InfixApply {
                func: Tagged::new(Identifier::Operator(Operator("!".to_owned()))),
                args: vec![
                    InfixPrimary::Application(PrefixApply {
                        func: PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("Some".to_owned())))),
                        args: vec![
                            PatternPrimary::Blank,
                        ],
                    }),
                    InfixPrimary::Primary(PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned()))))),
                ],
            })
            )));
        let tokens = [
            Token::new(TokenValue::from(Separator::LeftRound)),
            Token::new(TokenValue::from(Identifier::Name(Name("Some".to_owned())))),
            Token::new(TokenValue::from(Keyword::Underscore)),
            Token::new(TokenValue::from(Identifier::Operator(Operator("!".to_owned())))),
            Token::new(TokenValue::from(Identifier::Name(Name("a".to_owned())))),
            Token::new(TokenValue::from(Separator::RightRound)),
        ];
        let result = PatternPrimary::parse(TokenStream(&tokens));
        assert!(result.is_ok(), "Expected ok result, got {:?}", result);
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }
}
