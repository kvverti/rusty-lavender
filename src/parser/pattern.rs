use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{map, value};
use nom::sequence::delimited;

use crate::ast::Extract;
use crate::ast::symbol::AstSymbol;
use crate::parser::fixity::BasicFixity;
use crate::parser::ParseResult;
use crate::parser::primary::{literal, Primary};
use crate::parser::scoped::ScopedIdentifier;
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::token::fixed::{Keyword, Separator};
use crate::parser::token::identifier::Identifier;
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

impl Extract<AstSymbol> for PatternPrimary {
    fn extract(&self) -> Vec<AstSymbol> {
        match self {
            Self::Identifier(ScopedIdentifier {
                                 name: Identifier::Name(name),
                                 scopes
                             }) if scopes.is_empty() => vec![AstSymbol::new(&name.0)],
            Self::SubPattern(pattern) => pattern.extract(),
            _ => vec![]
        }
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

impl Extract<AstSymbol> for Pattern {
    fn extract(&self) -> Vec<AstSymbol> {
        let Self::Application(apply) = self;
        apply.extract()
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Extracted;
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

    #[test]
    fn extracts_names() {
        // extract simple names
        // extract prefix arguments
        // do not extract scoped IDs
        // do not extract symbolic IDs
        let input = "(a, Some b, (c), nspace::id, None, ($))";
        let expected = ["a", "b", "c", "None"];
        let input = Token::parse_sequence(input);
        let input = Pattern::parse(TokenStream(&input)).unwrap().1;
        let output = AstSymbol::extract_from(input).into_iter().map(|s| s.to_string()).collect::<Vec<_>>();
        assert_eq!(output.as_slice(), &expected);
    }
}
