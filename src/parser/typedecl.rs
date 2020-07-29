use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{map, value};
use nom::sequence::{delimited, preceded};

use crate::parser::fixity::{InfixApply, PrefixApply};
use crate::parser::ParseResult;
use crate::parser::primary::{name, Primary};
use crate::parser::scoped::ScopedIdentifier;
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::token::fixed::{Keyword, Separator};
use crate::parser::token::identifier::Name;
use crate::parser::typedecl::typelambda::TypeLambda;

/// Type lambda expression.
pub mod typelambda;

/// A type expression, used wherever a type may be placed.
#[derive(Clone, Debug, PartialEq)]
pub enum TypePrimary {
    /// A type name `A`.
    TypeIdentifier(ScopedIdentifier),
    /// A type variable name `'a`.
    TypeVariable(Name),
    /// A type hole `_` (triggers explicit type inference).
    TypeHole,
    /// A parenthesized type expression `( a )`.
    TypeSubExpression(Box<TypeExpression>),
}

impl Primary for TypePrimary {
    fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        alt((
            map(ScopedIdentifier::parse, Self::TypeIdentifier),
            map(
                preceded(tag(TokenValue::from(Separator::Check)), name),
                Self::TypeVariable,
            ),
            value(Self::TypeHole, tag(TokenValue::from(Keyword::Underscore))),
            map(
                delimited(
                    tag(TokenValue::from(Separator::LeftRound)),
                    TypeExpression::parse,
                    tag(TokenValue::from(Separator::RightRound)),
                ),
                |e| Self::TypeSubExpression(Box::new(e)),
            )
        ))(input)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeExpression {
    /// Simple type expressions `A`.
    TypePrimary(TypePrimary),
    /// Prefix type applications `A B ...`.
    TypeApplication(PrefixApply<TypePrimary>),
    /// Infix type applications `A @ B @ ...`.
    InfixTypeApplication(InfixApply<TypePrimary>),
    /// A universal quantifier, or type lambda expression.
    TypeLambda(TypeLambda),
}

impl TypeExpression {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        alt((
            map(TypeLambda::parse, Self::TypeLambda),
            map(InfixApply::parse, Self::InfixTypeApplication),
            map(PrefixApply::parse, Self::TypeApplication),
            map(TypePrimary::parse, Self::TypePrimary),
        ))(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::fixity::InfixPrimary;
    use crate::parser::token::identifier::{Identifier, Operator};
    use crate::parser::token::Token;

    use super::*;

    #[test]
    fn parses() {
        let expected = TypeExpression::TypeApplication(PrefixApply {
            func: TypePrimary::TypeIdentifier(ScopedIdentifier::from(Identifier::Name(Name("Type".to_owned())))),
            args: vec![
                TypePrimary::TypeVariable(Name("a".to_owned())),
                TypePrimary::TypeSubExpression(Box::new(
                    TypeExpression::InfixTypeApplication(InfixApply {
                        func: Identifier::Operator(Operator("->".to_owned())),
                        args: vec![
                            InfixPrimary::Primary(TypePrimary::TypeVariable(Name("a".to_owned()))),
                            InfixPrimary::Primary(TypePrimary::TypeHole),
                        ],
                    })
                )),
            ],
        });
        let input = [
            Token::new(TokenValue::from(Identifier::Name(Name("Type".to_string())))),
            Token::new(TokenValue::from(Separator::Check)),
            Token::new(TokenValue::from(Identifier::Name(Name("a".to_owned())))),
            Token::new(TokenValue::from(Separator::LeftRound)),
            Token::new(TokenValue::from(Separator::Check)),
            Token::new(TokenValue::from(Identifier::Name(Name("a".to_owned())))),
            Token::new(TokenValue::from(Identifier::Operator(Operator("->".to_string())))),
            Token::new(TokenValue::from(Keyword::Underscore)),
            Token::new(TokenValue::from(Separator::RightRound)),
        ];
        let result = TypeExpression::parse(TokenStream(&input));
        assert!(result.is_ok(), format!("Expected ok parse: {:?}", result));
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }
}
