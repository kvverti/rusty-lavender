use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::sequence::delimited;

use crate::parser::fixity::BasicFixity;
use crate::parser::ParseResult;
use crate::parser::primary::{literal, Primary};
use crate::parser::scoped::ScopedIdentifier;
use crate::parser::tagged::{Tagged, tagged};
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::token::fixed::Separator;
use crate::parser::token::literal::Literal;
use crate::parser::value::lambda::LambdaExpression;

/// Lambda expression parsers.
pub mod lambda;

/// A fundamental, "atomic" value expression.
#[derive(Clone, Debug, PartialEq)]
pub enum ValuePrimary {
    /// A literal token `a` (such a `True` or `3`)
    Literal(Literal),
    /// A name `a` (such as `map` or `(.)`)
    Identifier(Tagged<ScopedIdentifier>),
    /// A parenthesized expression `( a )`
    SubExpression(Box<ValueExpression>),
}

impl ValuePrimary {
    /// Parses a parenthesized subexpression.
    fn parse_subexpr(input: TokenStream) -> ParseResult<TokenStream, ValueExpression> {
        delimited(
            tag(TokenValue::from(Separator::LeftRound)),
            ValueExpression::parse,
            tag(TokenValue::from(Separator::RightRound)),
        )(input)
    }
}

impl Primary for ValuePrimary {
    fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        alt((
            map(literal, Self::Literal),
            map(tagged(ScopedIdentifier::parse), Self::Identifier),
            map(Self::parse_subexpr, |e| Self::SubExpression(Box::new(e))),
        ))(input)
    }
}

/// The types of Lavender expressions.
#[derive(Clone, Debug, PartialEq)]
pub enum ValueExpression {
    /// Primary, and prefix and infix function application.
    Application(BasicFixity<ValuePrimary>),
    /// Lambda expression `lam a b. c`
    Lambda(LambdaExpression),
}

impl ValueExpression {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        alt((
            map(LambdaExpression::parse, Self::Lambda),
            map(BasicFixity::parse, Self::Application),
        ))(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::fixity::{InfixApply, InfixPrimary, PrefixApply};
    use crate::parser::pattern::PatternPrimary;
    use crate::parser::tagged::Tagged;
    use crate::parser::token::{Token, TokenValue};
    use crate::parser::token::fixed::Separator;
    use crate::parser::token::identifier::{Identifier, Name, Operator};
    use crate::parser::token::literal::{IntLiteral, Literal};

    use super::*;

    #[test]
    fn parses() {
        let expected = ValueExpression::Application(BasicFixity::Infix(InfixApply {
            func: Tagged::new(Identifier::Operator(Operator("+".to_owned()))),
            args: vec![
                InfixPrimary::Primary(ValuePrimary::Literal(Literal::Int(IntLiteral(1)))),
                InfixPrimary::Application(PrefixApply {
                    func: ValuePrimary::Identifier(Tagged::new(ScopedIdentifier::from(Identifier::Name(Name("f".to_owned()))))),
                    args: vec![ValuePrimary::Literal(Literal::Int(IntLiteral(2)))],
                }),
                InfixPrimary::Primary(ValuePrimary::SubExpression(Box::new(
                    ValueExpression::Application(BasicFixity::Infix(InfixApply {
                        func: Tagged::new(Identifier::Operator(Operator("*".to_owned()))),
                        args: vec![
                            InfixPrimary::Primary(ValuePrimary::Identifier(Tagged::new(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned())))))),
                            InfixPrimary::Primary(ValuePrimary::Literal(Literal::Int(IntLiteral(3)))),
                        ],
                    }))
                ))),
            ],
        }));
        let expr = [
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(1)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator("+".to_owned())))),
            Token::new(TokenValue::Identifier(Identifier::Name(Name("f".to_owned())))),
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(2)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator("+".to_owned())))),
            Token::new(TokenValue::Separator(Separator::LeftRound)),
            Token::new(TokenValue::Identifier(Identifier::Name(Name("a".to_owned())))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator("*".to_owned())))),
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(3)))),
            Token::new(TokenValue::Separator(Separator::RightRound)),
        ];
        let input = "1 + f 2 + (a * 3)";
        let mut tokens = Token::parse_sequence(input);
        for t in &mut tokens {
            t.len = 0;
            t.col = 0;
        }
        assert_eq!(&tokens, &expr);
        let result = ValueExpression::parse(TokenStream(&tokens));
        assert!(result.is_ok(), format!("Expected ok expression parse, got {:?}", result));
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn expr_with_lambda() {
        let input = "a + (lam f. f a)";
        let expected = ValueExpression::Application(BasicFixity::Infix(InfixApply {
            func: Tagged {
                value: Identifier::Operator(Operator("+".to_owned())),
                idx: input.find('+').unwrap(),
                len: 1,
            },
            args: vec![
                InfixPrimary::Primary(ValuePrimary::Identifier(Tagged {
                    value: ScopedIdentifier::from(Identifier::Name(Name("a".to_owned()))),
                    idx: 0,
                    len: 1,
                })),
                InfixPrimary::Primary(ValuePrimary::SubExpression(Box::new(ValueExpression::Lambda(LambdaExpression::Value {
                    params: vec![
                        PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("f".to_owned())))),
                    ],
                    body: Box::new(ValueExpression::Application(BasicFixity::Prefix(PrefixApply {
                        func: ValuePrimary::Identifier(Tagged {
                            value: ScopedIdentifier::from(Identifier::Name(Name("f".to_owned()))),
                            idx: 12,
                            len: 1,
                        }),
                        args: vec![
                            ValuePrimary::Identifier(Tagged {
                                value: ScopedIdentifier::from(Identifier::Name(Name("a".to_owned()))),
                                idx: 14,
                                len: 1,
                            }),
                        ],
                    }))),
                }))))
            ],
        }));
        let result = Token::parse_sequence(input);
        let result = ValueExpression::parse(TokenStream(result.as_slice()));
        assert!(result.is_ok(), "Expected ok result, got {:?}", result);
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }
}
