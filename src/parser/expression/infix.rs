use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{map, not};
use nom::error::context;
use nom::multi::many0;
use nom::sequence::{delimited, preceded, tuple};

use crate::parser::expression::prefix::PrefixApply;
use crate::parser::expression::primary::Primary;
use crate::parser::ParseResult;
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::token::fixed::Separator;
use crate::parser::token::identifier::Identifier;

/// The primary expression type for infix application.
#[derive(Clone, Debug, PartialEq)]
pub enum InfixPrimary {
    Application(PrefixApply),
    Primary(Primary),
}

impl InfixPrimary {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        alt((
            map(PrefixApply::parse, Self::Application),
            map(Primary::parse, Self::Primary),
        ))(input)
    }
}

/// Infix function application `a @ b @ c ...` where `a`, `b`, etc. are prefix applications
/// and `@` is a single operator (which may be left or right associative - we don't decide that
/// here).
#[derive(Clone, Debug, PartialEq)]
pub struct InfixApply {
    /// The name of the function.
    pub func: Identifier,
    /// The arguments. At least two.
    pub args: Vec<InfixPrimary>,
}

impl InfixApply {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        let (input, (first, func, second)) = tuple((InfixPrimary::parse, Self::parse_operator, InfixPrimary::parse))(input)?;
        let op = TokenValue::from(func.clone());
        let (input, rest) = many0(preceded(tag(op), InfixPrimary::parse))(input)?;
        let (input, _) = context("Infix operators cannot be mixed", not(Self::parse_operator))(input)?;
        let mut args = vec![first, second];
        args.extend(rest.into_iter());
        Ok((input, Self { func, args }))
    }

    /// Parses an infix operator `@` or name `a` (in backticks).
    fn parse_operator(input: TokenStream) -> ParseResult<TokenStream, Identifier> {
        alt((
            map(Primary::parse_operator, Identifier::Operator),
            map(
                delimited(
                    tag(TokenValue::from(Separator::BackTick)),
                    Primary::parse_name,
                    tag(TokenValue::from(Separator::BackTick)),
                ),
                Identifier::Name,
            )
        ))(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::token::identifier::{Name, Operator};
    use crate::parser::token::literal::{IntLiteral, Literal};
    use crate::parser::token::Token;

    use super::*;

    #[test]
    fn name() {
        // infix name
        let expected = Identifier::Name(Name("hello".to_owned()));
        let infix_name = [
            Token::new(TokenValue::Separator(Separator::BackTick)),
            Token::new(TokenValue::Identifier(Identifier::Name(Name("hello".to_owned())))),
            Token::new(TokenValue::Separator(Separator::BackTick)),
        ];
        let result = InfixApply::parse_operator(TokenStream(&infix_name));
        assert!(result.is_ok(), format!("Expected ok parse, got {:?}", result));
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn operator() {
        // infix operator
        let expected = Identifier::Operator(Operator("**".to_owned()));
        let infix_name = [
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator("**".to_owned())))),
        ];
        let result = InfixApply::parse_operator(TokenStream(&infix_name));
        assert!(result.is_ok(), format!("Expected ok parse, got {:?}", result));
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn invalid_name() {
        // invalid infix name
        let infix_name = [
            Token::new(TokenValue::Identifier(Identifier::Name(Name("hello".to_owned())))),
        ];
        let result = InfixApply::parse_operator(TokenStream(&infix_name));
        assert!(result.is_err(), format!("Expected error parse, got {:?}", result));
    }

    #[test]
    fn invalid_operator() {
        // invalid infix operator
        let infix_name = [
            Token::new(TokenValue::Separator(Separator::BackTick)),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator("**".to_owned())))),
            Token::new(TokenValue::Separator(Separator::BackTick)),
        ];
        let result = InfixApply::parse_operator(TokenStream(&infix_name));
        assert!(result.is_err(), format!("Expected error parse, got {:?}", result));
    }

    #[test]
    fn operator_expression() {
        let expected = InfixApply {
            func: Identifier::Operator(Operator("@".to_owned())),
            args: vec![
                InfixPrimary::Primary(Primary::Literal(Literal::Int(IntLiteral(7)))),
                InfixPrimary::Application(PrefixApply {
                    func: Primary::Identifier(Identifier::Name(Name("f".to_owned()))),
                    args: vec![Primary::Literal(Literal::Int(IntLiteral(8)))],
                }),
                InfixPrimary::Application(PrefixApply {
                    func: Primary::Identifier(Identifier::Name(Name("g".to_owned()))),
                    args: vec![Primary::Literal(Literal::Int(IntLiteral(9)))],
                }),
                InfixPrimary::Primary(Primary::Literal(Literal::Int(IntLiteral(10)))),
            ],
        };
        let expr = [
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(7)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator("@".to_owned())))),
            Token::new(TokenValue::Identifier(Identifier::Name(Name("f".to_owned())))),
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(8)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator("@".to_owned())))),
            Token::new(TokenValue::Identifier(Identifier::Name(Name("g".to_owned())))),
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(9)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator("@".to_owned())))),
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(10)))),
        ];
        let result = InfixApply::parse(TokenStream(&expr));
        assert!(result.is_ok(), format!("Expected ok parse, got {:?}", result));
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn operator_expression_no_extra() {
        let expected = InfixApply {
            func: Identifier::Operator(Operator("@".to_owned())),
            args: vec![
                InfixPrimary::Primary(Primary::Literal(Literal::Int(IntLiteral(7)))),
                InfixPrimary::Application(PrefixApply {
                    func: Primary::Identifier(Identifier::Name(Name("f".to_owned()))),
                    args: vec![Primary::Literal(Literal::Int(IntLiteral(8)))],
                }),
            ],
        };
        let expr = [
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(7)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator("@".to_owned())))),
            Token::new(TokenValue::Identifier(Identifier::Name(Name("f".to_owned())))),
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(8)))),
        ];
        let result = InfixApply::parse(TokenStream(&expr));
        assert!(result.is_ok(), format!("Expected ok parse, got {:?}", result));
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn invalid_mixed_operators() {
        let expr = [
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(7)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator("@".to_owned())))),
            Token::new(TokenValue::Identifier(Identifier::Name(Name("f".to_owned())))),
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(8)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator("@".to_owned())))),
            Token::new(TokenValue::Identifier(Identifier::Name(Name("g".to_owned())))),
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(9)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator("+".to_owned())))),
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(10)))),
        ];
        let result = InfixApply::parse(TokenStream(&expr));
        assert!(result.is_err(), format!("Expected error parse, got {:?}", result));
    }

    #[test]
    fn invalid_incomplete() {
        let expr = [
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(7)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator("@".to_owned())))),
        ];
        let result = InfixApply::parse(TokenStream(&expr));
        assert!(result.is_err(), format!("Expected error parse, got {:?}", result));
    }
}
