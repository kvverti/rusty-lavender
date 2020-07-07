use nom::branch::alt;
use nom::combinator::map;

use crate::parser::expression::prefix::PrefixApply;
use crate::parser::expression::primary::Primary;
use crate::parser::fixity::infix_apply;
use crate::parser::ParseResult;
use crate::parser::token::identifier::Identifier;
use crate::parser::token::TokenStream;

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
        let (input, (func, args)) = infix_apply(InfixPrimary::parse)(input)?;
        Ok((input, Self { func, args }))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::token::{Token, TokenValue};
    use crate::parser::token::identifier::{Name, Operator};
    use crate::parser::token::literal::{IntLiteral, Literal};

    use super::*;

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
