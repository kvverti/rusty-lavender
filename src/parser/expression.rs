use nom::branch::alt;
use nom::combinator::map;

use crate::parser::expression::infix::InfixApply;
use crate::parser::expression::prefix::PrefixApply;
use crate::parser::expression::primary::Primary;
use crate::parser::ParseResult;
use crate::parser::token::TokenStream;

pub mod prefix;
pub mod infix;
pub mod primary;

/// The types of Lavender expressions.
#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionNode {
    Primary(Primary),
    /// Function application `a b`
    Application(PrefixApply),
    /// Infix function application `a @ b`
    InfixApplication(InfixApply),
}

impl ExpressionNode {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        alt((
            map(InfixApply::parse, Self::InfixApplication),
            map(PrefixApply::parse, Self::Application),
            map(Primary::parse, Self::Primary),
        ))(input)
    }
}

impl From<Primary> for ExpressionNode {
    fn from(v: Primary) -> Self {
        ExpressionNode::Primary(v)
    }
}

impl From<PrefixApply> for ExpressionNode {
    fn from(v: PrefixApply) -> Self {
        ExpressionNode::Application(v)
    }
}

impl From<InfixApply> for ExpressionNode {
    fn from(v: InfixApply) -> Self {
        ExpressionNode::InfixApplication(v)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::expression::infix::InfixPrimary;
    use crate::parser::token::{Token, TokenValue};
    use crate::parser::token::fixed::Separator;
    use crate::parser::token::identifier::{Identifier, Name, Operator};
    use crate::parser::token::literal::{IntLiteral, Literal};

    use super::*;

    #[test]
    fn parses() {
        let expected = ExpressionNode::InfixApplication(InfixApply {
            func: Identifier::Operator(Operator("+".to_owned())),
            args: vec![
                InfixPrimary::Primary(Primary::Literal(Literal::Int(IntLiteral(1)))),
                InfixPrimary::Application(PrefixApply {
                    func: Primary::Identifier(Identifier::Name(Name("f".to_owned()))),
                    args: vec![Primary::Literal(Literal::Int(IntLiteral(2)))],
                }),
                InfixPrimary::Primary(Primary::SubExpression(Box::new(
                    ExpressionNode::InfixApplication(InfixApply {
                        func: Identifier::Operator(Operator("*".to_owned())),
                        args: vec![
                            InfixPrimary::Primary(Primary::Identifier(Identifier::Name(Name("a".to_owned())))),
                            InfixPrimary::Primary(Primary::Literal(Literal::Int(IntLiteral(3)))),
                        ],
                    })
                ))),
            ],
        });
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
        let tokens = Token::parse_sequence(input);
        assert!(tokens.is_ok(), format!("Expected ok token parse, got {:?}", tokens));
        let (rest, mut tokens) = tokens.unwrap();
        for t in &mut tokens {
            t.len = 0;
            t.col = 0;
        }
        assert_eq!(rest, "");
        assert_eq!(&tokens, &expr);
        let result = ExpressionNode::parse(TokenStream(&tokens));
        assert!(result.is_ok(), format!("Expected ok expression parse, got {:?}", result));
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }
}
