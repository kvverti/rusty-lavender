use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::sequence::delimited;

use crate::parser::expression::ExpressionNode;
use crate::parser::ParseResult;
use crate::parser::primary::{literal, name, operator};
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::token::fixed::Separator;
use crate::parser::token::identifier::Identifier;
use crate::parser::token::literal::Literal;

/// A fundamental, "atomic" expression.
#[derive(Clone, Debug, PartialEq)]
pub enum Primary {
    /// A literal token `a` (such a `True` or `3`)
    Literal(Literal),
    /// A name `a` (such as `map` or `(.)`)
    Identifier(Identifier),
    /// A parenthesized expression `( a )`
    SubExpression(Box<ExpressionNode>),
}

impl Primary {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        alt((
            map(literal, Self::Literal),
            map(Self::parse_identifier, Self::Identifier),
            map(Self::parse_subexpr, |e| Self::SubExpression(Box::new(e))),
        ))(input)
    }

    /// Parses a primary identifier: a plain name, or an operator enclosed in parentheses.
    fn parse_identifier(input: TokenStream) -> ParseResult<TokenStream, Identifier> {
        alt((
            map(name, Identifier::Name),
            delimited(
                tag(TokenValue::from(Separator::LeftRound)),
                map(operator, Identifier::Operator),
                tag(TokenValue::from(Separator::RightRound)),
            ),
        ))(input)
    }

    /// Parses a parenthesized subexpression.
    /// TODO: remove line terminators inside subexpressions - another option is to force
    ///     indentation.
    fn parse_subexpr(input: TokenStream) -> ParseResult<TokenStream, ExpressionNode> {
        delimited(
            tag(TokenValue::from(Separator::LeftRound)),
            ExpressionNode::parse,
            tag(TokenValue::from(Separator::RightRound)),
        )(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::expression::primary::Primary;
    use crate::parser::token::{Token, TokenStream, TokenValue};
    use crate::parser::token::fixed::Separator;
    use crate::parser::token::identifier::{Identifier, Name, Operator};

    #[test]
    fn identifiers() {
        let name = TokenValue::from(Identifier::Name(Name("hello".to_owned())));
        let operator = TokenValue::from(Identifier::Operator(Operator("<$>".to_owned())));

        let name_input = [Token::new(name.clone())];
        let name_result = Primary::parse_identifier(TokenStream(&name_input));
        assert!(name_result.is_ok(), format!("Name result error {:?}", name_result));
        assert_eq!(TokenValue::from(name_result.unwrap().1), name);

        let operator_input = [
            Token::new(TokenValue::from(Separator::LeftRound)),
            Token::new(operator.clone()),
            Token::new(TokenValue::from(Separator::RightRound)),
        ];
        let operator_result = Primary::parse_identifier(TokenStream(&operator_input));
        assert!(operator_result.is_ok(), format!("Operator result error {:?}", operator_result));
        assert_eq!(TokenValue::from(operator_result.unwrap().1), operator);

        let error_op_input = [Token::new(operator.clone())];
        let error_op_result = Primary::parse_identifier(TokenStream(&error_op_input));
        assert!(error_op_result.is_err(), format!("Bare operator did not error {:?}", error_op_result));
    }
}
