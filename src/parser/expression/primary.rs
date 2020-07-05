use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::Err::Error;
use nom::error::{VerboseError, VerboseErrorKind};
use nom::sequence::delimited;

use crate::parser::expression::ExpressionNode;
use crate::parser::ParseResult;
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::token::fixed::Separator;
use crate::parser::token::identifier::{Identifier, Name, Operator};
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
            map(Self::parse_literal, Self::Literal),
            map(Self::parse_identifier, Self::Identifier),
            map(Self::parse_subexpr, |e| Self::SubExpression(Box::new(e))),
        ))(input)
    }

    /// Accepts an alphanumeric identifier.
    pub fn parse_name(input: TokenStream) -> ParseResult<TokenStream, Name> {
        if let Some(t) = input.0.get(0).map(|t| &t.value) {
            if let TokenValue::Identifier(Identifier::Name(v)) = t {
                Ok((TokenStream(&input.0[1..]), v.clone()))
            } else {
                Err(Error(VerboseError { errors: vec![(input, VerboseErrorKind::Context("Token not a name"))] }))
            }
        } else {
            Err(Error(VerboseError { errors: vec![(input, VerboseErrorKind::Context("Token does not exist"))] }))
        }
    }

    /// Accepts a symbolic identifier.
    pub fn parse_operator(input: TokenStream) -> ParseResult<TokenStream, Operator> {
        if let Some(t) = input.0.get(0).map(|t| &t.value) {
            if let TokenValue::Identifier(Identifier::Operator(v)) = t {
                Ok((TokenStream(&input.0[1..]), v.clone()))
            } else {
                Err(Error(VerboseError { errors: vec![(input, VerboseErrorKind::Context("Token not an operator"))] }))
            }
        } else {
            Err(Error(VerboseError { errors: vec![(input, VerboseErrorKind::Context("Token does not exist"))] }))
        }
    }

    /// Parses a primary identifier: a plain name, or an operator enclosed in parentheses.
    fn parse_identifier(input: TokenStream) -> ParseResult<TokenStream, Identifier> {
        alt((
            map(Self::parse_name, Identifier::Name),
            delimited(
                tag(TokenValue::from(Separator::LeftRound)),
                map(Self::parse_operator, Identifier::Operator),
                tag(TokenValue::from(Separator::RightRound)),
            ),
        ))(input)
    }

    /// Accepts a literal token.
    fn parse_literal(input: TokenStream) -> ParseResult<TokenStream, Literal> {
        if let Some(t) = input.0.get(0).map(|t| &t.value) {
            if let TokenValue::Literal(v) = t {
                Ok((TokenStream(&input.0[1..]), *v))
            } else {
                Err(Error(VerboseError { errors: vec![(input, VerboseErrorKind::Context("Token not a literal"))] }))
            }
        } else {
            Err(Error(VerboseError { errors: vec![(input, VerboseErrorKind::Context("Token does not exist"))] }))
        }
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
    use std::fmt::Debug;

    use crate::parser::token::fixed::Keyword;
    use crate::parser::token::literal::{BoolLiteral, FloatLiteral, IntLiteral};
    use crate::parser::token::Token;

    use super::*;

    fn test_parse<P, O>(expected: &[TokenValue], errors: &[TokenValue], parser: P)
        where P: Fn(TokenStream) -> ParseResult<TokenStream, O>,
              O: Into<TokenValue> + Debug,
    {
        let token_vec = expected.iter()
            .map(|t| Token::new(t.clone()))
            .collect::<Vec<_>>();
        let mut tokens = TokenStream(token_vec.as_slice());
        let mut token_len = token_vec.len();
        for value in expected {
            let result = parser(tokens);
            if let Ok((tokens1, output)) = result {
                tokens = tokens1;
                token_len -= 1;
                assert_eq!(output.into(), *value);
                assert_eq!(tokens1.0.len(), token_len);
            } else {
                panic!(format!("Literal parse failed {:?}", result));
            }
        }

        let error_token_vec = errors.iter()
            .map(|t| Token::new(t.clone()))
            .collect::<Vec<_>>();
        let mut error_tokens = TokenStream(error_token_vec.as_slice());
        while !error_tokens.0.is_empty() {
            let result = parser(error_tokens);
            assert!(result.is_err());
            error_tokens = TokenStream(&error_tokens.0[1..]);
        }
    }

    #[test]
    fn literals() {
        let expected = [
            TokenValue::from(Literal::Int(IntLiteral(23))),
            TokenValue::from(Literal::Float(FloatLiteral(2.0))),
            TokenValue::from(Literal::Bool(BoolLiteral(true))),
        ];
        let errors = [
            TokenValue::from(Separator::LeftCurly),
            TokenValue::from(Identifier::Operator(Operator("**".to_owned()))),
            TokenValue::from(Keyword::Class),
        ];
        test_parse(&expected, &errors, Primary::parse_literal);
    }

    #[test]
    fn names() {
        let expected = [
            TokenValue::from(Identifier::Name(Name("hello".to_owned()))),
            TokenValue::from(Identifier::Name(Name("world".to_owned()))),
            TokenValue::from(Identifier::Name(Name("thingy".to_owned()))),
        ];
        let errors = [
            TokenValue::from(Identifier::Operator(Operator("**".to_owned()))),
            TokenValue::from(Separator::LeftCurly),
        ];
        test_parse(&expected, &errors, |s| {
            Primary::parse_name(s).map(|(a, n)| (a, Identifier::Name(n)))
        });
    }

    #[test]
    fn operators() {
        let expected = [
            TokenValue::from(Identifier::Operator(Operator("<@>".to_owned()))),
            TokenValue::from(Identifier::Operator(Operator("<|>".to_owned()))),
            TokenValue::from(Identifier::Operator(Operator("<$>".to_owned()))),
        ];
        let errors = [
            TokenValue::from(Identifier::Name(Name("hello".to_owned()))),
            TokenValue::from(Separator::LeftCurly),
        ];
        test_parse(&expected, &errors, |s| {
            Primary::parse_operator(s).map(|(a, n)| (a, Identifier::Operator(n)))
        });
    }

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
