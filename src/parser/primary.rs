use nom::Err::Error;
use nom::error::{VerboseError, VerboseErrorKind};

use crate::parser::ParseResult;
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::token::identifier::{Identifier, Name, Operator};
use crate::parser::token::literal::Literal;

/// A trait for parsable primary expressions.
pub trait Primary
    where Self: Sized
{
    fn parse(input: TokenStream) -> ParseResult<TokenStream, Self>;
}

/// Accepts an alphanumeric identifier.
pub fn name(input: TokenStream) -> ParseResult<TokenStream, Name> {
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
pub fn operator(input: TokenStream) -> ParseResult<TokenStream, Operator> {
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

/// Accepts a literal token.
pub fn literal(input: TokenStream) -> ParseResult<TokenStream, Literal> {
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

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use crate::parser::token::fixed::{Keyword, Separator};
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
        test_parse(&expected, &errors, literal);
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
            name(s).map(|(a, n)| (a, Identifier::Name(n)))
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
            operator(s).map(|(a, n)| (a, Identifier::Operator(n)))
        });
    }
}