use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{map, not};
use nom::error::context;
use nom::multi::{many0, many1};
use nom::sequence::{delimited, preceded, tuple};

use crate::parser::ParseResult;
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::token::fixed::Separator;
use crate::parser::token::identifier::Identifier;
use crate::parser::primary::{operator, name};

/// Parses a prefix application with the given primary.
pub fn prefix_apply<O, F>(primary: F) -> impl Fn(TokenStream) -> ParseResult<TokenStream, (O, Vec<O>)>
    where F: Fn(TokenStream) -> ParseResult<TokenStream, O>,
{
    move |input| {
        let (input, first) = primary(input)?;
        let (input, rest) = many1(&primary)(input)?;
        Ok((input, (first, rest)))
    }
}

/// Parses an infix application with the given primary.
pub fn infix_apply<O, F>(primary: F) -> impl Fn(TokenStream) -> ParseResult<TokenStream, (Identifier, Vec<O>)>
    where F: Fn(TokenStream) -> ParseResult<TokenStream, O>,
{
    move |input| {
        let (input, (first, func, second)) = tuple((&primary, parse_operator, &primary))(input)?;
        let op = TokenValue::from(func.clone());
        let (input, rest) = many0(preceded(tag(op), &primary))(input)?;
        let (input, _) = context("Infix operators cannot be mixed", not(parse_operator))(input)?;
        let mut args = vec![first, second];
        args.extend(rest.into_iter());
        Ok((input, (func, args)))
    }
}

/// Parses an infix operator `@` or name `a` (in backticks).
fn parse_operator(input: TokenStream) -> ParseResult<TokenStream, Identifier> {
    alt((
        map(operator, Identifier::Operator),
        map(
            delimited(
                tag(TokenValue::from(Separator::BackTick)),
                name,
                tag(TokenValue::from(Separator::BackTick)),
            ),
            Identifier::Name,
        )
    ))(input)
}

#[cfg(test)]
mod tests {
    use crate::parser::token::{Token, TokenStream, TokenValue};
    use crate::parser::token::fixed::Separator;
    use crate::parser::token::identifier::{Identifier, Name, Operator};

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
        let result = parse_operator(TokenStream(&infix_name));
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
        let result = parse_operator(TokenStream(&infix_name));
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
        let result = parse_operator(TokenStream(&infix_name));
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
        let result = parse_operator(TokenStream(&infix_name));
        assert!(result.is_err(), format!("Expected error parse, got {:?}", result));
    }
}
