use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{map, not};
use nom::error::context;
use nom::multi::{many0, many1};
use nom::sequence::{delimited, pair, preceded, tuple};

use crate::parser::ParseResult;
use crate::parser::primary::{name, operator, Primary};
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::token::fixed::Separator;
use crate::parser::token::identifier::Identifier;

/// Prefix function application `a b c ...`, where all of `a`, `b`, `c` are primaries.
#[derive(Clone, Debug, PartialEq)]
pub struct PrefixApply<P: Primary> {
    /// The function expression.
    pub func: P,
    /// The function arguments. Nonempty.
    pub args: Vec<P>,
}

impl<P: Primary> PrefixApply<P> {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        map(
            pair(P::parse, many1(P::parse)),
            |(func, args)| Self { func, args },
        )(input)
    }
}

/// The primary expression type for infix application.
#[derive(Clone, Debug, PartialEq)]
pub enum InfixPrimary<P: Primary> {
    Application(PrefixApply<P>),
    Primary(P),
}

impl<P: Primary> InfixPrimary<P> {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        alt((
            map(PrefixApply::parse, Self::Application),
            map(P::parse, Self::Primary),
        ))(input)
    }
}

/// Infix function application `a @ b @ c ...` where `a`, `b`, etc. are prefix applications
/// and `@` is a single operator (which may be left or right associative - we don't decide that
/// here).
#[derive(Clone, Debug, PartialEq)]
pub struct InfixApply<P: Primary> {
    /// The name of the function.
    pub func: Identifier,
    /// The arguments. At least two.
    pub args: Vec<InfixPrimary<P>>,
}

impl<P: Primary> InfixApply<P> {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        let (input, (first, func, second)) = tuple((InfixPrimary::parse, infix_operator, InfixPrimary::parse))(input)?;
        let op = TokenValue::from(func.clone());
        let (input, rest) = many0(preceded(tag(op), InfixPrimary::parse))(input)?;
        let (input, _) = context("Infix operators cannot be mixed", not(infix_operator))(input)?;
        let mut args = vec![first, second];
        args.extend(rest.into_iter());
        Ok((input, Self { func, args }))
    }
}

/// Parses a primary identifier: a plain name, or an operator enclosed in parentheses.
pub fn prefix_operator(input: TokenStream) -> ParseResult<TokenStream, Identifier> {
    alt((
        map(name, Identifier::Name),
        delimited(
            tag(TokenValue::from(Separator::LeftRound)),
            map(operator, Identifier::Operator),
            tag(TokenValue::from(Separator::RightRound)),
        ),
    ))(input)
}

/// Parses an infix operator `@` or name `a` (in backticks).
pub fn infix_operator(input: TokenStream) -> ParseResult<TokenStream, Identifier> {
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
    use crate::parser::token::literal::{IntLiteral, Literal};
    use crate::parser::value::ValuePrimary;

    use super::*;
    use crate::parser::scoped::ScopedIdentifier;

    #[test]
    fn name() {
        // infix name
        let expected = Identifier::Name(Name("hello".to_owned()));
        let infix_name = [
            Token::new(TokenValue::Separator(Separator::BackTick)),
            Token::new(TokenValue::Identifier(Identifier::Name(Name("hello".to_owned())))),
            Token::new(TokenValue::Separator(Separator::BackTick)),
        ];
        let result = infix_operator(TokenStream(&infix_name));
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
        let result = infix_operator(TokenStream(&infix_name));
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
        let result = infix_operator(TokenStream(&infix_name));
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
        let result = infix_operator(TokenStream(&infix_name));
        assert!(result.is_err(), format!("Expected error parse, got {:?}", result));
    }

    #[test]
    fn identifiers() {
        let name = TokenValue::from(Identifier::Name(Name("hello".to_owned())));
        let operator = TokenValue::from(Identifier::Operator(Operator("<$>".to_owned())));

        let name_input = [Token::new(name.clone())];
        let name_result = prefix_operator(TokenStream(&name_input));
        assert!(name_result.is_ok(), format!("Name result error {:?}", name_result));
        assert_eq!(TokenValue::from(name_result.unwrap().1), name);

        let operator_input = [
            Token::new(TokenValue::from(Separator::LeftRound)),
            Token::new(operator.clone()),
            Token::new(TokenValue::from(Separator::RightRound)),
        ];
        let operator_result = prefix_operator(TokenStream(&operator_input));
        assert!(operator_result.is_ok(), format!("Operator result error {:?}", operator_result));
        assert_eq!(TokenValue::from(operator_result.unwrap().1), operator);

        let error_op_input = [Token::new(operator.clone())];
        let error_op_result = prefix_operator(TokenStream(&error_op_input));
        assert!(error_op_result.is_err(), format!("Bare operator did not error {:?}", error_op_result));
    }

    #[test]
    fn prefix_parses() {
        let expected = PrefixApply {
            func: ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Operator(Operator("+".to_owned())))),
            args: vec![
                ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("f".to_owned())))),
                ValuePrimary::Literal(Literal::Int(IntLiteral(1))),
            ],
        };
        let success = [
            TokenValue::from(Separator::LeftRound),
            TokenValue::from(Identifier::Operator(Operator("+".to_owned()))),
            TokenValue::from(Separator::RightRound),
            TokenValue::from(Identifier::Name(Name("f".to_owned()))),
            TokenValue::from(Literal::Int(IntLiteral(1))),
            TokenValue::from(Identifier::Operator(Operator("+".to_owned()))),
        ];
        let success_vec = success.iter()
            .map(|t| Token::new(t.clone()))
            .collect::<Vec<_>>();
        let result = PrefixApply::parse(TokenStream(success_vec.as_slice()));
        assert!(result.is_ok(), format!("Result not ok: {:?}", result));
        let (rest, expr) = result.unwrap();
        let rest = rest.0.iter().map(|t| t.value.clone()).collect::<Vec<_>>();
        let len = success.len();
        assert_eq!(rest.as_slice(), &success[len - 1..]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn operator_expression() {
        let expected = InfixApply {
            func: Identifier::Operator(Operator("@".to_owned())),
            args: vec![
                InfixPrimary::Primary(ValuePrimary::Literal(Literal::Int(IntLiteral(7)))),
                InfixPrimary::Application(PrefixApply {
                    func: ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("f".to_owned())))),
                    args: vec![ValuePrimary::Literal(Literal::Int(IntLiteral(8)))],
                }),
                InfixPrimary::Application(PrefixApply {
                    func: ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("g".to_owned())))),
                    args: vec![ValuePrimary::Literal(Literal::Int(IntLiteral(9)))],
                }),
                InfixPrimary::Primary(ValuePrimary::Literal(Literal::Int(IntLiteral(10)))),
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
                InfixPrimary::Primary(ValuePrimary::Literal(Literal::Int(IntLiteral(7)))),
                InfixPrimary::Application(PrefixApply {
                    func: ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("f".to_owned())))),
                    args: vec![ValuePrimary::Literal(Literal::Int(IntLiteral(8)))],
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
        let result = InfixApply::<ValuePrimary>::parse(TokenStream(&expr));
        assert!(result.is_err(), format!("Expected error parse, got {:?}", result));
    }

    #[test]
    fn invalid_incomplete() {
        let expr = [
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(7)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator("@".to_owned())))),
        ];
        let result = InfixApply::<ValuePrimary>::parse(TokenStream(&expr));
        assert!(result.is_err(), format!("Expected error parse, got {:?}", result));
    }
}
