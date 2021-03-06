use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::multi::{many0, many1};
use nom::sequence::{delimited, preceded};

use crate::parser::primary::{name, operator, Primary};
use crate::parser::tagged::{tagged, Tagged};
use crate::parser::token::fixed::Separator;
use crate::parser::token::identifier::Identifier;
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::ParseResult;

/// Prefix function application `a b c ...`, where all of `a`, `b`, `c` are primaries.
#[derive(Clone, Debug, PartialEq)]
pub struct PrefixApply<P> {
    /// The function expression.
    pub func: P,
    /// The function arguments. Nonempty.
    pub args: Vec<P>,
}

/// The primary expression type for infix application.
#[derive(Clone, Debug, PartialEq)]
pub enum InfixPrimary<P> {
    Application(PrefixApply<P>),
    Primary(P),
}

impl<P: Primary> InfixPrimary<P> {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        let (input, mut primaries) = many1(P::parse)(input)?;
        let rest = primaries.drain(1..).collect::<Vec<_>>();
        let first = primaries.pop().unwrap();
        let value = if rest.is_empty() {
            Self::Primary(first)
        } else {
            Self::Application(PrefixApply {
                func: first,
                args: rest,
            })
        };
        Ok((input, value))
    }
}

/// Infix function application `a @ b @ c ...` where `a`, `b`, etc. are prefix applications
/// and `@` is a single operator (which may be left or right associative - we don't decide that
/// here).
#[derive(Clone, Debug, PartialEq)]
pub struct InfixApply<P> {
    /// The name of the function.
    pub func: Tagged<Identifier>,
    /// The arguments. At least two.
    pub args: Vec<InfixPrimary<P>>,
}

/// A prefix expression `a b c ...` or infix expression `a @ b @ c ...` or primary expression `a`.
#[derive(Clone, Debug, PartialEq)]
pub enum BasicFixity<P> {
    Primary(P),
    Prefix(PrefixApply<P>),
    Infix(InfixApply<P>),
}

impl<P: Primary> BasicFixity<P> {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        let (input, first) = InfixPrimary::parse(input)?;
        if let Ok((input, func)) = tagged(infix_operator)(input) {
            // infix operator
            let (input, second) = InfixPrimary::parse(input)?;
            let op = TokenValue::from(func.value.clone());
            let (input, rest) = many0(preceded(tag(op), InfixPrimary::parse))(input)?;
            let mut args = vec![first, second];
            args.extend(rest.into_iter());
            Ok((input, Self::Infix(InfixApply { func, args })))
        } else {
            // prefix or primary
            let expr = match first {
                InfixPrimary::Primary(p) => Self::Primary(p),
                InfixPrimary::Application(p) => Self::Prefix(p),
            };
            Ok((input, expr))
        }
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
        ),
    ))(input)
}

#[cfg(test)]
mod tests {
    use crate::parser::scoped::ScopedIdentifier;
    use crate::parser::token::fixed::Separator;
    use crate::parser::token::identifier::{Identifier, Name, Operator};
    use crate::parser::token::literal::{IntLiteral, Literal};
    use crate::parser::token::{Token, TokenStream, TokenValue};
    use crate::parser::value::ValuePrimary;

    use super::*;

    #[test]
    fn name() {
        // infix name
        let expected = Identifier::Name(Name("hello".to_owned()));
        let infix_name = [
            Token::new(TokenValue::Separator(Separator::BackTick)),
            Token::new(TokenValue::Identifier(Identifier::Name(Name(
                "hello".to_owned(),
            )))),
            Token::new(TokenValue::Separator(Separator::BackTick)),
        ];
        let result = infix_operator(TokenStream(&infix_name));
        assert!(
            result.is_ok(),
            format!("Expected ok parse, got {:?}", result)
        );
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn operator() {
        // infix operator
        let expected = Identifier::Operator(Operator("**".to_owned()));
        let infix_name = [Token::new(TokenValue::Identifier(Identifier::Operator(
            Operator("**".to_owned()),
        )))];
        let result = infix_operator(TokenStream(&infix_name));
        assert!(
            result.is_ok(),
            format!("Expected ok parse, got {:?}", result)
        );
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn invalid_name() {
        // invalid infix name
        let infix_name = [Token::new(TokenValue::Identifier(Identifier::Name(Name(
            "hello".to_owned(),
        ))))];
        let result = infix_operator(TokenStream(&infix_name));
        assert!(
            result.is_err(),
            format!("Expected error parse, got {:?}", result)
        );
    }

    #[test]
    fn invalid_operator() {
        // invalid infix operator
        let infix_name = [
            Token::new(TokenValue::Separator(Separator::BackTick)),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator(
                "**".to_owned(),
            )))),
            Token::new(TokenValue::Separator(Separator::BackTick)),
        ];
        let result = infix_operator(TokenStream(&infix_name));
        assert!(
            result.is_err(),
            format!("Expected error parse, got {:?}", result)
        );
    }

    #[test]
    fn identifiers() {
        let name = TokenValue::from(Identifier::Name(Name("hello".to_owned())));
        let operator = TokenValue::from(Identifier::Operator(Operator("<$>".to_owned())));

        let name_input = [Token::new(name.clone())];
        let name_result = prefix_operator(TokenStream(&name_input));
        assert!(
            name_result.is_ok(),
            format!("Name result error {:?}", name_result)
        );
        assert_eq!(TokenValue::from(name_result.unwrap().1), name);

        let operator_input = [
            Token::new(TokenValue::from(Separator::LeftRound)),
            Token::new(operator.clone()),
            Token::new(TokenValue::from(Separator::RightRound)),
        ];
        let operator_result = prefix_operator(TokenStream(&operator_input));
        assert!(
            operator_result.is_ok(),
            format!("Operator result error {:?}", operator_result)
        );
        assert_eq!(TokenValue::from(operator_result.unwrap().1), operator);

        let error_op_input = [Token::new(operator)];
        let error_op_result = prefix_operator(TokenStream(&error_op_input));
        assert!(
            error_op_result.is_err(),
            format!("Bare operator did not error {:?}", error_op_result)
        );
    }

    #[test]
    fn prefix_parses() {
        let expected = BasicFixity::Prefix(PrefixApply {
            func: ValuePrimary::Identifier(Tagged::new(ScopedIdentifier::from(
                Identifier::Operator(Operator("+".to_owned())),
            ))),
            args: vec![
                ValuePrimary::Identifier(Tagged::new(ScopedIdentifier::from(Identifier::Name(
                    Name("f".to_owned()),
                )))),
                ValuePrimary::Literal(Literal::Int(IntLiteral(1))),
            ],
        });
        let success = [
            TokenValue::from(Separator::LeftRound),
            TokenValue::from(Identifier::Operator(Operator("+".to_owned()))),
            TokenValue::from(Separator::RightRound),
            TokenValue::from(Identifier::Name(Name("f".to_owned()))),
            TokenValue::from(Literal::Int(IntLiteral(1))),
        ];
        let success_vec = success
            .iter()
            .map(|t| Token::new(t.clone()))
            .collect::<Vec<_>>();
        let result = BasicFixity::parse(TokenStream(success_vec.as_slice()));
        assert!(result.is_ok(), format!("Result not ok: {:?}", result));
        let (rest, expr) = result.unwrap();
        let rest = rest.0.iter().map(|t| t.value.clone()).collect::<Vec<_>>();
        assert_eq!(rest.as_slice(), &[]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn operator_expression() {
        let expected = BasicFixity::Infix(InfixApply {
            func: Tagged::new(Identifier::Operator(Operator("@".to_owned()))),
            args: vec![
                InfixPrimary::Primary(ValuePrimary::Literal(Literal::Int(IntLiteral(7)))),
                InfixPrimary::Application(PrefixApply {
                    func: ValuePrimary::Identifier(Tagged::new(ScopedIdentifier::from(
                        Identifier::Name(Name("f".to_owned())),
                    ))),
                    args: vec![ValuePrimary::Literal(Literal::Int(IntLiteral(8)))],
                }),
                InfixPrimary::Application(PrefixApply {
                    func: ValuePrimary::Identifier(Tagged::new(ScopedIdentifier::from(
                        Identifier::Name(Name("g".to_owned())),
                    ))),
                    args: vec![ValuePrimary::Literal(Literal::Int(IntLiteral(9)))],
                }),
                InfixPrimary::Primary(ValuePrimary::Literal(Literal::Int(IntLiteral(10)))),
            ],
        });
        let expr = [
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(7)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator(
                "@".to_owned(),
            )))),
            Token::new(TokenValue::Identifier(Identifier::Name(Name(
                "f".to_owned(),
            )))),
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(8)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator(
                "@".to_owned(),
            )))),
            Token::new(TokenValue::Identifier(Identifier::Name(Name(
                "g".to_owned(),
            )))),
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(9)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator(
                "@".to_owned(),
            )))),
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(10)))),
        ];
        let result = BasicFixity::parse(TokenStream(&expr));
        assert!(
            result.is_ok(),
            format!("Expected ok parse, got {:?}", result)
        );
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn operator_expression_no_extra() {
        let expected = BasicFixity::Infix(InfixApply {
            func: Tagged::new(Identifier::Operator(Operator("@".to_owned()))),
            args: vec![
                InfixPrimary::Primary(ValuePrimary::Literal(Literal::Int(IntLiteral(7)))),
                InfixPrimary::Application(PrefixApply {
                    func: ValuePrimary::Identifier(Tagged::new(ScopedIdentifier::from(
                        Identifier::Name(Name("f".to_owned())),
                    ))),
                    args: vec![ValuePrimary::Literal(Literal::Int(IntLiteral(8)))],
                }),
            ],
        });
        let expr = [
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(7)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator(
                "@".to_owned(),
            )))),
            Token::new(TokenValue::Identifier(Identifier::Name(Name(
                "f".to_owned(),
            )))),
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(8)))),
        ];
        let result = BasicFixity::parse(TokenStream(&expr));
        assert!(
            result.is_ok(),
            format!("Expected ok parse, got {:?}", result)
        );
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn invalid_mixed_operators() {
        let expr = [
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(7)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator(
                "@".to_owned(),
            )))),
            Token::new(TokenValue::Identifier(Identifier::Name(Name(
                "f".to_owned(),
            )))),
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(8)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator(
                "@".to_owned(),
            )))),
            Token::new(TokenValue::Identifier(Identifier::Name(Name(
                "g".to_owned(),
            )))),
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(9)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator(
                "+".to_owned(),
            )))),
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(10)))),
        ];
        let result = BasicFixity::<ValuePrimary>::parse(TokenStream(&expr));
        assert!(
            result.is_ok(),
            format!("Expected ok parse, got {:?}", result)
        );
        let (rest, _) = result.unwrap();
        assert_eq!(rest.0, &expr[7..]);
    }

    #[test]
    fn invalid_incomplete() {
        let expr = [
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(7)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator(
                "@".to_owned(),
            )))),
        ];
        let result = BasicFixity::<ValuePrimary>::parse(TokenStream(&expr));
        assert!(
            result.is_err(),
            format!("Expected error parse, got {:?}", result)
        );
    }
}
