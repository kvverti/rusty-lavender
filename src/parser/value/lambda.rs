use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::multi::many1;
use nom::sequence::{delimited, pair};

use crate::parser::ParseResult;
use crate::parser::pattern::PatternPrimary;
use crate::parser::primary::Primary;
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::token::fixed::Keyword;
use crate::parser::token::identifier::{Identifier, Operator};
use crate::parser::value::ValueExpression;

/// A lambda expression (single case anonymous function) `lam a b. c`.
#[derive(Clone, Debug, PartialEq)]
pub struct LambdaExpression {
    /// The parameter patterns of the lambda expression.
    pub params: Vec<PatternPrimary>,
    /// The lambda body.
    pub body: Box<ValueExpression>,
}

impl LambdaExpression {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        map(
            pair(
                delimited(
                    tag(TokenValue::from(Keyword::Lam)),
                    many1(PatternPrimary::parse),
                    tag(TokenValue::from(Identifier::Operator(Operator(".".to_owned())))),
                ),
                ValueExpression::parse,
            ),
            |(params, body)| Self { params, body: Box::new(body) },
        )(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::fixity::{InfixApply, InfixPrimary, PrefixApply};
    use crate::parser::pattern::Pattern;
    use crate::parser::scoped::ScopedIdentifier;
    use crate::parser::token::fixed::Separator;
    use crate::parser::token::identifier::Name;
    use crate::parser::token::Token;
    use crate::parser::value::ValuePrimary;

    use super::*;

    #[test]
    fn parses() {
        let expected = LambdaExpression {
            params: vec![
                PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("x".to_owned())))),
                PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("y".to_owned())))),
                PatternPrimary::SubPattern(Box::new(Pattern::Application(PrefixApply {
                    func: PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("Id".to_owned())))),
                    args: vec![
                        PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("z".to_owned())))),
                    ],
                })))
            ],
            body: Box::new(ValueExpression::InfixApplication(InfixApply {
                func: Identifier::Operator(Operator("+".to_owned())),
                args: vec![
                    InfixPrimary::Primary(ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("x".to_owned()))))),
                    InfixPrimary::Primary(ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("z".to_owned()))))),
                ],
            })),
        };
        // lam x y (Id z). x + z
        let tokens = [
            Token::new(TokenValue::from(Keyword::Lam)),
            Token::new(TokenValue::from(Identifier::Name(Name("x".to_owned())))),
            Token::new(TokenValue::from(Identifier::Name(Name("y".to_owned())))),
            Token::new(TokenValue::from(Separator::LeftRound)),
            Token::new(TokenValue::from(Identifier::Name(Name("Id".to_owned())))),
            Token::new(TokenValue::from(Identifier::Name(Name("z".to_owned())))),
            Token::new(TokenValue::from(Separator::RightRound)),
            Token::new(TokenValue::from(Identifier::Operator(Operator(".".to_owned())))),
            Token::new(TokenValue::from(Identifier::Name(Name("x".to_owned())))),
            Token::new(TokenValue::from(Identifier::Operator(Operator("+".to_owned())))),
            Token::new(TokenValue::from(Identifier::Name(Name("z".to_owned())))),
        ];
        let result = LambdaExpression::parse(TokenStream(&tokens));
        assert!(result.is_ok(), "Expected ok result, got {:?}", result);
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }
}
