use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::multi::many1;
use nom::sequence::{preceded, tuple};

use crate::parser::fixity::prefix_operator;
use crate::parser::ParseResult;
use crate::parser::pattern::PatternPrimary;
use crate::parser::primary::Primary;
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::token::fixed::{Keyword, Separator};
use crate::parser::token::identifier::Identifier;
use crate::parser::value::ValueExpression;

/// A function definition `def f a => b`.
/// todo: allow multi-body functions
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition {
    pub name: Identifier,
    pub params: Vec<PatternPrimary>,
    pub body: ValueExpression,
}

impl FunctionDefinition {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        map(
            tuple((
                preceded(tag(TokenValue::from(Keyword::Def)), prefix_operator),
                many1(PatternPrimary::parse),
                preceded(tag(TokenValue::from(Separator::FatArrow)), ValueExpression::parse),
            )),
            |(name, params, body)| Self {
                name,
                params,
                body,
            },
        )(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::fixity::{InfixApply, InfixPrimary, PrefixApply};
    use crate::parser::pattern::Pattern;
    use crate::parser::scoped::ScopedIdentifier;
    use crate::parser::token::identifier::{Name, Operator};
    use crate::parser::token::Token;
    use crate::parser::value::ValuePrimary;

    use super::*;

    #[test]
    fn parses() {
        let expected = FunctionDefinition {
            name: Identifier::Operator(Operator("@".to_owned())),
            params: vec![
                PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned())))),
                PatternPrimary::SubPattern(Box::new(Pattern::Application(PrefixApply {
                    func: PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("Id".to_owned())))),
                    args: vec![PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("b".to_owned()))))],
                })))
            ],
            body: ValueExpression::InfixApplication(InfixApply {
                func: Identifier::Operator(Operator("+".to_owned())),
                args: vec![
                    InfixPrimary::Primary(ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned()))))),
                    InfixPrimary::Primary(ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("b".to_owned()))))),
                    InfixPrimary::Primary(ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned()))))),
                ],
            }),
        };
        let input = "def (@) a (Id b) => a + b + a";
        let (_, result) = Token::parse_sequence(input).expect("Unable to parse tokens");
        let result = FunctionDefinition::parse(TokenStream(result.as_slice()));
        assert!(result.is_ok(), "Expected ok result, got {:?}", result);
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }
}
