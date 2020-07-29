use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{map, opt};
use nom::multi::{count, many1};
use nom::sequence::{delimited, pair, preceded, tuple};

use crate::parser::fixity::prefix_operator;
use crate::parser::ParseResult;
use crate::parser::pattern::PatternPrimary;
use crate::parser::primary::Primary;
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::token::fixed::{Keyword, Separator};
use crate::parser::token::identifier::Identifier;
use crate::parser::typedecl::TypeExpression;
use crate::parser::value::ValueExpression;

/// A definition `def f a => b`.
#[derive(Clone, Debug, PartialEq)]
pub struct Definition {
    /// The name of the defined value.
    pub name: Identifier,
    /// The declared type of the defined value (optional).
    pub typ: Option<TypeExpression>,
    /// The initial parameter patterns (may be empty).
    pub params: Vec<PatternPrimary>,
    /// The one or more expression bodies for this value.
    pub bodies: Vec<DefinitionBody>,
}

impl Definition {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        map(
            tuple((
                preceded(tag(TokenValue::from(Keyword::Def)), prefix_operator),
                opt(delimited(
                    tag(TokenValue::from(Separator::Colon)),
                    TypeExpression::parse,
                    tag(TokenValue::from(Separator::Semicolon)),
                )),
                many1(PatternPrimary::parse),
                alt((
                    many1(DefinitionBody::multiple),
                    count(DefinitionBody::single, 1),
                ))
            )),
            |(name, typ, params, bodies)| Self {
                name,
                typ,
                params,
                bodies,
            },
        )(input)
    }
}

/// An expression body for a definition. Definitions may have multiple bodies.
#[derive(Clone, Debug, PartialEq)]
pub struct DefinitionBody {
    /// Any additional parameter patterns (may be empty).
    pub params: Vec<PatternPrimary>,
    /// The definition body.
    pub body: ValueExpression,
}

impl DefinitionBody {
    /// Parses a multiple definition body `; a => b`.
    pub fn multiple(input: TokenStream) -> ParseResult<TokenStream, Self> {
        map(
            pair(
                preceded(tag(TokenValue::from(Separator::Semicolon)), many1(PatternPrimary::parse)),
                preceded(tag(TokenValue::from(Separator::FatArrow)), ValueExpression::parse),
            ),
            |(params, body)| Self { params, body },
        )(input)
    }

    /// Parses a single definition body `=> b`.
    pub fn single(input: TokenStream) -> ParseResult<TokenStream, Self> {
        map(
            preceded(tag(TokenValue::from(Separator::FatArrow)), ValueExpression::parse),
            |body| Self { params: vec![], body },
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
    use crate::parser::typedecl::typelambda::TypeLambda;
    use crate::parser::typedecl::TypePrimary;
    use crate::parser::value::ValuePrimary;

    use super::*;

    #[test]
    fn single() {
        let expected = Definition {
            name: Identifier::Operator(Operator("@".to_owned())),
            typ: None,
            params: vec![
                PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned())))),
                PatternPrimary::SubPattern(Box::new(Pattern::Application(PrefixApply {
                    func: PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("Id".to_owned())))),
                    args: vec![PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("b".to_owned()))))],
                })))
            ],
            bodies: vec![
                DefinitionBody {
                    params: vec![],
                    body: ValueExpression::InfixApplication(InfixApply {
                        func: Identifier::Operator(Operator("+".to_owned())),
                        args: vec![
                            InfixPrimary::Primary(ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned()))))),
                            InfixPrimary::Primary(ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("b".to_owned()))))),
                            InfixPrimary::Primary(ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned()))))),
                        ],
                    }),
                }
            ],
        };
        let input = "def (@) a (Id b) => a + b + a";
        let (_, result) = Token::parse_sequence(input).expect("Unable to parse tokens");
        let result = Definition::parse(TokenStream(result.as_slice()));
        assert!(result.is_ok(), "Expected ok result, got {:?}", result);
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn multiple() {
        let expected = Definition {
            name: Identifier::Name(Name("bind".to_owned())),
            typ: None,
            params: vec![PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("f".to_owned()))))],
            bodies: vec![
                DefinitionBody {
                    params: vec![
                        PatternPrimary::SubPattern(Box::new(Pattern::Application(PrefixApply {
                            func: PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("Some".to_owned())))),
                            args: vec![PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned()))))],
                        })))
                    ],
                    body: ValueExpression::Application(PrefixApply {
                        func: ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("f".to_owned())))),
                        args: vec![ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned()))))],
                    }),
                },
                DefinitionBody {
                    params: vec![PatternPrimary::Blank],
                    body: ValueExpression::Primary(ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("None".to_owned()))))),
                }
            ],
        };
        let input = "
            def bind f
                ; (Some a) => f a
                ; _ => None
        ";
        let (_, result) = Token::parse_sequence(input).expect("Unable to parse tokens");
        let result = Definition::parse(TokenStream(result.as_slice()));
        assert!(result.is_ok(), "Expected ok result, got {:?}", result);
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn with_type() {
        let expected = Definition {
            name: Identifier::Name(Name("const".to_owned())),
            typ: Some(TypeExpression::InfixTypeApplication(InfixApply {
                func: Identifier::Operator(Operator("->".to_owned())),
                args: vec![
                    InfixPrimary::Primary(TypePrimary::TypeVariable(Name("a".to_owned()))),
                    InfixPrimary::Primary(TypePrimary::TypeSubExpression(Box::new(
                        TypeExpression::TypeLambda(TypeLambda {
                            params: vec![Name("b".to_owned())],
                            body: Box::new(TypeExpression::InfixTypeApplication(InfixApply {
                                func: Identifier::Operator(Operator("->".to_owned())),
                                args: vec![
                                    InfixPrimary::Primary(TypePrimary::TypeIdentifier(ScopedIdentifier::from(Identifier::Name(Name("b".to_owned()))))),
                                    InfixPrimary::Primary(TypePrimary::TypeVariable(Name("a".to_owned()))),
                                ],
                            })),
                        })
                    )))
                ],
            })),
            params: vec![
                PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned())))),
                PatternPrimary::Blank,
            ],
            bodies: vec![
                DefinitionBody {
                    params: vec![],
                    body: ValueExpression::Primary(ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned()))))),
                }
            ],
        };
        let input = "
            def const: 'a -> (for b. b -> 'a);
                a _ => a
        ";
        let (_, result) = Token::parse_sequence(input).expect("Unable to parse tokens");
        let result = Definition::parse(TokenStream(result.as_slice()));
        assert!(result.is_ok(), "Expected ok result, got {:?}", result);
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }
}
