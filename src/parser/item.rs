use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{map, map_res, opt};
use nom::multi::{count, many0, many1};
use nom::sequence::{delimited, pair, preceded, separated_pair, tuple};

use crate::parser::fixity::prefix_operator;
use crate::parser::ParseResult;
use crate::parser::pattern::PatternPrimary;
use crate::parser::primary::{name, Primary};
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::token::fixed::{Keyword, Separator};
use crate::parser::token::identifier::Identifier;
use crate::parser::typedecl::{TypeExpression, TypePrimary};
use crate::parser::value::ValueExpression;

/// Fixity of a binary operator.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Fixity { Left, Right, None }

/// A definition `def f a => b`.
#[derive(Clone, Debug, PartialEq)]
pub struct Definition {
    /// The name of the defined value.
    pub name: Identifier,
    /// The fixity of the definition (only relevant for symbolic definitions).
    pub fixity: Fixity,
    /// The declared type of the defined value (optional).
    pub typ: TypeExpression,
    /// The initial parameter patterns (may be empty).
    pub params: Vec<PatternPrimary>,
    /// The one or more expression bodies for this value.
    pub bodies: Vec<DefinitionBody>,
}

impl Definition {
    /// Parses a regular definition.
    pub fn regular(input: TokenStream) -> ParseResult<TokenStream, Self> {
        map(
            tuple((
                preceded(tag(TokenValue::from(Keyword::Def)), Self::definition_name),
                opt(delimited(
                    tag(TokenValue::from(Separator::Colon)),
                    TypeExpression::parse,
                    tag(TokenValue::from(Separator::Semicolon)),
                )),
                many0(PatternPrimary::parse),
                alt((
                    many1(DefinitionBody::multiple),
                    count(DefinitionBody::single, 1),
                ))
            )),
            |((fixity, name), typ, params, bodies)| Self {
                name,
                fixity,
                typ: typ.unwrap_or_else(|| TypeExpression::TypePrimary(TypePrimary::TypeHole)),
                params,
                bodies,
            },
        )(input)
    }

    /// Parses an intrinsic definition. An intrinsic definition must use an alphanumeric name,
    /// and consists of only a name and explicit type.
    pub fn intrinsic(input: TokenStream) -> ParseResult<TokenStream, Self> {
        map(
            separated_pair(
                preceded(tag(TokenValue::from(Keyword::Def)), name),
                tag(TokenValue::from(Separator::Colon)),
                TypeExpression::parse,
            ),
            |(name, typ)| Self {
                name: Identifier::Name(name),
                fixity: Fixity::None,
                typ,
                params: vec![],
                bodies: vec![],
            },
        )(input)
    }

    fn definition_name(input: TokenStream) -> ParseResult<TokenStream, (Fixity, Identifier)> {
        use Identifier::Operator;
        map_res(
            tuple((
                opt(tag(TokenValue::from(Separator::Check))),
                prefix_operator,
                opt(tag(TokenValue::from(Separator::Check))),
            )),
            |tup| {
                match tup {
                    (Some(_), op @ Operator(_), None) => Ok((Fixity::Left, op)),
                    (None, op @ Operator(_), Some(_)) => Ok((Fixity::Right, op)),
                    (None, op, None) => Ok((Fixity::None, op)),
                    _ => Err("Invalid fixity"),
                }
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
    use crate::parser::fixity::{BasicFixity, InfixApply, InfixPrimary, PrefixApply};
    use crate::parser::pattern::Pattern;
    use crate::parser::scoped::ScopedIdentifier;
    use crate::parser::tagged::Tagged;
    use crate::parser::token::identifier::{Name, Operator};
    use crate::parser::token::Token;
    use crate::parser::typedecl::typelambda::TypeLambda;
    use crate::parser::typedecl::TypePrimary;
    use crate::parser::value::ValuePrimary;

    use super::*;

    #[test]
    fn single() {
        let input = "def '(@) a (Id b) => a + b + a";
        let expected = Definition {
            name: Identifier::Operator(Operator("@".to_owned())),
            fixity: Fixity::Left,
            typ: TypeExpression::TypePrimary(TypePrimary::TypeHole),
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
                    body: ValueExpression::Application(BasicFixity::Infix(InfixApply {
                        func: Tagged {
                            value: Identifier::Operator(Operator("+".to_owned())),
                            idx: input.find("+").unwrap(),
                            len: 1,
                        },
                        args: vec![
                            InfixPrimary::Primary(ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned()))))),
                            InfixPrimary::Primary(ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("b".to_owned()))))),
                            InfixPrimary::Primary(ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned()))))),
                        ],
                    })),
                }
            ],
        };
        let result = Token::parse_sequence(input);
        let result = Definition::regular(TokenStream(result.as_slice()));
        assert!(result.is_ok(), "Expected ok result, got {:?}", result);
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn multiple() {
        let expected = Definition {
            name: Identifier::Name(Name("bind".to_owned())),
            fixity: Fixity::None,
            typ: TypeExpression::TypePrimary(TypePrimary::TypeHole),
            params: vec![PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("f".to_owned()))))],
            bodies: vec![
                DefinitionBody {
                    params: vec![
                        PatternPrimary::SubPattern(Box::new(Pattern::Application(PrefixApply {
                            func: PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("Some".to_owned())))),
                            args: vec![PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned()))))],
                        })))
                    ],
                    body: ValueExpression::Application(BasicFixity::Prefix(PrefixApply {
                        func: ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("f".to_owned())))),
                        args: vec![ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned()))))],
                    })),
                },
                DefinitionBody {
                    params: vec![PatternPrimary::Blank],
                    body: ValueExpression::Application(BasicFixity::Primary(ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("None".to_owned())))))),
                }
            ],
        };
        let input = "
            def bind f
                ; (Some a) => f a
                ; _ => None
        ";
        let result = Token::parse_sequence(input);
        let result = Definition::regular(TokenStream(result.as_slice()));
        assert!(result.is_ok(), "Expected ok result, got {:?}", result);
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn with_type() {
        let input = "
            def const: 'a -> (for b. b -> 'a);
                a _ => a
        ";
        let expected = Definition {
            name: Identifier::Name(Name("const".to_owned())),
            fixity: Fixity::None,
            typ: TypeExpression::InfixTypeApplication(InfixApply {
                func: Tagged {
                    value: Identifier::Operator(Operator("->".to_owned())),
                    idx: input.find("->").unwrap(),
                    len: 2,
                },
                args: vec![
                    InfixPrimary::Primary(TypePrimary::TypeVariable(Name("a".to_owned()))),
                    InfixPrimary::Primary(TypePrimary::TypeSubExpression(Box::new(
                        TypeExpression::TypeLambda(TypeLambda::Value {
                            params: vec![Name("b".to_owned())],
                            body: Box::new(TypeExpression::InfixTypeApplication(InfixApply {
                                func: Tagged {
                                    value: Identifier::Operator(Operator("->".to_owned())),
                                    idx: input.match_indices("->").nth(1).unwrap().0,
                                    len: 2,
                                },
                                args: vec![
                                    InfixPrimary::Primary(TypePrimary::TypeIdentifier(ScopedIdentifier::from(Identifier::Name(Name("b".to_owned()))))),
                                    InfixPrimary::Primary(TypePrimary::TypeVariable(Name("a".to_owned()))),
                                ],
                            })),
                        })
                    )))
                ],
            }),
            params: vec![
                PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned())))),
                PatternPrimary::Blank,
            ],
            bodies: vec![
                DefinitionBody {
                    params: vec![],
                    body: ValueExpression::Application(BasicFixity::Primary(ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned())))))),
                }
            ],
        };
        let result = Token::parse_sequence(input);
        let result = Definition::regular(TokenStream(result.as_slice()));
        assert!(result.is_ok(), "Expected ok result, got {:?}", result);
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn intrinsic() {
        let input = "def addi: Int -> Int -> Int";
        let expected = Definition {
            name: Identifier::Name(Name("addi".to_owned())),
            fixity: Fixity::None,
            typ: TypeExpression::InfixTypeApplication(InfixApply {
                func: Tagged {
                    value: Identifier::Operator(Operator("->".to_owned())),
                    idx: input.find("->").unwrap(),
                    len: 2,
                },
                args: vec![
                    InfixPrimary::Primary(TypePrimary::TypeIdentifier(ScopedIdentifier::from(Identifier::Name(Name("Int".to_owned()))))),
                    InfixPrimary::Primary(TypePrimary::TypeIdentifier(ScopedIdentifier::from(Identifier::Name(Name("Int".to_owned()))))),
                    InfixPrimary::Primary(TypePrimary::TypeIdentifier(ScopedIdentifier::from(Identifier::Name(Name("Int".to_owned()))))),
                ],
            }),
            params: vec![],
            bodies: vec![],
        };
        let result = Token::parse_sequence(input);
        let result = Definition::intrinsic(TokenStream(result.as_slice()));
        assert!(result.is_ok(), "Expected ok result, got {:?}", result);
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn fixity() {
        let input = [
            ("'(@)", Fixity::Left),
            ("(@)'", Fixity::Right),
            ("(@)", Fixity::None),
        ];
        for &(i, f) in input.iter() {
            let result = Token::parse_sequence(i);
            let result = Definition::definition_name(TokenStream(result.as_slice()));
            assert!(result.is_ok(), "Expected ok result, got {:?}", result);
            let (rest, (result, _)) = result.unwrap();
            assert_eq!(rest.0, &[]);
            assert_eq!(result, f);
        }
    }

    #[test]
    fn fixity_errs() {
        let input = [
            "'f",
            "'(@)'",
        ];
        for &i in input.iter() {
            let result = Token::parse_sequence(i);
            let result = Definition::definition_name(TokenStream(result.as_slice()));
            assert!(result.is_err(), "Expected err result, got {:?}", result);
        }
    }
}
