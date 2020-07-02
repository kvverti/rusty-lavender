use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric1, one_of};
use nom::character::is_alphabetic;
use nom::combinator::{map, verify};
use nom::IResult;
use nom::multi::{fold_many1, many1};

use crate::parser::{Source, with_input};

/// An alphanumeric identifier.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Name<'a> {
    pub src: Source<'a>,
    pub value: String,
}

impl<'a> Name<'a> {
    pub fn parse(input: Source<'a>) -> IResult<Source<'a>, Self> {
        map(
            with_input(verify(
                fold_many1(
                    alt((alphanumeric1, tag("_"))),
                    String::new(), |a, b| a + b,
                ),
                |s: &str| {
                    let s = s.as_bytes();
                    is_alphabetic(s[0]) || s[0] == b'_'
                })),
            |(src, v)| Self {
                src,
                value: v.to_owned(),
            },
        )(input)
    }
}

/// A symbolic identifier. Symbolic identifiers may be used for function and type names, but
/// not for parameter names or module names.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Operator<'a> {
    pub src: Source<'a>,
    pub value: String,
}

impl<'a> Operator<'a> {
    pub fn parse(input: Source<'a>) -> IResult<Source<'a>, Self> {
        map(
            with_input(many1(one_of("~!@$%^&*-+=|:<>?,./"))),
            |(src, v)| Self {
                src,
                value: v.into_iter().collect(),
            },
        )(input)
    }
}

/// An identifier.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Identifier<'a> {
    Name(Name<'a>),
    Operator(Operator<'a>),
}

impl<'a> Identifier<'a> {
    pub fn parse(input: Source<'a>) -> IResult<Source<'a>, Self> {
        alt((
            map(Name::parse, Self::Name),
            map(Operator::parse, Self::Operator),
        ))(input)
    }
}

#[cfg(test)]
mod tests {
    use nom::Err::Error;
    use nom::error::ErrorKind;

    use super::*;

    #[test]
    fn names() {
        let cases = [
            ("thing$", Ok(("$", Name { src: "thing", value: "thing".to_owned() }))),
            ("abd_123", Ok(("", Name { src: "abd_123", value: "abd_123".to_owned() }))),
            ("12abc", Err(Error(("12abc", ErrorKind::Verify)))),
            ("_12abc", Ok(("", Name { src: "_12abc", value: "_12abc".to_owned() }))),
        ];
        for c in &cases {
            assert_eq!(Name::parse(c.0), c.1);
        }
    }

    #[test]
    fn operators() {
        let cases = [
            ("<*>", Ok(("", Operator { src: "<*>", value: "<*>".to_owned() }))),
            ("???[", Ok(("[", Operator { src: "???", value: "???".to_owned() }))),
            ("a!!", Err(Error(("a!!", ErrorKind::OneOf)))),
        ];
        for c in &cases {
            assert_eq!(Operator::parse(c.0), c.1);
        }
    }
}
