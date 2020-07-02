use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric1, one_of};
use nom::character::is_alphabetic;
use nom::combinator::{map, verify};
use nom::IResult;
use nom::multi::{fold_many1, many1};

use crate::parser::Source;

/// An alphanumeric identifier.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Name(pub String);

impl Name {
    pub fn parse(input: Source) -> IResult<Source, Self> {
        map(
            verify(
                fold_many1(
                    alt((alphanumeric1, tag("_"))),
                    String::new(), |a, b| a + b,
                ),
                |s: &str| {
                    let s = s.as_bytes();
                    is_alphabetic(s[0]) || s[0] == b'_'
                }),
            |v| Self(v.to_owned()),
        )(input)
    }
}

/// A symbolic identifier. Symbolic identifiers may be used for function and type names, but
/// not for parameter names or module names.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Operator(pub String);

impl Operator {
    pub fn parse(input: Source) -> IResult<Source, Self> {
        map(
            many1(one_of("~!@$%^&*-+=|:<>?,./")),
            |v| Self(v.into_iter().collect()),
        )(input)
    }
}

/// An identifier.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Identifier {
    Name(Name),
    Operator(Operator),
}

impl Identifier {
    pub fn parse(input: Source) -> IResult<Source, Self> {
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
            ("thing$", Ok(("$", Name("thing".to_owned())))),
            ("abd_123", Ok(("", Name("abd_123".to_owned())))),
            ("12abc", Err(Error(("12abc", ErrorKind::Verify)))),
            ("_12abc", Ok(("", Name("_12abc".to_owned())))),
        ];
        for c in &cases {
            assert_eq!(Name::parse(c.0), c.1);
        }
    }

    #[test]
    fn operators() {
        let cases = [
            ("<*>", Ok(("", Operator("<*>".to_owned())))),
            ("???[", Ok(("[", Operator("???".to_owned())))),
            ("a!!", Err(Error(("a!!", ErrorKind::OneOf)))),
        ];
        for c in &cases {
            assert_eq!(Operator::parse(c.0), c.1);
        }
    }
}
