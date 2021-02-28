use std::str::FromStr;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::digit1;
use nom::combinator::{map, map_res, value, verify};
use nom::error::context;
use nom::number::complete::recognize_float;
use nom::IResult;

use crate::parser::Source;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct IntLiteral(pub i64);

impl IntLiteral {
    pub fn parse(input: Source) -> IResult<Source, Self> {
        context(
            "Tokenizing integer",
            map_res(digit1, |digits| i64::from_str(digits).map(Self)),
        )(input)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct FloatLiteral(pub f64);

impl FloatLiteral {
    pub fn parse(input: Source) -> IResult<Source, Self> {
        context(
            "Tokenizing float",
            map_res(
                verify(
                    recognize_float,
                    // ensure that we don't match the integer pattern
                    |s: &str| s.contains(|c: char| !c.is_ascii_digit()),
                ),
                |s| f64::from_str(s).map(Self),
            ),
        )(input)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct BoolLiteral(pub bool);

impl BoolLiteral {
    pub fn parse(input: Source) -> IResult<Source, Self> {
        context(
            "Tokenizing bool",
            map(
                alt((value(true, tag("True")), value(false, tag("False")))),
                Self,
            ),
        )(input)
    }
}

/// Whether the entire input represents a bool literal.
///
/// ```
/// assert!(is_bool_literal("True"));
/// assert!(!is_bool_literal("Trues"));
/// ```
pub fn is_bool_literal(input: Source) -> bool {
    let result = BoolLiteral::parse(input);
    if let Ok((rest, _)) = result {
        // entire input is a bool literal iff there is no remainder
        rest.is_empty()
    } else {
        // error -> not a literal
        false
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Literal {
    Bool(BoolLiteral),
    Int(IntLiteral),
    Float(FloatLiteral),
}

impl Literal {
    pub fn parse(input: Source) -> IResult<Source, Literal> {
        alt((
            map(BoolLiteral::parse, Literal::Bool),
            map(FloatLiteral::parse, Literal::Float),
            map(IntLiteral::parse, Literal::Int),
        ))(input)
    }
}

#[cfg(test)]
mod tests {
    use nom::error::ErrorKind;
    use nom::Err::Error;

    use super::*;

    #[test]
    fn booleans() {
        let cases = [
            ("True", Ok(("", BoolLiteral(true)))),
            ("False", Ok(("", BoolLiteral(false)))),
            ("True;", Ok((";", BoolLiteral(true)))),
            ("False \n", Ok((" \n", BoolLiteral(false)))),
            ("true", Err(Error(("true", ErrorKind::Tag)))),
            ("Fals", Err(Error(("Fals", ErrorKind::Tag)))),
        ];
        for c in &cases {
            assert_eq!(BoolLiteral::parse(c.0), c.1);
        }
    }

    #[test]
    fn ints() {
        let cases = [
            ("0;", Ok((";", IntLiteral(0)))),
            ("47", Ok(("", IntLiteral(47)))),
            ("10000000000 +", Ok((" +", IntLiteral(10000000000)))),
            (
                "999999999999999999999999999999999999999;",
                Err(Error((
                    "999999999999999999999999999999999999999;",
                    ErrorKind::MapRes,
                ))),
            ),
            ("1hhh", Ok(("hhh", IntLiteral(1)))),
            ("three", Err(Error(("three", ErrorKind::Digit)))),
        ];
        for c in &cases {
            assert_eq!(IntLiteral::parse(c.0), c.1);
        }
    }

    #[test]
    fn floats() {
        let cases = [
            ("0.0", Ok(("", FloatLiteral(0.0)))),
            ("12.0;", Ok((";", FloatLiteral(12.0)))),
            ("12;", Err(Error(("12;", ErrorKind::Verify)))),
            ("0.47", Ok(("", FloatLiteral(0.47)))),
            ("1e-30", Ok(("", FloatLiteral(1e-30)))),
            ("e", Err(Error(("e", ErrorKind::Char)))),
        ];
        for c in &cases {
            assert_eq!(FloatLiteral::parse(c.0), c.1);
        }
    }
}
