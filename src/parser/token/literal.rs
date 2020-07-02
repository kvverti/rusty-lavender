use std::str::FromStr;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::digit1;
use nom::combinator::{map, map_res, value};
use nom::error::context;
use nom::IResult;
use nom::number::complete::double;

use crate::parser::{Source, with_input};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct IntLiteral<'a> {
    pub src: Source<'a>,
    pub val: i64,
}

impl<'a> IntLiteral<'a> {
    pub fn parse(input: Source<'a>) -> IResult<Source<'a>, Self> {
        context(
            "Tokenizing integer",
            map_res(
                with_input(digit1),
                |(src, digits)| {
                    i64::from_str(digits).map(|val| Self { src, val })
                },
            ),
        )(input)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct FloatLiteral<'a> {
    pub src: Source<'a>,
    pub val: f64,
}

impl<'a> FloatLiteral<'a> {
    pub fn parse(input: Source<'a>) -> IResult<Source<'a>, Self> {
        context(
            "Tokenizing float",
            map(
                with_input(double),
                |(src, val)| {
                    Self { src, val }
                },
            ),
        )(input)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct BoolLiteral<'a> {
    pub src: Source<'a>,
    pub val: bool,
}

impl<'a> BoolLiteral<'a> {
    pub fn parse(input: Source<'a>) -> IResult<Source<'a>, Self> {
        context(
            "Tokenizing bool",
            map(
                with_input(alt((
                    value(true, tag("True")),
                    value(false, tag("False")),
                ))),
                |(src, val)| {
                    Self { src, val }
                },
            ),
        )(input)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Literal<'a> {
    Bool(BoolLiteral<'a>),
    Int(IntLiteral<'a>),
    Float(FloatLiteral<'a>),
}

impl<'a> Literal<'a> {
    pub fn parse(input: Source<'a>) -> IResult<Source<'a>, Literal<'a>> {
        alt((
            map(BoolLiteral::parse, Literal::Bool),
            map(IntLiteral::parse, Literal::Int),
            map(FloatLiteral::parse, Literal::Float),
        ))(input)
    }
}

#[cfg(test)]
mod tests {
    use nom::Err::Error;
    use nom::error::ErrorKind;

    use super::*;

    #[test]
    fn booleans() {
        let cases = [
            ("True", Ok(("", BoolLiteral { src: "True", val: true }))),
            ("False", Ok(("", BoolLiteral { src: "False", val: false }))),
            ("True;", Ok((";", BoolLiteral { src: "True", val: true }))),
            ("False \n", Ok((" \n", BoolLiteral { src: "False", val: false }))),
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
            ("0;", Ok((";", IntLiteral { src: "0", val: 0 }))),
            ("47", Ok(("", IntLiteral { src: "47", val: 47 }))),
            ("10000000000 +", Ok((" +", IntLiteral { src: "10000000000", val: 10000000000 }))),
            (
                "999999999999999999999999999999999999999;",
                Err(Error(("999999999999999999999999999999999999999;", ErrorKind::MapRes)))
            ),
            ("1hhh", Ok(("hhh", IntLiteral { src: "1", val: 1 }))),
            ("three", Err(Error(("three", ErrorKind::Digit)))),
        ];
        for c in &cases {
            assert_eq!(IntLiteral::parse(c.0), c.1);
        }
    }

    #[test]
    fn floats() {
        let cases = [
            ("0.0", Ok(("", FloatLiteral { src: "0.0", val: 0.0 }))),
            ("12;", Ok((";", FloatLiteral { src: "12", val: 12.0 }))),
            ("0.47", Ok(("", FloatLiteral { src: "0.47", val: 0.47 }))),
            ("1e-30", Ok(("", FloatLiteral { src: "1e-30", val: 1e-30 }))),
            ("e", Err(Error(("e", ErrorKind::Float)))),
        ];
        for c in &cases {
            assert_eq!(FloatLiteral::parse(c.0), c.1);
        }
    }
}
