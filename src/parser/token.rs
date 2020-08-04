use nom::branch::alt;
use nom::character::complete::anychar;
use nom::combinator::map;
use nom::IResult;

use crate::parser::{Source, with_len};
use crate::parser::token::delimiter::token_delimiter;
use crate::parser::token::fixed::{Keyword, Separator};
use crate::parser::token::identifier::Identifier;
use crate::parser::token::literal::Literal;

/// Parsers for names and operators.
pub mod identifier;
/// Parsers for separators and keywords.
pub mod fixed;
/// Token delimiters: whitespace, comments, and new lines.
pub mod delimiter;
/// Literals: bool, int, float.
pub mod literal;
/// Token stream trait implementations.
mod stream;
/// Token value trait implementations.
mod value;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct TokenStream<'a>(pub &'a [Token]);

#[derive(Clone, Debug, PartialEq)]
pub enum TokenValue {
    Literal(Literal),
    Keyword(Keyword),
    Separator(Separator),
    Identifier(Identifier),
    Unrecognized(char),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub value: TokenValue,
    pub col: usize,
    pub len: usize,
}

impl Token {
    pub fn new(value: TokenValue) -> Self {
        Self { value, col: 0, len: 0 }
    }

    /// Parses a single token.
    pub fn parse(input: Source) -> IResult<Source, Self> {
        map(
            with_len(alt((
                map(Identifier::parse, TokenValue::Identifier),
                map(Literal::parse, TokenValue::Literal),
                map(Keyword::parse, TokenValue::Keyword),
                map(Separator::parse, TokenValue::Separator),
                map(anychar, TokenValue::Unrecognized),
            ))),
            |(len, res)| Self {
                value: res,
                col: 0,
                len,
            },
        )(input)
    }

    /// Parses a sequence of tokens, optionally delimited by token delimiters.
    pub fn parse_sequence(mut input: Source) -> Vec<Self> {
        let mut col = 0;
        let mut vec = Vec::new();
        // parse any leading token delimiter
        if let Ok((rest, (len, _))) = with_len(token_delimiter)(input) {
            input = rest;
            col += len;
        }
        // parse a token and set its column
        while let Ok((rest, mut token)) = Self::parse(input) {
            input = rest;
            token.col = col;
            col += token.len;
            vec.push(token);
            if let Ok((rest, (len, _))) = with_len(token_delimiter)(input) {
                input = rest;
                col += len;
            }
        }
        vec
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::token::identifier::{Name, Operator};
    use crate::parser::token::literal::{BoolLiteral, FloatLiteral, IntLiteral};

    use super::*;

    #[test]
    fn parses() {
        let test_case = "hello&& \t \t class (False 23)  @ 22.0\n# above is madness\n  ;";
        let expected = [
            TokenValue::Identifier(Identifier::Name(Name("hello".to_owned()))),
            TokenValue::Identifier(Identifier::Operator(Operator("&&".to_owned()))),
            TokenValue::Keyword(Keyword::Class),
            TokenValue::Separator(Separator::LeftRound),
            TokenValue::Literal(Literal::Bool(BoolLiteral(false))),
            TokenValue::Literal(Literal::Int(IntLiteral(23))),
            TokenValue::Separator(Separator::RightRound),
            TokenValue::Identifier(Identifier::Operator(Operator("@".to_owned()))),
            TokenValue::Literal(Literal::Float(FloatLiteral(22.0))),
            TokenValue::Separator(Separator::Semicolon),
        ];
        let f = |s: &str| (test_case.rfind(s).unwrap(), s.len());
        let expected_columns = [
            f("hello"),
            f("&&"),
            f("class"),
            f("("),
            f("False"),
            f("23"),
            f(")"),
            f("@"),
            f("22.0"),
            f(";"),
        ];
        let tokens = Token::parse_sequence(test_case);
        let columns = tokens.iter().map(|t| (t.col, t.len)).collect::<Vec<_>>();
        let values = tokens.into_iter().map(|t| t.value).collect::<Vec<_>>();
        assert_eq!(values.as_slice(), expected);
        assert_eq!(columns, expected_columns);
    }

    #[test]
    fn keywords() {
        let expected = [
            TokenValue::Keyword(Keyword::Def),
            TokenValue::Keyword(Keyword::Type),
            TokenValue::Identifier(Identifier::Name(Name("types".to_owned()))),
            TokenValue::Identifier(Identifier::Operator(Operator("<=>".to_owned()))),
            TokenValue::Separator(Separator::FatArrow),
            TokenValue::Identifier(Identifier::Operator(Operator("=>>".to_owned()))),
            TokenValue::Identifier(Identifier::Name(Name("simple".to_owned()))),
            TokenValue::Keyword(Keyword::Impl),
            TokenValue::Literal(Literal::Bool(BoolLiteral(true))),
            TokenValue::Identifier(Identifier::Name(Name("unTrue".to_owned()))),
            TokenValue::Identifier(Identifier::Name(Name("Trues".to_owned()))),
            TokenValue::Identifier(Identifier::Name(Name("_a".to_owned()))),
            TokenValue::Keyword(Keyword::Underscore),
        ];
        let case = "def type types <=> => =>> simple impl True unTrue Trues _a _";
        let result = Token::parse_sequence(case);
        let result = result.into_iter().map(|t| t.value).collect::<Vec<_>>();
        assert_eq!(result.as_slice(), &expected);
    }
}
