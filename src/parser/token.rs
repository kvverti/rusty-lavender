use nom::branch::alt;
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
    Indent(i32),
    Literal(Literal),
    Keyword(Keyword),
    Separator(Separator),
    Identifier(Identifier),
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
                map(delimiter::indent, TokenValue::Indent),
                map(Literal::parse, TokenValue::Literal),
                map(Keyword::parse, TokenValue::Keyword),
                map(Separator::parse, TokenValue::Separator),
                map(Identifier::parse, TokenValue::Identifier),
            ))),
            |(len, res)| Self {
                value: res,
                col: 0,
                len,
            },
        )(input)
    }

    /// Parses a sequence of tokens, optionally delimited by token delimiters.
    pub fn parse_sequence(mut input: Source) -> IResult<Source, Vec<Self>> {
        let mut col = 0;
        let mut vec = Vec::new();
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
        Ok((input, vec))
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
            TokenValue::Indent(2),
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
            f("\n# above is madness\n  "),
            f(";"),
        ];
        let result = Token::parse_sequence(test_case);
        if let Ok((rest, tokens)) = result {
            let columns = tokens.iter().map(|t| (t.col, t.len)).collect::<Vec<_>>();
            let values = tokens.into_iter().map(|t| t.value).collect::<Vec<_>>();
            assert_eq!(rest, "");
            assert_eq!(values.as_slice(), expected);
            assert_eq!(columns, expected_columns);
        } else {
            panic!(format!("Parsing failed, got {:?}", result));
        }
    }
}
