use nom::branch::alt;
use nom::combinator::map;
use nom::IResult;

use crate::parser::{Source, with_len};
use crate::parser::token::delimiter::LineEnd;
use crate::parser::token::fixed::{Keyword, Separator};
use crate::parser::token::identifier::Identifier;
use crate::parser::token::literal::Literal;

/// Parsers for names and operators.
mod identifier;
/// Parsers for separators and keywords.
mod fixed;
/// Token delimiters: whitespace, comments, and new lines.
mod delimiter;
/// Literals: bool, int, float.
mod literal;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenValue {
    Delimiter(LineEnd),
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
    pub fn parse(input: Source) -> IResult<Source, Self> {
        map(
            with_len(alt((
                map(LineEnd::parse, TokenValue::Delimiter),
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
}

#[cfg(test)]
mod tests {
    use nom::multi::many0;
    use nom::sequence::terminated;

    use crate::parser::token::delimiter::token_delimiter;
    use crate::parser::token::identifier::{Name, Operator};
    use crate::parser::token::literal::{BoolLiteral, FloatLiteral, IntLiteral};

    use super::*;

    #[test]
    fn parses() {
        let test_case = "hello&& \t \t class (False 23)  @ 22.0\n# above is madness\n;";
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
            TokenValue::Delimiter(LineEnd::Soft),
            TokenValue::Separator(Separator::Semicolon),
        ];
        let parser = many0(terminated(Token::parse, token_delimiter));
        let result = parser(test_case);
        if let Ok((rest, tokens)) = result {
            let tokens = tokens.into_iter().map(|t| t.value).collect::<Vec<_>>();
            assert_eq!(rest, "");
            assert_eq!(tokens.as_slice(), expected);
        } else {
            panic!(format!("Parsing failed, got {:?}", result));
        }
    }
}
