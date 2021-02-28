use crate::parser::token::fixed::{Keyword, Separator};
use crate::parser::token::identifier::Identifier;
use crate::parser::token::literal::Literal;
use crate::parser::token::TokenValue;
use nom::InputLength;

impl From<Literal> for TokenValue {
    #[inline]
    fn from(v: Literal) -> Self {
        TokenValue::Literal(v)
    }
}

impl From<Keyword> for TokenValue {
    #[inline]
    fn from(v: Keyword) -> Self {
        TokenValue::Keyword(v)
    }
}

impl From<Separator> for TokenValue {
    #[inline]
    fn from(v: Separator) -> Self {
        TokenValue::Separator(v)
    }
}

impl From<Identifier> for TokenValue {
    #[inline]
    fn from(v: Identifier) -> Self {
        TokenValue::Identifier(v)
    }
}

impl InputLength for TokenValue {
    fn input_len(&self) -> usize {
        1
    }
}
