use nom::combinator::map;

use crate::parser::{ParseResult, with_len};
use crate::parser::token::TokenStream;

#[derive(Clone, Debug, PartialEq)]
pub struct Tagged<T> {
    pub value: T,
    pub idx: usize,
    pub len: usize,
}

impl<T> Tagged<T> {
    pub fn new(value: T) -> Self {
        Self { value, idx: 0, len: 0 }
    }
}

pub fn tagged<F, O>(parser: F) -> impl Fn(TokenStream) -> ParseResult<TokenStream, Tagged<O>>
    where F: Fn(TokenStream) -> ParseResult<TokenStream, O>
{
    move |input| {
        map(with_len(&parser), |(len, value)| Tagged {
            value,
            idx: input.0[0].col,
            len: {
                let tkn = &input.0[len - 1];
                let idx = input.0[0].col;
                (tkn.col - idx) + tkn.len
            },
        })(input)
    }
}