use crate::parser::token::TokenStream;
use crate::parser::{ParseResult, with_len};
use nom::combinator::map;

#[derive(Clone, Debug, PartialEq)]
pub struct Tagged<T> {
    pub value: T,
    pub idx: usize,
    pub len: usize,
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
                tkn.col + tkn.len
            },
        })(input)
    }
}