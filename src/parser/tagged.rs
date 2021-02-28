use nom::combinator::map;

use crate::parser::primary::Primary;
use crate::parser::token::TokenStream;
use crate::parser::{with_len, ParseResult};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Tagged<T> {
    pub value: T,
    pub idx: usize,
    pub len: usize,
}

impl<T> Tagged<T> {
    pub fn new(value: T) -> Self {
        Self {
            value,
            idx: 0,
            len: 0,
        }
    }

    pub fn map<U, F>(self, f: F) -> Tagged<U>
    where
        F: FnOnce(T) -> U,
    {
        let Tagged { value, idx, len } = self;
        Tagged {
            value: f(value),
            idx,
            len,
        }
    }

    pub fn as_ref(&self) -> Tagged<&T> {
        Tagged {
            value: &self.value,
            idx: self.idx,
            len: self.len,
        }
    }
}

impl<P: Primary> Primary for Tagged<P> {
    fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        tagged(P::parse)(input)
    }
}

pub fn tagged<'a, F, O>(
    parser: F,
) -> impl Fn(TokenStream<'a>) -> ParseResult<TokenStream<'a>, Tagged<O>>
where
    F: Fn(TokenStream<'a>) -> ParseResult<TokenStream<'a>, O>,
{
    move |input| {
        map(with_len(&parser), |(len, value)| {
            if !input.0.is_empty() {
                Tagged {
                    value,
                    idx: input.0[0].col,
                    len: {
                        let tkn = &input.0[if len == 0 { 0 } else { len - 1 }];
                        let idx = input.0[0].col;
                        (tkn.col - idx) + tkn.len
                    },
                }
            } else {
                Tagged {
                    value,
                    idx: 0,
                    len: 0,
                }
            }
        })(input)
    }
}
