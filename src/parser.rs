use nom::bytes::complete::take_till;
use nom::error::{ParseError, VerboseError};
use nom::{IResult, Offset};

use crate::parser::tagged::Tagged;
use crate::parser::token::{TokenStream, TokenValue};

/// Generic parsers for prefix and infix juxtaposition.
pub(crate) mod fixity;
/// Item parsers.
pub mod item;
/// Pattern parsers.
pub(crate) mod pattern;
/// Generic parsers for primary nodes.
pub(crate) mod primary;
/// Scoped identifier parser.
pub(crate) mod scoped;
/// Stores tag info like source location in the parse tree.
pub mod tagged;
/// The tokenizer.
pub mod token;
/// Explicit type declarations.
pub(crate) mod typedecl;
/// Value expression parsers.
pub(crate) mod value;

pub type Source<'a> = &'a str;
pub type ParseResult<I, O> = IResult<I, O, VerboseError<I>>;

fn with_len<I, O, E, F>(p: F) -> impl Fn(I) -> IResult<I, (usize, O), E>
where
    F: Fn(I) -> IResult<I, O, E>,
    I: Clone + Offset,
    E: ParseError<I>,
{
    move |input| {
        let (rest, res) = p(input.clone())?;
        let len = input.offset(&rest);
        Ok((rest, (len, res)))
    }
}

/// Consumes input until a separator or a keyword is reached.
fn until_next_sync_point<'a>(
    ctx: &'static str,
    input: TokenStream<'a>,
) -> (TokenStream<'a>, Tagged<&'static str>) {
    tagged::tagged(nom::combinator::value(
        ctx,
        take_till(TokenValue::is_keyword_or_separator),
    ))(input)
    .expect("take_till returned error")
}
