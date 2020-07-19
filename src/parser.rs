use nom::{IResult, Offset};
use nom::error::{ParseError, VerboseError};

/// Generic parsers for prefix and infix juxtaposition.
mod fixity;
/// Generic parsers for primary nodes.
mod primary;
/// Scoped identifier parser.
mod scoped;
/// The tokenizer.
mod token;
/// Explicit type declarations.
mod typedecl;
/// Value expression parsers.
mod value;

type Source<'a> = &'a str;
type ParseResult<I, O> = IResult<I, O, VerboseError<I>>;

fn with_len<I, O, E, F>(p: F) -> impl Fn(I) -> IResult<I, (usize, O), E>
    where F: Fn(I) -> IResult<I, O, E>,
          I: Clone + Offset,
          E: ParseError<I>,
{
    move |input| {
        let (rest, res) = p(input.clone())?;
        let len = input.offset(&rest);
        Ok((rest, (len, res)))
    }
}
