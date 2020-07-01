use std::ops::RangeTo;

use nom::{IResult, Offset, Slice};

/// Parsers for names and operators.
mod identifier;
/// Parsers for separators and keywords.
mod fixed;
/// Token delimiters: whitespace, comments, and new lines.
mod delimiter;
/// Literals: bool, int, float.
mod literal;

type Source<'a> = &'a str;

fn with_input<I, O, F>(p: F) -> impl Fn(I) -> IResult<I, (I, O)>
    where F: Fn(I) -> IResult<I, O>,
          I: Clone + Offset + Slice<RangeTo<usize>>
{
    move |input| {
        let (rest, res) = p(input.clone())?;
        let len = input.offset(&rest);
        Ok((rest, (input.slice(..len), res)))
    }
}
