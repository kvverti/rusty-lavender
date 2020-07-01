use nom::{IResult, Offset};

/// Parsers for separators and keywords.
mod fixed;
/// Token delimiters: whitespace, comments, and new lines.
mod delimiter;
/// Literals: bool, int, float.
mod literal;

type Source<'a> = &'a str;

fn with_input<'a, O, F>(p: F) -> impl Fn(Source<'a>) -> IResult<Source<'a>, (Source<'a>, O)>
    where F: Fn(Source<'a>) -> IResult<Source<'a>, O>,
{
    move |input| {
        let (rest, res) = p(input)?;
        let len = input.offset(rest);
        Ok((rest, (&input[..len], res)))
    }
}
