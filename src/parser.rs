use nom::{IResult, Offset};

/// The tokenizer.
mod token;

type Source<'a> = &'a str;

fn with_len<I, O, F>(p: F) -> impl Fn(I) -> IResult<I, (usize, O)>
    where F: Fn(I) -> IResult<I, O>,
          I: Clone + Offset
{
    move |input| {
        let (rest, res) = p(input.clone())?;
        let len = input.offset(&rest);
        Ok((rest, (len, res)))
    }
}
