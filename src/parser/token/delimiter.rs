use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{line_ending, not_line_ending, space1};
use nom::combinator::value;
use nom::IResult;
use nom::multi::fold_many0;
use nom::sequence::pair;

use crate::parser::Source;

/// Parses a comment. Used during tokenization to discard insignificant whitespace.
///
/// Comments accept an optional line ending before the familiar `# comment` syntax. This is so
/// comments do not contribute to the line terminator structure of the source code.
pub fn comment(input: Source) -> IResult<Source, ()> {
    value((), pair(tag("#"), not_line_ending))(input)
}

/// Parses a token delimiter. These are discarded after tokenization.
///
/// Token delimiters are composed of a sequence of a (possibly empty) comments and spaces, and
/// line endings.
pub fn token_delimiter(input: Source) -> IResult<Source, ()> {
    fold_many0(
        alt((
            value((), space1),
            value((), line_ending),
            comment,
        )),
        (),
        |_, _| (),
    )(input)
}

#[cfg(test)]
mod tests {
    use nom::sequence::delimited;

    use super::*;

    #[test]
    fn delimiter() {
        let parser = delimited(tag("a"), token_delimiter, tag("a"));
        let successes = [
            "a a",
            "aa",
            "a\t \ta",
            "a # comment\na",
            "a\n\n   \n # comment \n\n \t\t a",
        ];
        for c in &successes {
            let result = parser(c);
            assert!(result.is_ok(), format!("Ok case {:?}, result {:?}", c, result));
        }
    }
}
