use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{line_ending, not_line_ending, space1};
use nom::combinator::value;
use nom::IResult;
use nom::multi::{many0, many0_count, many1, many1_count};
use nom::sequence::{pair, preceded, terminated};

use crate::parser::Source;

/// Lavender uses line endings as item terminators. A line ending is included in the token stream
/// if it is not followed by indentation. Further parsing may discard or act on line endings
/// depending on the item being parsed (clearly internal line endings would be dropped).
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct LineEnd();

impl LineEnd {
    /// Parses a line ending with optional interleaved delimiters.
    pub fn parse(input: Source) -> IResult<Source, Self> {
        value(
            LineEnd(),
            many1_count(terminated(line_ending, token_delimiter)),
        )(input)
    }
}

/// Parses a sequence of one or more spaces.
pub fn spaces(input: Source) -> IResult<Source, ()> {
    value((), space1)(input)
}

/// Parses a comment. Used during tokenization to discard insignificant whitespace.
///
/// Comments accept an optional line ending before the familiar `# comment` syntax. This is so
/// comments do not contribute to the line terminator structure of the source code.
pub fn comment(input: Source) -> IResult<Source, ()> {
    value((), pair(tag("#"), not_line_ending))(input)
}

/// Parses a token delimiter. These are discarded after tokenization.
///
/// Token delimiters are composed of a sequence of a (possibly empty) comments, spaces,
/// and new lines, terminated by a non-line-ending.
pub fn token_delimiter(input: Source) -> IResult<Source, ()> {
    value((), many0_count(
        preceded(
            many0(line_ending),
            many1(alt((comment, spaces))),
        ),
    ))(input)
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
            "a # comment\n\ta",
            "a\n\n\n  a",
        ];
        let failures = [
            "a\na",
            "a # comment\na",
            "a \n # comment\na",
        ];
        for c in &successes {
            let result = parser(c);
            assert!(result.is_ok(), format!("Ok case {:?}, result {:?}", c, result));
        }
        for c in &failures {
            let result = parser(c);
            assert!(result.is_err(), format!("Error case {:?}, result {:?}", c, result));
        }
    }

    #[test]
    fn line_wrap() {
        let parser = delimited(tag("a"), LineEnd::parse, tag("a"));
        let cases = [
            "a\na",
            "a\n\na",
            "a\n# comment\na",
            "a\n    \na",
        ];
        for c in &cases {
            let result = parser(c);
            assert!(result.is_ok(), format!("Ok case {:?}, result {:?}", c, result));
        }
    }
}
