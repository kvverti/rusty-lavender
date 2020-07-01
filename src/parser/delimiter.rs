use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{line_ending, not_line_ending, space0, space1};
use nom::combinator::{map, opt, value};
use nom::IResult;
use nom::multi::{many0_count, many1_count};
use nom::sequence::{pair, preceded, tuple};

use crate::parser::Source;

/// Lavender uses line endings as item terminators. The `Soft` variant is composed of a single
/// line terminator, and may be present within expressions. The `Hard` variant is composed
/// of multiple line terminators, and always terminates an item.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LineEnd {
    /// A soft line terminator, which only terminates an expression if subsequent tokens would
    /// be malformed.
    Soft,
    /// A hard line terminator, which always terminates an expression.
    Hard,
}

impl LineEnd {
    /// Parses a line ending with optional interleaved token delimiters.
    pub fn parse(input: Source) -> IResult<Source, Self> {
        map(
            many1_count(preceded(token_delimiter, line_ending)),
            |n| if n > 1 { Self::Hard } else { Self::Soft },
        )(input)
    }
}

/// Parses optional internal whitespace. Used during tokenization to discard
/// insignificant whitespace.
pub fn spaces(input: Source) -> IResult<Source, ()> {
    value((), space1)(input)
}

/// Parses a comment. Used during tokenization to discard insignificant whitespace.
///
/// Comments accept an optional line ending before the familiar `# comment` syntax. This is so
/// comments do not contribute to the line terminator structure of the source code.
pub fn comment(input: Source) -> IResult<Source, ()> {
    value((), tuple((opt(pair(line_ending, space0)), tag("#"), not_line_ending)))(input)
}

/// Parses a token delimiter. These are discarded after tokenization.
pub fn token_delimiter(input: Source) -> IResult<Source, ()> {
    value((), many0_count(alt((comment, spaces))))(input)
}

#[cfg(test)]
mod tests {
    use nom::sequence::delimited;

    use super::*;

    #[test]
    fn line_wrap() {
        let parser = delimited(tag("a"), LineEnd::parse, tag("a"));
        let cases = [
            ("a\na1", Ok(("1", LineEnd::Soft))),
            ("a\n\na2", Ok(("2", LineEnd::Hard))),
            ("a \n \na3", Ok(("3", LineEnd::Hard))),
            ("a # comment\na4", Ok(("4", LineEnd::Soft))),
            ("a # comment  \n  # comment\na5", Ok(("5", LineEnd::Soft))),
            ("a\n# comment\na6", Ok(("6", LineEnd::Soft))),
            ("a\n\n# comment\na7", Ok(("7", LineEnd::Hard))),
            ("a # comment\n\na8", Ok(("8", LineEnd::Hard))),
            ("a # comment\n# comment\n\na9", Ok(("9", LineEnd::Hard))),
        ];
        for c in &cases {
            assert_eq!(parser(c.0), c.1);
        }
    }
}
