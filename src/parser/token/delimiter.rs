use nom::bytes::complete::tag;
use nom::character::complete::{line_ending, not_line_ending, space0};
use nom::combinator::{map, opt, value};
use nom::IResult;
use nom::multi::many0;
use nom::sequence::{pair, preceded};

use crate::parser::Source;

/// Parses an absolute indent. An indent is a line terminator, followed by zero or more
/// empty lines, followed by a sequence of whitespace. The returned value is the number
/// of whitespace columns in the indent.
pub fn indent(input: Source) -> IResult<Source, i32> {
    preceded(
        pair(line_ending, many0(empty_line)),
        map(space0, |s: &str| s.len() as i32),
    )(input)
}

/// Parses a completely empty line, containing only spaces and comments.
/// These lines are ignored.
fn empty_line(input: Source) -> IResult<Source, ()> {
    value((), preceded(pair(space0, opt(comment)), line_ending))(input)
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
/// Token delimiters are composed of a sequence of a (possibly empty) comments and spaces,
/// terminated by a non-line-ending.
pub fn token_delimiter(input: Source) -> IResult<Source, ()> {
    value((), pair(space0, opt(comment)))(input)
}

#[cfg(test)]
mod tests {
    use nom::sequence::delimited;

    use super::*;

    #[test]
    fn delimiter() {
        let parser = delimited(tag("a"), token_delimiter, tag("\n"));
        let successes = [
            "a \n",
            "a\n",
            "a\t \t\n",
            "a # comment\n",
        ];
        for c in &successes {
            let result = parser(c);
            assert!(result.is_ok(), format!("Ok case {:?}, result {:?}", c, result));
        }
    }

    #[test]
    fn line_wrap() {
        let parser = delimited(tag("a"), indent, tag("a"));
        let cases = [
            ("a\na", 0),
            ("a\n\n  a", 2),
            ("a\n# comment\n   a", 3),
            ("a\n    \n a", 1),
        ];
        for &(c, v) in &cases {
            let result = parser(c);
            assert!(result.is_ok(), format!("Ok case {:?}, result {:?}", c, result));
            let (rest, result) = result.unwrap();
            assert_eq!(rest, "");
            assert_eq!(result, v);
        }
    }
}
