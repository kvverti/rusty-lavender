use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::value;
use nom::IResult;

use crate::parser::Source;

/// A literal separator. The difference between separators and keywords is somewhat arbitrary,
/// but separators generally separate or group regions of source code. Tokens that may be used
/// as either "separators" or operators (such as `:`) are classified as operators.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Separator {
    LeftRound,
    RightRound,
    LeftCurly,
    RightCurly,
    LeftSquare,
    RightSquare,
    Semicolon,
    FatArrow,
    BackTick,
    Check,
    Colon,
    DoubleColon,
}

impl Separator {
    pub fn parse(input: Source) -> IResult<Source, Self> {
        alt((
            value(Self::LeftRound, tag("(")),
            value(Self::RightRound, tag(")")),
            value(Self::LeftCurly, tag("{")),
            value(Self::RightCurly, tag("}")),
            value(Self::LeftSquare, tag("[")),
            value(Self::RightSquare, tag("]")),
            value(Self::Semicolon, tag(";")),
            value(Self::FatArrow, tag("=>")),
            value(Self::BackTick, tag("`")),
            value(Self::Check, tag("'")),
            value(Self::DoubleColon, tag("::")),
            value(Self::Colon, tag(":")),
        ))(input)
    }
}

/// A Lavender keyword. The difference between separators and keywords is somewhat arbitrary,
/// but keywords generally precede or are part of a single source construct.
///
/// Note that the built-in types `Int`, `Float`, and `Bool`, and the built-in type constructor
/// `->` are not keywords, but are instead globally defined names. The values `True` and `False`
/// are also not keywords; they are literals.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Keyword {
    /// Value definition `def x y => z`.
    Def,
    /// Type alias `type A => B`.
    Type,
    /// Type class definition `class F a { }`.
    Class,
    /// Type class implementation `impl F A { }`.
    Impl,
    /// Data definition `data A => A1 B | A2 C`.
    Data,
    /// Lambda expression `for a b. c`.
    For,
    /// Unbound value `f _`.
    Underscore,
}

impl Keyword {
    pub fn parse(input: Source) -> IResult<Source, Self> {
        alt((
            value(Self::Def, tag("def")),
            value(Self::Type, tag("type")),
            value(Self::Class, tag("class")),
            value(Self::Impl, tag("impl")),
            value(Self::Data, tag("data")),
            value(Self::For, tag("for")),
            value(Self::Underscore, tag("_")),
        ))(input)
    }
}

/// Tests whether a string represents either a keyword or a separator.
/// This function returns true if and only if the entire input represents a keyword or
/// separator; for example
///
/// ```
/// assert!(is_keyword_or_separator("=>"));
/// assert!(is_keyword_or_separator("impl"));
///
/// assert!(!is_keyword_or_separator("=>>"));
/// assert!(!is_keyword_or_separator("impls"));
/// assert!(!is_keyword_or_separator("simple"));
/// ```
pub fn is_keyword_or_separator(input: Source) -> bool {
    let result = alt((value((), Separator::parse), value((), Keyword::parse)))(input);
    if let Ok((rest, _)) = result {
        // the entire input is a keyword if and only if the remainder is empty
        rest.is_empty()
    } else {
        // error -> not a keyword
        false
    }
}
