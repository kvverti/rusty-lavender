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
    Def,
    Type,
    Class,
    Impl,
    Underscore,
    Ellipsis,
}

impl Keyword {
    pub fn parse(input: Source) -> IResult<Source, Self> {
        alt((
            value(Self::Def, tag("def")),
            value(Self::Type, tag("type")),
            value(Self::Class, tag("class")),
            value(Self::Impl, tag("impl")),
            value(Self::Underscore, tag("_")),
            value(Self::Ellipsis, tag("...")),
        ))(input)
    }
}