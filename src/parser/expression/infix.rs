use crate::parser::expression::primary::Primary;
use crate::parser::ParseResult;
use crate::parser::token::identifier::Identifier;
use crate::parser::token::TokenStream;

/// Infix function application.
#[derive(Clone, Debug, PartialEq)]
pub struct InfixApply {
    pub func: Identifier,
    pub arg1: Primary,
    pub arg2: Primary,
}

impl InfixApply {
    pub fn parse(_input: TokenStream) -> ParseResult<TokenStream, Self> {
        unimplemented!()
    }
}