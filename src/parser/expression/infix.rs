use crate::parser::expression::primary::Primary;
use crate::parser::ParseResult;
use crate::parser::token::identifier::Identifier;
use crate::parser::token::TokenStream;

/// Infix function application.
#[derive(Clone, Debug, PartialEq)]
pub struct InfixApply {
    /// The name of the function.
    pub func: Identifier,
    /// The arguments. At least two.
    pub args: Vec<Primary>,
}

impl InfixApply {
    pub fn parse(_input: TokenStream) -> ParseResult<TokenStream, Self> {
        unimplemented!()
    }
}