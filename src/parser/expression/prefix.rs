use crate::parser::expression::ExpressionNode;
use crate::parser::token::TokenStream;
use nom::IResult;

/// Prefix function application.
#[derive(Clone, Debug, PartialEq)]
pub struct PrefixApply {
    pub func: ExpressionNode,
    pub arg: ExpressionNode,
}

impl PrefixApply {
    pub fn parse(_input: TokenStream) -> IResult<TokenStream, Self> {
        unimplemented!()
    }
}