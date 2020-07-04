use crate::parser::token::identifier::Identifier;
use crate::parser::expression::ExpressionNode;
use crate::parser::token::TokenStream;
use nom::IResult;

/// Infix function application.
#[derive(Clone, Debug, PartialEq)]
pub struct InfixApply {
    pub func: Identifier,
    pub arg1: ExpressionNode,
    pub arg2: ExpressionNode,
}

impl InfixApply {
    pub fn parse(_input: TokenStream) -> IResult<TokenStream, Self> {
        unimplemented!()
    }
}