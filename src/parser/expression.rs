use crate::parser::expression::infix::InfixApply;
use crate::parser::expression::prefix::PrefixApply;
use crate::parser::expression::primary::Primary;
use crate::parser::ParseResult;
use crate::parser::token::TokenStream;

pub mod prefix;
pub mod infix;
pub mod primary;

/// The types of Lavender expressions.
#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionNode {
    Primary(Box<Primary>),
    /// Function application `a b`
    Application(Box<PrefixApply>),
    /// Infix function application `a @ b`
    InfixApplication(Box<InfixApply>),
}

impl ExpressionNode {
    pub fn parse(_input: TokenStream) -> ParseResult<TokenStream, Self> {
        unimplemented!()
    }
}
