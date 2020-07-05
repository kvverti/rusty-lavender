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
    Primary(Primary),
    /// Function application `a b`
    Application(PrefixApply),
    /// Infix function application `a @ b`
    InfixApplication(InfixApply),
}

impl ExpressionNode {
    pub fn parse(_input: TokenStream) -> ParseResult<TokenStream, Self> {
        unimplemented!()
    }
}

impl From<Primary> for ExpressionNode {
    fn from(v: Primary) -> Self {
        ExpressionNode::Primary(v)
    }
}

impl From<PrefixApply> for ExpressionNode {
    fn from(v: PrefixApply) -> Self {
        ExpressionNode::Application(v)
    }
}

impl From<InfixApply> for ExpressionNode {
    fn from(v: InfixApply) -> Self {
        ExpressionNode::InfixApplication(v)
    }
}
