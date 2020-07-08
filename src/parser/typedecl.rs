use crate::parser::fixity::{InfixApply, PrefixApply};
use crate::parser::ParseResult;
use crate::parser::primary::Primary;
use crate::parser::token::identifier::{Identifier, Name};
use crate::parser::token::TokenStream;

pub enum TypePrimary {
    TypeIdentifier(Identifier),
    TypeVariable(Name),
    TypeSubExpression(Box<TypeExpression>),
}

impl Primary for TypePrimary {
    fn parse(_input: TokenStream) -> ParseResult<TokenStream, Self> {
        unimplemented!()
    }
}

pub enum TypeExpression {
    TypePrimary(TypePrimary),
    TypeApplication(PrefixApply<TypePrimary>),
    InfixTypeApplication(InfixApply<TypePrimary>),
}
