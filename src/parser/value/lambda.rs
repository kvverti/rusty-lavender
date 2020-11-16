use nom::bytes::complete::tag;
use nom::multi::many1;

use crate::ast::symbol::{AstSymbol, SymbolSpace, ExtractSymbol, SymbolData, SymbolContext};
use crate::parser::{ParseResult, until_next_sync_point};
use crate::parser::pattern::PatternPrimary;
use crate::parser::primary::Primary;
use crate::parser::tagged::Tagged;
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::token::fixed::Keyword;
use crate::parser::token::identifier::{Identifier, Operator};
use crate::parser::value::ValueExpression;

/// A lambda expression (single case anonymous function) `lam a b. c`.
#[derive(Clone, Debug, PartialEq)]
pub enum LambdaExpression {
    /// A lambda expression.
    Value {
        /// The parameter patterns of the lambda expression.
        params: Vec<PatternPrimary>,
        /// The lambda body.
        body: Box<ValueExpression>,
    },
    /// An error that occurred when parsing a lambda expression.
    Error {
        /// The error message and source columns until the next sequence point.
        context: Tagged<&'static str>,
    },
}

/// Matches a parser, or returns an error lambda node if there is a parser error.
macro_rules! next {
    ($ctx:literal, $x:expr, $input:expr) => {
        match $x($input) {
            // parse success
            Ok(v) => v,
            // parse error
            Err(nom::Err::Error(_)) => {
                let (input, context) = until_next_sync_point($ctx, $input);
                return Ok((input, Self::Error {
                    context,
                }));
            }
            // parse failure
            Err(e) => return Err(e),
        }
    };
}

impl LambdaExpression {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        let (input, _) = tag(TokenValue::from(Keyword::Lam))(input)?;
        let (input, params) = next!("Expected pattern", many1(PatternPrimary::parse), input);
        let (input, _) = next!("Expected '.'", tag(TokenValue::from(Identifier::Operator(Operator(".".to_owned())))), input);
        let (input, body) = next!("Expected expression", ValueExpression::parse, input);
        Ok((input, Self::Value {
            params,
            body: Box::new(body),
        }))
    }
}

impl ExtractSymbol for LambdaExpression {
    /// Extract the lambda names and anything in the body
    fn extract(&self, data: &mut SymbolData, ctx: SymbolContext) {
        if let Self::Value { params, body } = self {
            let inner_scope = AstSymbol::in_scope(SymbolSpace::Value, ctx.enclosing_scope, &ctx.scope_idx.to_string());
            let inner_ctx = ctx.with_enclosing_scope(&inner_scope);
            for param in params {
                param.extract(data, inner_ctx);
            }
            body.extract(data, inner_ctx);
        } else {
            unreachable!();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::fixity::{BasicFixity, InfixApply, InfixPrimary, PrefixApply};
    use crate::parser::pattern::Pattern;
    use crate::parser::scoped::ScopedIdentifier;
    use crate::parser::tagged::Tagged;
    use crate::parser::token::fixed::Separator;
    use crate::parser::token::identifier::Name;
    use crate::parser::token::Token;
    use crate::parser::value::ValuePrimary;

    use super::*;

    #[test]
    fn parses() {
        let expected = LambdaExpression::Value {
            params: vec![
                PatternPrimary::Identifier(Tagged::new(ScopedIdentifier::from(Identifier::Name(Name("x".to_owned()))))),
                PatternPrimary::Identifier(Tagged::new(ScopedIdentifier::from(Identifier::Name(Name("y".to_owned()))))),
                PatternPrimary::SubPattern(Box::new(Pattern::Application(BasicFixity::Prefix(PrefixApply {
                    func: PatternPrimary::Identifier(Tagged::new(ScopedIdentifier::from(Identifier::Name(Name("Id".to_owned()))))),
                    args: vec![
                        PatternPrimary::Identifier(Tagged::new(ScopedIdentifier::from(Identifier::Name(Name("z".to_owned()))))),
                    ],
                }))))
            ],
            body: Box::new(ValueExpression::Application(BasicFixity::Infix(InfixApply {
                func: Tagged::new(Identifier::Operator(Operator("+".to_owned()))),
                args: vec![
                    InfixPrimary::Primary(ValuePrimary::Identifier(Tagged::new(ScopedIdentifier::from(Identifier::Name(Name("x".to_owned())))))),
                    InfixPrimary::Primary(ValuePrimary::Identifier(Tagged::new(ScopedIdentifier::from(Identifier::Name(Name("z".to_owned())))))),
                ],
            }))),
        };
        // lam x y (Id z). x + z
        let tokens = [
            Token::new(TokenValue::from(Keyword::Lam)),
            Token::new(TokenValue::from(Identifier::Name(Name("x".to_owned())))),
            Token::new(TokenValue::from(Identifier::Name(Name("y".to_owned())))),
            Token::new(TokenValue::from(Separator::LeftRound)),
            Token::new(TokenValue::from(Identifier::Name(Name("Id".to_owned())))),
            Token::new(TokenValue::from(Identifier::Name(Name("z".to_owned())))),
            Token::new(TokenValue::from(Separator::RightRound)),
            Token::new(TokenValue::from(Identifier::Operator(Operator(".".to_owned())))),
            Token::new(TokenValue::from(Identifier::Name(Name("x".to_owned())))),
            Token::new(TokenValue::from(Identifier::Operator(Operator("+".to_owned())))),
            Token::new(TokenValue::from(Identifier::Name(Name("z".to_owned())))),
        ];
        let result = LambdaExpression::parse(TokenStream(&tokens));
        assert!(result.is_ok(), "Expected ok result, got {:?}", result);
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }
}
