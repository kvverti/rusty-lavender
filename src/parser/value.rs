use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::sequence::delimited;

use crate::ast::{Extract, SemanticContext, SemanticData};
use crate::ast::symbol::{AstSymbol, SymbolSpace};
use crate::parser::fixity::{BasicFixity, InfixNamespace};
use crate::parser::ParseResult;
use crate::parser::primary::{literal, Primary};
use crate::parser::scoped::ScopedIdentifier;
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::token::fixed::Separator;
use crate::parser::token::literal::Literal;
use crate::parser::value::lambda::LambdaExpression;

/// Lambda expression parsers.
pub mod lambda;

/// A fundamental, "atomic" value expression.
#[derive(Clone, Debug, PartialEq)]
pub enum ValuePrimary {
    /// A literal token `a` (such a `True` or `3`)
    Literal(Literal),
    /// A name `a` (such as `map` or `(.)`)
    Identifier(ScopedIdentifier),
    /// A parenthesized expression `( a )`
    SubExpression(Box<ValueExpression>),
}

impl ValuePrimary {
    /// Parses a parenthesized subexpression.
    fn parse_subexpr(input: TokenStream) -> ParseResult<TokenStream, ValueExpression> {
        delimited(
            tag(TokenValue::from(Separator::LeftRound)),
            ValueExpression::parse,
            tag(TokenValue::from(Separator::RightRound)),
        )(input)
    }
}

impl Primary for ValuePrimary {
    fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        alt((
            map(literal, Self::Literal),
            map(ScopedIdentifier::parse, Self::Identifier),
            map(Self::parse_subexpr, |e| Self::SubExpression(Box::new(e))),
        ))(input)
    }
}

impl InfixNamespace for ValuePrimary {
    const NAMESPACE: SymbolSpace = SymbolSpace::Value;
}

impl Extract for ValuePrimary {
    fn extract(&self, data: &mut SemanticData, ctx: &SemanticContext) {
        match self {
            // literals declare no symbols
            Self::Literal(_) => {}
            // scoped IDs in value expressions are unbound symbols
            Self::Identifier(id) => {
                let symbol = AstSymbol::from_scopes(SymbolSpace::Value, &id.to_scopes());
                data.declare_unbound_symbol(ctx.enclosing_scope.clone(), symbol);
            }
            // subexpressions pass through symbols
            Self::SubExpression(expr) => expr.extract(data, ctx),
        }
    }
}

/// The types of Lavender expressions.
#[derive(Clone, Debug, PartialEq)]
pub enum ValueExpression {
    /// Primary, and prefix and infix function application.
    Application(BasicFixity<ValuePrimary>),
    /// Lambda expression `lam a b. c`
    Lambda(LambdaExpression),
}

impl ValueExpression {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        alt((
            map(LambdaExpression::parse, Self::Lambda),
            map(BasicFixity::parse, Self::Application),
        ))(input)
    }
}

impl Extract for ValueExpression {
    fn extract(&self, data: &mut SemanticData, ctx: &SemanticContext) {
        match self {
            Self::Application(expr) => expr.extract(data, ctx),
            Self::Lambda(lambda) => lambda.extract(data, ctx),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::fixity::{InfixApply, InfixPrimary, PrefixApply};
    use crate::parser::pattern::PatternPrimary;
    use crate::parser::tagged::Tagged;
    use crate::parser::token::{Token, TokenValue};
    use crate::parser::token::fixed::Separator;
    use crate::parser::token::identifier::{Identifier, Name, Operator};
    use crate::parser::token::literal::{IntLiteral, Literal};

    use super::*;
    use nom::lib::std::collections::HashSet;

    #[test]
    fn parses() {
        let expected = ValueExpression::Application(BasicFixity::Infix(InfixApply {
            func: Tagged::new(Identifier::Operator(Operator("+".to_owned()))),
            args: vec![
                InfixPrimary::Primary(ValuePrimary::Literal(Literal::Int(IntLiteral(1)))),
                InfixPrimary::Application(PrefixApply {
                    func: ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("f".to_owned())))),
                    args: vec![ValuePrimary::Literal(Literal::Int(IntLiteral(2)))],
                }),
                InfixPrimary::Primary(ValuePrimary::SubExpression(Box::new(
                    ValueExpression::Application(BasicFixity::Infix(InfixApply {
                        func: Tagged::new(Identifier::Operator(Operator("*".to_owned()))),
                        args: vec![
                            InfixPrimary::Primary(ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned()))))),
                            InfixPrimary::Primary(ValuePrimary::Literal(Literal::Int(IntLiteral(3)))),
                        ],
                    }))
                ))),
            ],
        }));
        let expr = [
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(1)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator("+".to_owned())))),
            Token::new(TokenValue::Identifier(Identifier::Name(Name("f".to_owned())))),
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(2)))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator("+".to_owned())))),
            Token::new(TokenValue::Separator(Separator::LeftRound)),
            Token::new(TokenValue::Identifier(Identifier::Name(Name("a".to_owned())))),
            Token::new(TokenValue::Identifier(Identifier::Operator(Operator("*".to_owned())))),
            Token::new(TokenValue::Literal(Literal::Int(IntLiteral(3)))),
            Token::new(TokenValue::Separator(Separator::RightRound)),
        ];
        let input = "1 + f 2 + (a * 3)";
        let mut tokens = Token::parse_sequence(input);
        for t in &mut tokens {
            t.len = 0;
            t.col = 0;
        }
        assert_eq!(&tokens, &expr);
        let result = ValueExpression::parse(TokenStream(&tokens));
        assert!(result.is_ok(), format!("Expected ok expression parse, got {:?}", result));
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn expr_with_lambda() {
        let input = "a + (lam f. f a)";
        let expected = ValueExpression::Application(BasicFixity::Infix(InfixApply {
            func: Tagged {
                value: Identifier::Operator(Operator("+".to_owned())),
                idx: input.find('+').unwrap(),
                len: 1,
            },
            args: vec![
                InfixPrimary::Primary(ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned()))))),
                InfixPrimary::Primary(ValuePrimary::SubExpression(Box::new(ValueExpression::Lambda(LambdaExpression::Value {
                    params: vec![
                        PatternPrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("f".to_owned())))),
                    ],
                    body: Box::new(ValueExpression::Application(BasicFixity::Prefix(PrefixApply {
                        func: ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("f".to_owned())))),
                        args: vec![
                            ValuePrimary::Identifier(ScopedIdentifier::from(Identifier::Name(Name("a".to_owned())))),
                        ],
                    }))),
                }))))
            ],
        }));
        let result = Token::parse_sequence(input);
        let result = ValueExpression::parse(TokenStream(result.as_slice()));
        assert!(result.is_ok(), "Expected ok result, got {:?}", result);
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn extracts_names() {
        let input = "a @ (lam (Some b, c). b + c + (lam _. a))";
        let input = Token::parse_sequence(input);
        let expr = ValueExpression::parse(TokenStream(&input)).unwrap().1;
        let mut data = SemanticData::new();
        let ctx = SemanticContext {
            enclosing_scope: AstSymbol::new(SymbolSpace::Value, ""),
            enclosing_definition: AstSymbol::new(SymbolSpace::Value, ""),
        };
        let expected = SemanticData::from_parts(
            // patterns never declare anything explicitly
            HashSet::new(),
            vec![
                (AstSymbol::from_scopes(SymbolSpace::Value, &[""]), AstSymbol::from_scopes(SymbolSpace::Value, &["a"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &[""]), AstSymbol::from_scopes(SymbolSpace::Value, &["@"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["", "0"]), AstSymbol::from_scopes(SymbolSpace::Pattern, &["Some"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["", "0"]), AstSymbol::from_scopes(SymbolSpace::Pattern, &["b"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["", "0"]), AstSymbol::from_scopes(SymbolSpace::Pattern, &[","])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["", "0"]), AstSymbol::from_scopes(SymbolSpace::Pattern, &["c"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["", "0"]), AstSymbol::from_scopes(SymbolSpace::Value, &["b"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["", "0"]), AstSymbol::from_scopes(SymbolSpace::Value, &["+"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["", "0"]), AstSymbol::from_scopes(SymbolSpace::Value, &["c"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["", "0", "0"]), AstSymbol::from_scopes(SymbolSpace::Value, &["a"])),
            ].into_iter().collect(),
        );
        expr.extract(&mut data, &ctx);
        assert_eq!(data, expected);
    }
}
