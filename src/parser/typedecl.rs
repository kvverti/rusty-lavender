use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{map, value};
use nom::sequence::{delimited, preceded};

use crate::ast::{Extract, SemanticContext, SemanticData};
use crate::ast::symbol::{AstSymbol, SymbolSpace};
use crate::parser::fixity::{BasicFixity, InfixNamespace};
use crate::parser::ParseResult;
use crate::parser::primary::{name, Primary};
use crate::parser::scoped::ScopedIdentifier;
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::token::fixed::{Keyword, Separator};
use crate::parser::token::identifier::Name;
use crate::parser::typedecl::typelambda::TypeLambda;

/// Type lambda expression.
pub mod typelambda;

/// A type expression, used wherever a type may be placed.
#[derive(Clone, Debug, PartialEq)]
pub enum TypePrimary {
    /// A type name `A`.
    TypeIdentifier(ScopedIdentifier),
    /// A type variable name `'a`.
    TypeVariable(Name),
    /// A type hole `_` (triggers explicit type inference).
    TypeHole,
    /// A parenthesized type expression `( a )`.
    TypeSubExpression(Box<TypeExpression>),
}

impl Primary for TypePrimary {
    fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        alt((
            map(ScopedIdentifier::parse, Self::TypeIdentifier),
            map(
                preceded(tag(TokenValue::from(Separator::Check)), name),
                Self::TypeVariable,
            ),
            value(Self::TypeHole, tag(TokenValue::from(Keyword::Underscore))),
            map(
                delimited(
                    tag(TokenValue::from(Separator::LeftRound)),
                    TypeExpression::parse,
                    tag(TokenValue::from(Separator::RightRound)),
                ),
                |e| Self::TypeSubExpression(Box::new(e)),
            )
        ))(input)
    }
}

impl Extract for TypePrimary {
    fn extract(&self, data: &mut SemanticData, ctx: &SemanticContext) {
        match self {
            // type IDs are unbound symbols (at first)
            Self::TypeIdentifier(id) => {
                let symbol = AstSymbol::from_scopes(SymbolSpace::Type, &id.to_scopes());
                data.declare_unbound_symbol(ctx.enclosing_scope.clone(), symbol);
            }
            // type variables are bound to definition scope
            Self::TypeVariable(name) => {
                let symbol = AstSymbol::in_scope(SymbolSpace::Type, &ctx.enclosing_definition, &name.0);
                data.declare_symbol(symbol);
            }
            // subexpressions pass through
            Self::TypeSubExpression(expr) => expr.extract(data, ctx),
            // a hole declares no symbols
            Self::TypeHole => {}
        }
    }
}

impl InfixNamespace for TypePrimary {
    const NAMESPACE: SymbolSpace = SymbolSpace::Type;
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeExpression {
    /// Basic type applications.
    TypeApplication(BasicFixity<TypePrimary>),
    /// A universal quantifier, or type lambda expression.
    TypeLambda(TypeLambda),
}

impl TypeExpression {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        alt((
            map(TypeLambda::parse, Self::TypeLambda),
            map(BasicFixity::parse, Self::TypeApplication),
        ))(input)
    }
}

impl Extract for TypeExpression {
    fn extract(&self, data: &mut SemanticData, ctx: &SemanticContext) {
        match self {
            Self::TypeApplication(app) => app.extract(data, ctx),
            Self::TypeLambda(lambda) => lambda.extract(data, ctx),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::fixity::{InfixApply, InfixPrimary, PrefixApply};
    use crate::parser::tagged::Tagged;
    use crate::parser::token::identifier::{Identifier, Operator};
    use crate::parser::token::Token;

    use super::*;

    #[test]
    fn parses() {
        let expected = TypeExpression::TypeApplication(BasicFixity::Prefix(PrefixApply {
            func: TypePrimary::TypeIdentifier(ScopedIdentifier::from(Identifier::Name(Name("Type".to_owned())))),
            args: vec![
                TypePrimary::TypeVariable(Name("a".to_owned())),
                TypePrimary::TypeSubExpression(Box::new(
                    TypeExpression::TypeApplication(BasicFixity::Infix(InfixApply {
                        func: Tagged::new(Identifier::Operator(Operator("->".to_owned()))),
                        args: vec![
                            InfixPrimary::Primary(TypePrimary::TypeVariable(Name("a".to_owned()))),
                            InfixPrimary::Primary(TypePrimary::TypeHole),
                        ],
                    }))
                )),
            ],
        }));
        let input = [
            Token::new(TokenValue::from(Identifier::Name(Name("Type".to_string())))),
            Token::new(TokenValue::from(Separator::Check)),
            Token::new(TokenValue::from(Identifier::Name(Name("a".to_owned())))),
            Token::new(TokenValue::from(Separator::LeftRound)),
            Token::new(TokenValue::from(Separator::Check)),
            Token::new(TokenValue::from(Identifier::Name(Name("a".to_owned())))),
            Token::new(TokenValue::from(Identifier::Operator(Operator("->".to_string())))),
            Token::new(TokenValue::from(Keyword::Underscore)),
            Token::new(TokenValue::from(Separator::RightRound)),
        ];
        let result = TypeExpression::parse(TokenStream(&input));
        assert!(result.is_ok(), format!("Expected ok parse: {:?}", result));
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn extracts_names() {
        let input = "'a -> (for b. b -> 'a)";
        let input = Token::parse_sequence(input);
        let expr = TypeExpression::parse(TokenStream(&input)).unwrap().1;
        let mut data = SemanticData::new();
        let ctx = SemanticContext {
            enclosing_scope: AstSymbol::new(SymbolSpace::Value, ""),
            enclosing_definition: AstSymbol::new(SymbolSpace::Value, ""),
        };
        let expected = SemanticData::from_parts(
            vec![
                AstSymbol::from_scopes(SymbolSpace::Type, &["", "a"]),
                AstSymbol::from_scopes(SymbolSpace::Type, &["", "0", "b"])
            ].into_iter().collect(),
            vec![
                (AstSymbol::from_scopes(SymbolSpace::Value, &[""]), AstSymbol::from_scopes(SymbolSpace::Type, &["->"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["", "0"]), AstSymbol::from_scopes(SymbolSpace::Type, &["->"])),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["", "0"]), AstSymbol::from_scopes(SymbolSpace::Type, &["b"])),
            ].into_iter().collect(),
        );
        expr.extract(&mut data, &ctx);
        assert_eq!(data, expected);
    }
}
