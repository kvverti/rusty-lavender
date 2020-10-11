use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::multi::many0;
use nom::sequence::{pair, terminated};

use crate::ast::Extract;
use crate::ast::symbol::AstSymbol;
use crate::parser::fixity::prefix_operator;
use crate::parser::ParseResult;
use crate::parser::primary::name;
use crate::parser::token::{TokenStream, TokenValue};
use crate::parser::token::fixed::Separator;
use crate::parser::token::identifier::{Identifier, Name};

/// A scoped identifier `a::b::c`
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ScopedIdentifier {
    pub name: Identifier,
    pub scopes: Vec<Name>,
}

impl ScopedIdentifier {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        map(
            pair(
                many0(terminated(name, tag(TokenValue::from(Separator::DoubleColon)))),
                prefix_operator,
            ),
            |(scopes, name)| ScopedIdentifier { name, scopes },
        )(input)
    }
}

impl Extract<AstSymbol> for ScopedIdentifier {
    fn extract(&self) -> Vec<AstSymbol> {
        let mut symb = self.name.extract();
        for Name(scope) in self.scopes.iter().rev() {
            symb.iter_mut().for_each(|s| s.place_in_scope(scope));
        }
        symb
    }
}

impl From<Identifier> for ScopedIdentifier {
    fn from(v: Identifier) -> Self {
        Self { name: v, scopes: vec![] }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Extracted;
    use crate::parser::token::Token;

    use super::*;

    #[test]
    fn qualified() {
        let expected = ScopedIdentifier {
            name: Identifier::Name(Name("c".to_owned())),
            scopes: vec![
                Name("a".to_owned()),
                Name("b".to_owned()),
            ],
        };
        let input = [
            Token::new(TokenValue::from(Identifier::Name(Name("a".to_owned())))),
            Token::new(TokenValue::from(Separator::DoubleColon)),
            Token::new(TokenValue::from(Identifier::Name(Name("b".to_owned())))),
            Token::new(TokenValue::from(Separator::DoubleColon)),
            Token::new(TokenValue::from(Identifier::Name(Name("c".to_owned())))),
        ];
        let result = ScopedIdentifier::parse(TokenStream(&input));
        assert!(result.is_ok(), "Expected ok result, got {:?}", result);
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn unqualified() {
        let expected = ScopedIdentifier {
            name: Identifier::Name(Name("c".to_owned())),
            scopes: vec![],
        };
        let input = [
            Token::new(TokenValue::from(Identifier::Name(Name("c".to_owned())))),
        ];
        let result = ScopedIdentifier::parse(TokenStream(&input));
        assert!(result.is_ok(), "Expected ok result, got {:?}", result);
        let (rest, result) = result.unwrap();
        assert_eq!(rest.0, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn tailed() {
        let input = [
            Token::new(TokenValue::from(Identifier::Name(Name("a".to_owned())))),
            Token::new(TokenValue::from(Separator::DoubleColon)),
            Token::new(TokenValue::from(Identifier::Name(Name("b".to_owned())))),
            Token::new(TokenValue::from(Separator::DoubleColon)),
        ];
        let result = ScopedIdentifier::parse(TokenStream(&input));
        assert!(result.is_err(), "Expected err result, got {:?}", result);
    }

    #[test]
    fn name_extract() {
        let id = ScopedIdentifier {
            name: Identifier::Name(Name("a".into())),
            scopes: vec![Name("b".into()), Name("c".into()), Name("d".into())],
        };
        let symbs: Vec<AstSymbol> = AstSymbol::extract_from(id);
        assert_eq!(symbs.len(), 1);
        let symb = symbs.first().unwrap();
        assert_eq!(*symb, AstSymbol::new_scoped(&["b", "c", "d", "a"]));
    }
}
