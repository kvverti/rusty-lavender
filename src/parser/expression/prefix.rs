use nom::combinator::map;
use nom::sequence::pair;

use crate::parser::expression::primary::Primary;
use crate::parser::ParseResult;
use crate::parser::token::TokenStream;

/// Prefix function application `a b`, where both `a` and `b` are primaries.
#[derive(Clone, Debug, PartialEq)]
pub struct PrefixApply {
    pub func: Primary,
    pub arg: Primary,
}

impl PrefixApply {
    pub fn parse(input: TokenStream) -> ParseResult<TokenStream, Self> {
        map(
            pair(Primary::parse, Primary::parse),
            |(func, arg)| Self { func, arg },
        )(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::expression::prefix::PrefixApply;
    use crate::parser::expression::primary::Primary;
    use crate::parser::token::{Token, TokenStream, TokenValue};
    use crate::parser::token::fixed::Separator;
    use crate::parser::token::identifier::{Identifier, Name, Operator};
    use crate::parser::token::literal::{IntLiteral, Literal};

    #[test]
    fn parses() {
        let expected = PrefixApply {
            func: Primary::Identifier(Identifier::Operator(Operator("+".to_owned()))),
            arg: Primary::Identifier(Identifier::Name(Name("f".to_owned()))),
        };
        let success = [
            TokenValue::from(Separator::LeftRound),
            TokenValue::from(Identifier::Operator(Operator("+".to_owned()))),
            TokenValue::from(Separator::RightRound),
            TokenValue::from(Identifier::Name(Name("f".to_owned()))),
            TokenValue::from(Literal::Int(IntLiteral(1))),
        ];
        let success_vec = success.iter()
            .map(|t| Token::new(t.clone()))
            .collect::<Vec<_>>();
        let result = PrefixApply::parse(TokenStream(success_vec.as_slice()));
        assert!(result.is_ok(), format!("Result not ok: {:?}", result));
        let (rest, expr) = result.unwrap();
        let rest = rest.0.iter().map(|t| t.value.clone()).collect::<Vec<_>>();
        assert_eq!(rest.as_slice(), &success[4..]);
        assert_eq!(expr, expected);
    }
}