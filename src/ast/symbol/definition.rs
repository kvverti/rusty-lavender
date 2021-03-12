use crate::ast::symbol::{
    AstSymbol, ExtractSymbol, SymbolContext, SymbolData, SymbolSpace, GLOBAL_SCOPE,
};
use crate::parser::item::{Definition, DefinitionBody};
use crate::parser::token::identifier::Identifier;

impl ExtractSymbol for DefinitionBody {
    /// Extract symbols from the patterns and the expression body. The passed context
    /// is already contained in the function's scope.
    fn extract(&self, data: &mut SymbolData, ctx: SymbolContext<'_>) {
        let inner_scope = AstSymbol::in_scope(
            SymbolSpace::Value,
            ctx.enclosing_scope,
            &ctx.implicit_scope.as_scopes().join("/"),
        );
        let inner_ctx = ctx
            .with_enclosing_scope(&inner_scope)
            .with_implicit_scope(&GLOBAL_SCOPE);
        for param in &self.params {
            param.extract(data, inner_ctx);
        }
        self.body.extract(data, inner_ctx);
    }
}

impl ExtractSymbol for Definition {
    /// Extract the name, type, parameters, and definition bodies.
    fn extract(&self, data: &mut SymbolData, ctx: SymbolContext<'_>) {
        let name = self.name.as_ref().map(Identifier::value);
        let name_scope =
            name.map(|name| AstSymbol::in_scope(SymbolSpace::Value, ctx.enclosing_scope, name));
        data.declare_with_data(name_scope.clone(), self.fixity);
        let function_ctx = ctx
            .with_enclosing_scope(&name_scope.value)
            .with_enclosing_definition(&name_scope.value);
        self.typ.extract(data, function_ctx);
        for param in &self.params {
            param.extract(data, function_ctx);
        }
        // disambiguate type level anon scopes from bodies
        let body_scope = AstSymbol::in_scope(SymbolSpace::Value, &name_scope.value, "#body");
        let body_ctx = function_ctx.with_enclosing_scope(&body_scope);
        for (idx, body) in self.bodies.iter().enumerate() {
            let implicit =
                AstSymbol::in_scope(SymbolSpace::Value, &ctx.implicit_scope, &idx.to_string());
            body.extract(data, body_ctx.with_implicit_scope(&implicit));
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::item::Fixity;
    use crate::parser::tagged::Tagged;
    use crate::parser::token::{Token, TokenStream};

    use super::*;

    #[test]
    fn extracts_symbols_from_simple() {
        let input = "def ($)' a => a";
        let input = Token::parse_sequence(input);
        let input = Definition::regular(TokenStream(&input)).unwrap().1;
        let mut data = SymbolData::new();
        let ctx = SymbolContext::new();
        let expected = SymbolData::from_parts(
            vec![
                (
                    AstSymbol::from_scopes(SymbolSpace::Value, &["$"]),
                    Tagged {
                        value: Fixity::Right,
                        idx: 4,
                        len: 3,
                    },
                ),
                (
                    AstSymbol::from_scopes(SymbolSpace::Value, &["$", "a"]),
                    Tagged {
                        value: Fixity::None,
                        idx: 9,
                        len: 1,
                    },
                ),
            ]
            .into_iter()
            .collect(),
            vec![],
        );
        input.extract(&mut data, ctx);
        assert_eq!(data, expected);
    }

    #[test]
    fn extracts_symbols_from_single_body() {
        let input = "def const: 'a -> (for b. b -> 'a); a _ => a";
        let input = Token::parse_sequence(input);
        let input = Definition::regular(TokenStream(&input)).unwrap().1;
        let mut data = SymbolData::new();
        let ctx = SymbolContext::new();
        let expected = SymbolData::from_parts(
            vec![
                (
                    AstSymbol::from_scopes(SymbolSpace::Value, &["const"]),
                    Tagged {
                        value: Fixity::None,
                        idx: 4,
                        len: 5,
                    },
                ),
                (
                    AstSymbol::from_scopes(SymbolSpace::Type, &["const", "a"]),
                    Tagged {
                        value: Fixity::None,
                        idx: 11,
                        len: 2,
                    },
                ),
                (
                    AstSymbol::from_scopes(SymbolSpace::Type, &["const", "1", "b"]),
                    Tagged {
                        value: Fixity::None,
                        idx: 22,
                        len: 1,
                    },
                ),
                (
                    AstSymbol::from_scopes(SymbolSpace::Value, &["const", "a"]),
                    Tagged {
                        value: Fixity::None,
                        idx: 35,
                        len: 1,
                    },
                ),
            ]
            .into_iter()
            .collect(),
            vec![],
        );
        input.extract(&mut data, ctx);
        assert_eq!(data, expected);
    }

    #[test]
    fn extracts_symbols_from_multi_body() {
        let input_str = "\
            def map: ('a -> 'b) -> Option 'a -> Option 'b;\
                f ; (Some a) => Some (f a)\
                  ; None => None";
        let input = Token::parse_sequence(input_str);
        let input = Definition::regular(TokenStream(&input)).unwrap().1;
        let mut data = SymbolData::new();
        let ctx = SymbolContext::new();
        let expected = SymbolData::from_parts(
            vec![
                (
                    AstSymbol::from_scopes(SymbolSpace::Value, &["map"]),
                    Tagged {
                        value: Fixity::None,
                        idx: input_str.match_indices("map").next().unwrap().0,
                        len: 3,
                    },
                ),
                (
                    AstSymbol::from_scopes(SymbolSpace::Type, &["map", "a"]),
                    Tagged {
                        value: Fixity::None,
                        idx: input_str.match_indices("'a").next().unwrap().0,
                        len: 2,
                    },
                ),
                (
                    AstSymbol::from_scopes(SymbolSpace::Type, &["map", "b"]),
                    Tagged {
                        value: Fixity::None,
                        idx: input_str.match_indices("'b").next().unwrap().0,
                        len: 2,
                    },
                ),
                (
                    AstSymbol::from_scopes(SymbolSpace::Value, &["map", "f"]),
                    Tagged {
                        value: Fixity::None,
                        idx: input_str.match_indices('f').nth(1).unwrap().0,
                        len: 1,
                    },
                ),
                (
                    AstSymbol::from_scopes(SymbolSpace::Value, &["map", "#body", "0", "a"]),
                    Tagged {
                        value: Fixity::None,
                        idx: input_str.match_indices('a').nth(3).unwrap().0,
                        len: 1,
                    },
                ),
            ]
            .into_iter()
            .collect(),
            vec![],
        );
        input.extract(&mut data, ctx);
        assert_eq!(data, expected);
    }
}
