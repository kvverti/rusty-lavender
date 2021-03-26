use crate::ast::node::{AstDefinition, AstDefinitionBody, ExtractAstNode};
use crate::ast::symbol::{AstSymbol, SymbolContext, SymbolData, SymbolSpace, GLOBAL_SCOPE};
use crate::parser::item::{Definition, DefinitionBody};

impl ExtractAstNode for DefinitionBody {
    type Node = AstDefinitionBody;

    fn construct_ast(self, data: &SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        let Self { params, body } = self;
        let inner_scope = AstSymbol::in_scope(
            SymbolSpace::Value,
            ctx.enclosing_scope,
            &ctx.implicit_scope.as_scopes().join("/"),
        );
        let inner_ctx = ctx
            .with_enclosing_scope(&inner_scope)
            .with_implicit_scope(&GLOBAL_SCOPE);
        let params = params
            .into_iter()
            .map(|param| param.construct_ast(data, inner_ctx));
        let body = body.construct_ast(data, inner_ctx);
        AstDefinitionBody {
            params: params.collect(),
            body,
        }
    }
}

impl ExtractAstNode for Definition {
    type Node = AstDefinition;

    fn construct_ast(self, data: &SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        let Self {
            name,
            typ,
            params,
            bodies,
            ..
        } = self;
        let name = AstSymbol::in_scope(SymbolSpace::Value, ctx.enclosing_scope, name.value.value());
        let name_key = data
            .resolve(&GLOBAL_SCOPE, name)
            .expect("Unbound definition");
        let name = &data.data(SymbolSpace::Value, name_key).0.value;
        let def_ctx = ctx
            .with_enclosing_scope(name)
            .with_enclosing_definition(name);
        let typ = typ.construct_ast(data, def_ctx);
        let params = params
            .into_iter()
            .map(|param| param.construct_ast(data, def_ctx));
        let body_scope = AstSymbol::in_scope(SymbolSpace::Value, name, "#body");
        let body_ctx = def_ctx.with_enclosing_scope(&body_scope);
        let bodies = bodies.into_iter().enumerate().map(|(idx, body)| {
            let implicit =
                AstSymbol::in_scope(SymbolSpace::Value, ctx.implicit_scope, &idx.to_string());
            body.construct_ast(data, body_ctx.with_implicit_scope(&implicit))
        });
        AstDefinition {
            name: name_key,
            typ,
            params: params.collect(),
            bodies: bodies.collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::node::{
        AstDefinition, AstDefinitionBody, AstPatternExpression, AstTypeExpression,
        AstValueExpression, ExtractAstNode,
    };
    use crate::ast::symbol::{ExtractSymbol, LookupKey, SymbolContext, SymbolData};
    use crate::parser::item::Definition;
    use crate::parser::token::{Token, TokenStream};

    #[test]
    fn constructs() {
        let input = "def (.) f g a => f (g a)";
        let dot = LookupKey::new(0);
        let f = LookupKey::new(1);
        let g = LookupKey::new(2);
        let a = LookupKey::new(3);
        let mut data = SymbolData::new();
        let expected = AstDefinition {
            name: dot,
            typ: AstTypeExpression::Hole,
            params: vec![
                AstPatternExpression::Symbol(f),
                AstPatternExpression::Symbol(g),
                AstPatternExpression::Symbol(a),
            ],
            bodies: vec![AstDefinitionBody {
                params: vec![],
                body: AstValueExpression::Application(
                    Box::new(AstValueExpression::Symbol(f)),
                    Box::new(AstValueExpression::Application(
                        Box::new(AstValueExpression::Symbol(g)),
                        Box::new(AstValueExpression::Symbol(a)),
                    )),
                ),
            }],
        };
        let input = Token::parse_sequence(input);
        let input = Definition::regular(TokenStream(&input)).unwrap().1;
        input.extract(&mut data, SymbolContext::new());
        let ast = input.construct_ast(&data, SymbolContext::new());
        assert_eq!(ast, expected);
    }
}
