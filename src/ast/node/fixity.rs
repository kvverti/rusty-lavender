use crate::ast::node::ExtractAstNode;
use crate::ast::symbol::{AstSymbol, SymbolContext, SymbolData, SymbolSpace};
use crate::parser::fixity::{BasicFixity, InfixApply, InfixPrimary, PrefixApply};
use crate::parser::item::Fixity;
use crate::parser::tagged::Tagged;

/// Implemented om primaries to define the symbol namespace an infix symbol should be part of.
pub trait InfixNamespace {
    const NAMESPACE: SymbolSpace;
}

/// Generic interface for AST node construction.
pub trait AstApply<'a> {
    /// Constructs a symbol node.
    fn symbol(symb: &'a AstSymbol) -> Self;

    /// Constructs an application node.
    fn apply(f: Self, a: Self) -> Self;

    /// Constructs an error node.
    fn error(msg: Tagged<&'static str>) -> Self;
}

impl<'a, P: ExtractAstNode<'a>> ExtractAstNode<'a> for PrefixApply<P>
    where P::Node: AstApply<'a>
{
    type Node = P::Node;

    fn construct_ast(self, data: &'a SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        let implicit = AstSymbol::in_scope(SymbolSpace::Value, ctx.implicit_scope, "0");
        let func_node = self.func.construct_ast(data, ctx.with_implicit_scope(&implicit));
        // left fold args: f a b c -> ((f a) b) c
        self.args.into_iter().enumerate()
            .map(|(idx, arg)| {
                let implicit = AstSymbol::in_scope(SymbolSpace::Value, ctx.implicit_scope, &(1 + idx).to_string());
                arg.construct_ast(data, ctx.with_implicit_scope(&implicit))
            })
            .fold(func_node, AstApply::apply)
    }
}

impl<'a, P: ExtractAstNode<'a>> ExtractAstNode<'a> for InfixPrimary<P>
    where P::Node: AstApply<'a>
{
    type Node = P::Node;

    fn construct_ast(self, data: &'a SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        match self {
            Self::Primary(p) => p.construct_ast(data, ctx),
            Self::Application(prefix) => prefix.construct_ast(data, ctx),
        }
    }
}

impl<'a, P: ExtractAstNode<'a> + InfixNamespace> ExtractAstNode<'a> for InfixApply<P>
    where P::Node: AstApply<'a> + Clone
{
    type Node = P::Node;

    fn construct_ast(self, data: &'a SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        let Self { func, args } = self;
        let func_symbol = AstSymbol::new(P::NAMESPACE, func.value.value());
        let (func_node, fixity) = data.resolve_symbol(ctx.enclosing_scope, func_symbol)
            .map(|(s, f)| (Self::Node::symbol(s), f))
            .unwrap_or_else(|| (Self::Node::error(func.as_ref().map(|_| "Cannot resolve symbol")), Fixity::Left));
        // fold order depends on the fixity of the definition
        if fixity == Fixity::None && args.len() != 2 {
            // must have exactly two arguments
            Self::Node::error(func.as_ref().map(|_| "Chained expression with a non-associative operator"))
        } else {
            let mut args = args.into_iter()
                .enumerate()
                .map(|(idx, arg)| {
                    let implicit = AstSymbol::in_scope(SymbolSpace::Value, ctx.implicit_scope, &idx.to_string());
                    arg.construct_ast(data, ctx.with_implicit_scope(&implicit))
                })
                .collect::<Vec<_>>();
            // reverse order if right associative
            if fixity == Fixity::Right {
                args.reverse();
            }
            let mut args = args.into_iter();
            let mut acc = args.next().expect("Empty infix arguments");
            for arg in args {
                // (1 + 2) + 3 or 1 + (2 + 3)
                if fixity == Fixity::Right {
                    acc = Self::Node::apply(Self::Node::apply(func_node.clone(), arg), acc);
                } else {
                    acc = Self::Node::apply(Self::Node::apply(func_node.clone(), acc), arg);
                }
            }
            acc
        }
    }
}

impl<'a, P: ExtractAstNode<'a> + InfixNamespace> ExtractAstNode<'a> for BasicFixity<P>
    where P::Node: AstApply<'a> + Clone
{
    type Node = P::Node;

    fn construct_ast(self, data: &'a SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        match self {
            Self::Primary(p) => p.construct_ast(data, ctx),
            Self::Prefix(prefix) => prefix.construct_ast(data, ctx),
            Self::Infix(infix) => infix.construct_ast(data, ctx),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::node::{AstPatternExpression, AstValueExpression, ExtractAstNode};
    use crate::ast::symbol::{AstSymbol, ExtractSymbol, GLOBAL_SCOPE, SymbolContext, SymbolData, SymbolSpace};
    use crate::parser::item::Fixity;
    use crate::parser::tagged::Tagged;
    use crate::parser::token::{Token, TokenStream};
    use crate::parser::value::ValueExpression;

    #[test]
    fn proper_inner_scopes() {
        let input =
            "(a (lam a. a)) (lam a. a) `a` (lam a. (lam a. a) a (lam a. a))";
        //    *       0/0/1        0/1  *                 1,0 1        1,2
        let a = AstSymbol::from_scopes(SymbolSpace::Value, &["a"]);
        let a001 = AstSymbol::from_scopes(SymbolSpace::Value, &["0/0/1", "a"]);
        let a01 = AstSymbol::from_scopes(SymbolSpace::Value, &["0/1", "a"]);
        let a1 = AstSymbol::from_scopes(SymbolSpace::Value, &["1", "a"]);
        let a10 = AstSymbol::from_scopes(SymbolSpace::Value, &["1", "0", "a"]);
        let a12 = AstSymbol::from_scopes(SymbolSpace::Value, &["1", "2", "a"]);
        let mut data = SymbolData::from_parts(
            vec![
                (a.clone(), Tagged::new(Fixity::None)),
            ].into_iter().collect(),
            vec![
                (GLOBAL_SCOPE.clone(), a.clone()),
                (GLOBAL_SCOPE.clone(), a.clone()),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["0/0/1"]), a.clone()),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["0/1"]), a.clone()),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["1"]), a.clone()),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["1", "0"]), a.clone()),
                (AstSymbol::from_scopes(SymbolSpace::Value, &["1", "2"]), a.clone()),
            ],
        );
        let expected = AstValueExpression::Application(
            Box::new(AstValueExpression::Application(
                Box::new(AstValueExpression::Symbol(&a)),
                Box::new(AstValueExpression::Application(
                    Box::new(AstValueExpression::Application(
                        Box::new(AstValueExpression::Symbol(&a)),
                        Box::new(AstValueExpression::Abstraction(
                            vec![AstPatternExpression::Symbol(&a001)],
                            Box::new(AstValueExpression::Symbol(&a001)),
                        )),
                    )),
                    Box::new(AstValueExpression::Abstraction(
                        vec![AstPatternExpression::Symbol(&a01)],
                        Box::new(AstValueExpression::Symbol(&a01)),
                    )),
                )),
            )),
            Box::new(AstValueExpression::Abstraction(
                vec![AstPatternExpression::Symbol(&a1)],
                Box::new(AstValueExpression::Application(
                    Box::new(AstValueExpression::Application(
                        Box::new(AstValueExpression::Abstraction(
                            vec![AstPatternExpression::Symbol(&a10)],
                            Box::new(AstValueExpression::Symbol(&a10)),
                        )),
                        Box::new(AstValueExpression::Symbol(&a1)),
                    )),
                    Box::new(AstValueExpression::Abstraction(
                        vec![AstPatternExpression::Symbol(&a12)],
                        Box::new(AstValueExpression::Symbol(&a12)),
                    )),
                )),
            )),
        );
        let input = Token::parse_sequence(input);
        let input = ValueExpression::parse(TokenStream(&input)).unwrap().1;
        input.extract(&mut data, SymbolContext::new());
        let ast = input.construct_ast(&data, SymbolContext::new());
        data.assert_resolved();
        assert_eq!(ast, expected);
    }
}
