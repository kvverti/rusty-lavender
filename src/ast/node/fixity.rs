use crate::ast::node::ExtractAstNode;
use crate::ast::symbol::{AstSymbol, SymbolContext, SymbolData, SymbolSpace};
use crate::parser::fixity::{BasicFixity, InfixApply, InfixPrimary, PrefixApply};
use crate::parser::item::Fixity;
use crate::parser::primary::Primary;
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

impl<'a, P: Primary + ExtractAstNode<'a>> ExtractAstNode<'a> for PrefixApply<P>
    where P::Node: AstApply<'a>
{
    type Node = P::Node;

    fn construct_ast(self, data: &'a SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        let func_node = self.func.construct_ast(data, ctx);
        // left fold args: f a b c -> ((f a) b) c
        self.args.into_iter().enumerate()
            .map(|(idx, arg)| arg.construct_ast(data, ctx.with_scope_idx(ctx.scope_idx + 1 + idx as u32)))
            .fold(func_node, AstApply::apply)
    }
}

impl<'a, P: Primary + ExtractAstNode<'a>> ExtractAstNode<'a> for InfixPrimary<P>
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

impl<'a, P: Primary + ExtractAstNode<'a> + InfixNamespace> ExtractAstNode<'a> for InfixApply<P>
    where P::Node: AstApply<'a> + Clone
{
    type Node = P::Node;

    fn construct_ast(self, data: &'a SymbolData, ctx: SymbolContext<'_>) -> Self::Node {
        let Self { func, mut args } = self;
        let func_symbol = AstSymbol::new(P::NAMESPACE, func.value.value());
        let (func_node, fixity) = data.resolve_symbol(ctx.enclosing_scope, func_symbol)
            .map(|(s, f)| (Self::Node::symbol(s), f))
            .unwrap_or_else(|| (Self::Node::error(func.as_ref().map(|_| "Cannot resolve symbol")), Fixity::Left));
        // fold order depends on the fixity of the definition
        if fixity == Fixity::None && args.len() != 2 {
            // must have exactly two arguments
            Self::Node::error(func.as_ref().map(|_| "Chained expression with a non-associative operator"))
        } else {
            // reverse order if right associative
            if fixity == Fixity::Right {
                args.reverse()
            }
            let mut args = args.into_iter()
                .enumerate()
                .map(|(idx, arg)| arg.construct_ast(data, ctx.with_scope_idx(ctx.scope_idx + idx as u32)));
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

impl<'a, P: Primary + ExtractAstNode<'a> + InfixNamespace> ExtractAstNode<'a> for BasicFixity<P>
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
