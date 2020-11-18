//! Contains AST nodes.

// AST nodes contain
// - symbols used
//   Vec<AstSymbol>
// Plan
// 1. Iterate over parse tree
// 2. collect unbound symbols from parse tree nodes
// 2a. construct AST nodes corresponding to the parts of the parse tree, containing the
//     extracted symbols

use crate::ast::node::fixity::AstApply;
use crate::ast::symbol::{AstSymbol, SymbolContext, SymbolData};
use crate::parser::tagged::Tagged;
use crate::parser::token::literal::Literal;

mod pattern;
mod fixity;

/// Trait implemented on parse tree nodes to construct the corresponding AST.
pub trait ExtractAstNode<'a> {
    /// The type of AST node.
    type Node: 'a;

    /// Consumes this parse subtree and constructs the corresponding AST subtree.
    fn construct_ast(self, data: &'a SymbolData, ctx: SymbolContext<'_>) -> Self::Node;
}

/// The AST representation of value expressions.
#[derive(Clone, Debug, PartialEq)]
pub enum AstValueExpression {
    /// Literal value
    Constant(Literal),
    /// Symbol (an identifier)
    Symbol(AstSymbol),
    /// Function application
    Application(Box<Self>, Box<Self>),
    /// Lambda expression
    Abstraction(Vec<AstPatternExpression<'static>>, Box<Self>),
    /// Error node
    Error(Tagged<&'static str>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstTypeExpression {
    /// Explicit type inference
    Hole,
    /// Use of a type
    Symbol(AstSymbol),
    /// Type constructor application
    Application(Box<Self>, Box<Self>),
    /// Type lambda
    Abstraction(Vec<AstSymbol>, Box<Self>),
    /// Error node
    Error(Tagged<&'static str>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstPatternExpression<'a> {
    /// Blank (ignore) pattern
    Blank,
    /// Constant pattern
    Constant(Literal),
    /// Binding
    Symbol(&'a AstSymbol),
    /// Destructuring
    Application(Box<Self>, Box<Self>),
    /// Error node
    Error(Tagged<&'static str>),
}

impl<'a> AstApply<'a> for AstPatternExpression<'a> {
    fn symbol(symb: &'a AstSymbol) -> Self {
        Self::Symbol(symb)
    }

    fn apply(f: Self, a: Self) -> Self {
        Self::Application(Box::new(f), Box::new(a))
    }

    fn error(msg: Tagged<&'static str>) -> Self {
        Self::Error(msg)
    }
}
