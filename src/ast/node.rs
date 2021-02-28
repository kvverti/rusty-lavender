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

mod definition;
mod fixity;
mod pattern;
mod typedecl;
mod value;

/// Trait implemented on parse tree nodes to construct the corresponding AST.
pub trait ExtractAstNode<'a> {
    /// The type of AST node.
    type Node: 'a;

    /// Consumes this parse subtree and constructs the corresponding AST subtree.
    fn construct_ast(self, data: &'a SymbolData, ctx: SymbolContext<'_>) -> Self::Node;
}

/// The AST representation of value expressions.
#[derive(Clone, Debug, PartialEq)]
pub enum AstValueExpression<'a> {
    /// Literal value
    Constant(Literal),
    /// Symbol (an identifier)
    Symbol(&'a AstSymbol),
    /// Function application
    Application(Box<Self>, Box<Self>),
    /// Lambda expression
    Abstraction(Vec<AstPatternExpression<'a>>, Box<Self>),
    /// Error node
    Error(Tagged<&'static str>),
}

impl<'a> AstApply<'a> for AstValueExpression<'a> {
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AstTypeExpression<'a> {
    /// Explicit type inference
    Hole,
    /// Use of a type
    Symbol(&'a AstSymbol),
    /// Type constructor application
    Application(Box<Self>, Box<Self>),
    /// Type lambda
    Abstraction(Vec<&'a AstSymbol>, Box<Self>),
    /// Error node
    Error(Tagged<&'static str>),
}

impl<'a> AstApply<'a> for AstTypeExpression<'a> {
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

#[derive(Clone, Debug, PartialEq)]
pub struct AstDefinition<'a> {
    pub name: &'a AstSymbol,
    pub typ: AstTypeExpression<'a>,
    pub params: Vec<AstPatternExpression<'a>>,
    pub bodies: Vec<AstDefinitionBody<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstDefinitionBody<'a> {
    pub params: Vec<AstPatternExpression<'a>>,
    pub body: AstValueExpression<'a>,
}
