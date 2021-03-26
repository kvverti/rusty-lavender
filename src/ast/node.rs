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
use crate::ast::symbol::{LookupKey, SymbolContext, SymbolData};
use crate::parser::tagged::Tagged;
use crate::parser::token::literal::Literal;

mod definition;
mod fixity;
mod pattern;
mod typedecl;
mod value;

/// Trait implemented on parse tree nodes to construct the corresponding AST.
pub trait ExtractAstNode {
    /// The type of AST node.
    type Node;

    /// Consumes this parse subtree and constructs the corresponding AST subtree.
    fn construct_ast(self, data: &SymbolData, ctx: SymbolContext<'_>) -> Self::Node;
}

/// The AST representation of value expressions.
#[derive(Clone, Debug, PartialEq)]
pub enum AstValueExpression {
    /// Literal value
    Constant(Literal),
    /// Symbol (an identifier)
    Symbol(LookupKey),
    /// Function application
    Application(Box<Self>, Box<Self>),
    /// Lambda expression
    Abstraction(Vec<AstPatternExpression>, Box<Self>),
    /// Error node
    Error(Tagged<&'static str>),
}

impl AstApply for AstValueExpression {
    fn symbol(key: LookupKey) -> Self {
        Self::Symbol(key)
    }

    fn apply(f: Self, a: Self) -> Self {
        Self::Application(Box::new(f), Box::new(a))
    }

    fn error(msg: Tagged<&'static str>) -> Self {
        Self::Error(msg)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AstTypeExpression {
    /// Explicit type inference
    Hole,
    /// Use of a type
    Symbol(LookupKey),
    /// Type constructor application
    Application(Box<Self>, Box<Self>),
    /// Type lambda
    Abstraction(Vec<LookupKey>, Box<Self>),
    /// Error node
    Error(Tagged<&'static str>),
}

impl AstApply for AstTypeExpression {
    fn symbol(key: LookupKey) -> Self {
        Self::Symbol(key)
    }

    fn apply(f: Self, a: Self) -> Self {
        Self::Application(Box::new(f), Box::new(a))
    }

    fn error(msg: Tagged<&'static str>) -> Self {
        Self::Error(msg)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstPatternExpression {
    /// Blank (ignore) pattern
    Blank,
    /// Constant pattern
    Constant(Literal),
    /// Binding
    Symbol(LookupKey),
    /// Destructuring
    Application(Box<Self>, Box<Self>),
    /// Error node
    Error(Tagged<&'static str>),
}

impl AstApply for AstPatternExpression {
    fn symbol(key: LookupKey) -> Self {
        Self::Symbol(key)
    }

    fn apply(f: Self, a: Self) -> Self {
        Self::Application(Box::new(f), Box::new(a))
    }

    fn error(msg: Tagged<&'static str>) -> Self {
        Self::Error(msg)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstDefinition {
    pub name: LookupKey,
    pub typ: AstTypeExpression,
    pub params: Vec<AstPatternExpression>,
    pub bodies: Vec<AstDefinitionBody>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstDefinitionBody {
    pub params: Vec<AstPatternExpression>,
    pub body: AstValueExpression,
}
