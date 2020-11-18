//! The AST module contains the intermediate representation for Lavender after parsing produces
//! a parse tree. The AST resolves symbols, transforms infix application into prefix application
//! with the appropriate fixity, and performs type checking and inference, as well as various
//! inlining and optimizations. The AST is then used to generate bytecode which can be executed
//! by the runtime.

mod node;
pub mod symbol;
pub mod types;
