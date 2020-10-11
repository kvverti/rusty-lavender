//! The AST module contains the intermediate representation for Lavender after parsing produces
//! a parse tree. The AST resolves symbols, transforms infix application into prefix application
//! with the appropriate fixity, and performs type checking and inference, as well as various
//! inlining and optimizations. The AST is then used to generate bytecode which can be executed
//! by the runtime.

pub mod symbol;
pub mod types;

/// Extracts a collection of values from some type. This trait is used for walking the parse
/// tree and extracting names, types, definitions, etc.
pub trait Extract<T> {
    /// Extracts a collection of values. Prefer [Extracted::extract_from] to this function.
    fn extract(&self) -> Vec<T>;
}

/// Implemented on types which are extracted from types implementing [Extract<T>].
pub trait Extracted<T>: Sized {
    /// Extracts values from the given argument.
    fn extract_from(t: T) -> Vec<Self>;
}

/// Blanket implementation of [Extracted<T>].
impl<A, T: Extract<A>> Extracted<T> for A {
    fn extract_from(t: T) -> Vec<Self> {
        t.extract()
    }
}
