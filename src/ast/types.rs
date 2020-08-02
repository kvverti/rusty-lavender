//! Contains the intermediate type representations. These types are resolved during type
//! inference.

use crate::ast::symbol::AstSymbol;

/// A reference to an `AstType` in the context of some arena.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Default)]
pub struct TypeRef(usize);

/// The type of an expression or name.
///
/// Note: It's very important that values of `AstType` are not copied accidentally, since the type
/// inference algorithm is very identity-sensitive.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AstType<'a> {
    /// A free variable which must be inferred.
    FreeVariable(&'a AstSymbol),
    /// A bound variable which may be captured.
    BoundVariable(&'a AstSymbol),
    /// A plain concrete type such as `Int`.
    Atom(&'a AstSymbol),
    /// A function type.
    Function {
        param: TypeRef,
        result: TypeRef,
    },
    /// A type with the given bound variable substitution.
    Substitution {
        schema: TypeRef,
        binding: TypeRef,
    },
    /// A type equal to it's inner type. References to `Equivalence` should be replaced with
    /// references to the inner type.
    Equivalence(TypeRef),
}
