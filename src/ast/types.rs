//! Contains the intermediate type representations. These types are resolved during type
//! inference.

use std::cell::RefCell;
use std::fmt::{Display, Formatter};

use crate::ast::symbol::AstSymbol;

/// A reference to an `AstType` in the context of some arena.
pub type TypeRef<'sym, 'arena> = &'arena RefCell<AstType<'sym, 'arena>>;

/// A bound type variable for use in a type schema. These may be user defined or inferred by
/// the type checker.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BoundVariable<'a> {
    /// A variable declared in the source code.
    Declared(&'a AstSymbol),
    /// A variable inferred by the type checker.
    Inferred(u64),
}

impl Display for BoundVariable<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Declared(symb) => {
                let last_scope = symb.as_scopes().last().expect("No name for user-defined binding");
                write!(f, "'{}", last_scope)
            }
            Self::Inferred(idx) => {
                write!(f, "'{}", idx)
            }
        }
    }
}

/// The type of an expression or name.
///
/// Note: It's very important that values of `AstType` are not copied accidentally, since the type
/// inference algorithm is very identity-sensitive.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AstType<'sym, 'arena> {
    /// A free variable which must be inferred.
    FreeVariable(u64),
    /// A bound variable which may be captured.
    BoundVariable(BoundVariable<'sym>),
    /// A plain concrete type such as `Int`.
    Atom(&'sym AstSymbol),
    /// A function type.
    Function {
        param: TypeRef<'sym, 'arena>,
        result: TypeRef<'sym, 'arena>,
    },
    /// A type universally quantified over a set of bound variables.
    Schema {
        vars: Vec<TypeRef<'sym, 'arena>>,
        inner: TypeRef<'sym, 'arena>,
    },
    /// A type equal to it's inner type. References to this should be replaced with
    /// references to the inner type.
    Unification(TypeRef<'sym, 'arena>),
}

impl Display for AstType<'_, '_> {
    /// Format an [`AstType`], handling recursive references.
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fn write_type_ref(f: &mut Formatter<'_>, type_ref: TypeRef<'_, '_>) -> std::fmt::Result {
            // if we don't have unique access, we are in a cycle
            // we shouldn't have a cycle anyway...
            if let Ok(typ) = type_ref.try_borrow_mut() {
                write!(f, "{}", typ)
            } else {
                f.write_str("...")
            }
        }
        match self {
            AstType::FreeVariable(idx) => write!(f, "#{}", idx),
            AstType::BoundVariable(v) => write!(f, "{}", v),
            AstType::Atom(symb) => write!(f, "{}", symb.as_scopes().join("::")),
            AstType::Function { param, result } => {
                f.write_str("(")
                    .and_then(|_| write_type_ref(f, param))
                    .and_then(|_| f.write_str(") -> "))
                    .and_then(|_| write_type_ref(f, result))
            }
            AstType::Schema { vars, inner } => {
                let mut ret = f.write_str("for");
                for v in vars {
                    ret = ret
                        .and_then(|_| f.write_str(" "))
                        .and_then(|_| write_type_ref(f, v));
                }
                ret
                    .and_then(|_| f.write_str(". "))
                    .and_then(|_| write_type_ref(f, inner))
            }
            AstType::Unification(type_ref) => {
                write_type_ref(f, type_ref)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use typed_arena::Arena;

    use crate::ast::symbol::SymbolSpace;

    use super::*;

    #[test]
    fn test_basic_format() {
        let int = AstSymbol::from_scopes(SymbolSpace::Type, &["sys", "intrinsic", "Int"]);
        let a = AstSymbol::from_scopes(SymbolSpace::Type, &["f", "a"]);
        let type_arena = Arena::new();
        let free_0: TypeRef = type_arena.alloc(RefCell::new(AstType::FreeVariable(0)));
        let bound_0: TypeRef = type_arena.alloc(RefCell::new(AstType::BoundVariable(BoundVariable::Inferred(0))));
        let bound_d: TypeRef = type_arena.alloc(RefCell::new(AstType::BoundVariable(BoundVariable::Declared(&a))));
        let atom: TypeRef = type_arena.alloc(RefCell::new(AstType::Atom(&int)));
        let func: TypeRef = type_arena.alloc(RefCell::new(AstType::Function {
            param: free_0,
            result: bound_0,
        }));
        let schema: TypeRef = type_arena.alloc(RefCell::new(AstType::Schema {
            vars: vec![bound_0],
            inner: func,
        }));
        let uni: TypeRef = type_arena.alloc(RefCell::new(AstType::Unification(schema)));
        let expected = vec![
            "#0",
            "'0",
            "'a",
            "sys::intrinsic::Int",
            "(#0) -> '0",
            "for '0. (#0) -> '0",
            "for '0. (#0) -> '0",
        ];
        let types = vec![
            free_0,
            bound_0,
            bound_d,
            atom,
            func,
            schema,
            uni,
        ].into_iter().map(|r| format!("{}", r.borrow())).collect::<Vec<_>>();
        assert_eq!(types, expected);
    }

    #[test]
    fn test_recursive() {
        let type_arena = Arena::new();
        let type_ref: TypeRef = type_arena.alloc(RefCell::new(AstType::FreeVariable(0)));
        *type_ref.borrow_mut() = AstType::Unification(type_ref);
        let expected = "...";
        let result = format!("{}", type_ref.borrow());
        assert_eq!(result, expected);
    }
}
