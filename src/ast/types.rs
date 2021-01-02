//! Contains the intermediate type representations. These types are resolved during type
//! inference.

use std::fmt::{Display, Formatter};

use crate::ast::symbol::AstSymbol;

/// A reference to an `AstType` in the context of some arena.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Default)]
pub struct TypeRef(usize);

impl TypeRef {
    /// Formats a [`TypeRef`] according to its referent type, while handling recursion,
    /// if it should ever happen.
    fn format(self, f: &mut Formatter<'_>, ctx: &[AstType<'_>], seen: &mut Vec<TypeRef>) -> std::fmt::Result {
        if seen.contains(&self) {
            // recursive
            write!(f, "...")
        } else {
            seen.push(self);
            let typ = &ctx[self.0];
            let result = typ.format(f, ctx, seen);
            let last = seen.pop();
            assert_eq!(last.expect("Expected to pop this reference"), self);
            result
        }
    }
}

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
pub enum AstType<'a> {
    /// A free variable which must be inferred.
    FreeVariable(u64),
    /// A bound variable which may be captured.
    BoundVariable(BoundVariable<'a>),
    /// A plain concrete type such as `Int`.
    Atom(&'a AstSymbol),
    /// A function type.
    Function {
        param: TypeRef,
        result: TypeRef,
    },
    /// A type universally quantified over a set of bound variables.
    Schema {
        vars: Vec<TypeRef>,
        inner: TypeRef,
    },
    /// A type equal to it's inner type. References to this should be replaced with
    /// references to the inner type.
    Unification(TypeRef),
}

impl<'a> AstType<'a> {
    /// Creates a displayable type given context of all other types.
    pub fn with_context<'b>(&'b self, ctx: &'b [Self]) -> TypeFormattingContext<'a, 'b> {
        TypeFormattingContext {
            ctx,
            typ: self,
        }
    }

    /// Format an [`AstType`], handling recursive references.
    fn format(&self, f: &mut Formatter<'_>, ctx: &[Self], seen: &mut Vec<TypeRef>) -> std::fmt::Result {
        match self {
            AstType::FreeVariable(idx) => write!(f, "#{}", idx),
            AstType::BoundVariable(v) => write!(f, "{}", v),
            AstType::Atom(symb) => write!(f, "{}", symb.as_scopes().join("::")),
            AstType::Function { param, result } => {
                f.write_str("(")
                    .and_then(|_| param.format(f, ctx, seen))
                    .and_then(|_| f.write_str(") -> "))
                    .and_then(|_| result.format(f, ctx, seen))
            }
            AstType::Schema { vars, inner } => {
                let mut ret = f.write_str("for");
                for v in vars {
                    ret = ret
                        .and_then(|_| f.write_str(" "))
                        .and_then(|_| v.format(f, ctx, seen));
                }
                let ret = ret.and_then(|_| f.write_str(". "));
                ret.and_then(|_| inner.format(f, ctx, seen))
            }
            AstType::Unification(type_ref) => {
                type_ref.format(f, ctx, seen)
            }
        }
    }
}

/// Display helper for [`AstType`].
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct TypeFormattingContext<'a, 'b> {
    ctx: &'b [AstType<'a>],
    typ: &'b AstType<'a>,
}

impl Display for TypeFormattingContext<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.typ.format(f, self.ctx, &mut Vec::new())
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::symbol::SymbolSpace;

    use super::*;

    #[test]
    fn test_basic_format() {
        let int = AstSymbol::from_scopes(SymbolSpace::Type, &["sys", "intrinsic", "Int"]);
        let a = AstSymbol::from_scopes(SymbolSpace::Type, &["f", "a"]);
        let types = [
            AstType::FreeVariable(0),
            AstType::BoundVariable(BoundVariable::Inferred(0)),
            AstType::BoundVariable(BoundVariable::Declared(&a)),
            AstType::Atom(&int),
            AstType::Function {
                param: TypeRef(0),
                result: TypeRef(1),
            },
            AstType::Schema {
                vars: vec![TypeRef(1)],
                inner: TypeRef(4),
            },
            AstType::Unification(TypeRef(5)),
        ];
        let expected = vec![
            "#0",
            "'0",
            "'a",
            "sys::intrinsic::Int",
            "(#0) -> '0",
            "for '0. (#0) -> '0",
            "for '0. (#0) -> '0",
        ];
        let formatting_types = types.iter()
            .map(|typ| typ.with_context(&types))
            .map(|type_ctx| format!("{}", type_ctx))
            .collect::<Vec<_>>();
        assert_eq!(formatting_types, expected);
    }

    #[test]
    fn test_recursive() {
        let types = [
            AstType::Unification(TypeRef(0))
        ];
        let expected = "...";
        let result = format!("{}", types[0].with_context(&types));
        assert_eq!(result, expected);
    }
}
