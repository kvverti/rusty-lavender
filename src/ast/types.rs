//! Contains the intermediate type representations. These types are resolved during type
//! inference.

use std::cell::RefCell;
use std::fmt::{Display, Formatter};

use typed_arena::Arena;

use crate::ast::symbol::AstSymbol;

mod instantiation;
mod unification;

pub type TypeArena<'sym, 'arena> = Arena<RefCell<AstType<'sym, 'arena>>>;

/// Visitor for AST types.
pub trait TypeVisitor<'sym, 'arena> {
    type Input;
    type Output;

    fn visit_free(&mut self, v: u64, typ: TypeRef<'sym, 'arena>, arg: Self::Input) -> Self::Output;
    fn visit_bound(
        &mut self,
        v: BoundVariable<'sym>,
        typ: TypeRef<'sym, 'arena>,
        arg: Self::Input,
    ) -> Self::Output;
    fn visit_atom(
        &mut self,
        sym: &'sym AstSymbol,
        typ: TypeRef<'sym, 'arena>,
        arg: Self::Input,
    ) -> Self::Output;
    fn visit_apply(
        &mut self,
        ctor: TypeRef<'sym, 'arena>,
        par: TypeRef<'sym, 'arena>,
        typ: TypeRef<'sym, 'arena>,
        arg: Self::Input,
    ) -> Self::Output;
    fn visit_schema(
        &mut self,
        vars: &[BoundVariable<'sym>],
        inner: TypeRef<'sym, 'arena>,
        typ: TypeRef<'sym, 'arena>,
        arg: Self::Input,
    ) -> Self::Output;
}

/// A bound type variable for use in a type schema. These may be user defined or inferred by
/// the type checker.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
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
                let last_scope = symb
                    .as_scopes()
                    .last()
                    .expect("No name for user-defined binding");
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
    /// Type application.
    Application {
        ctor: TypeRef<'sym, 'arena>,
        arg: TypeRef<'sym, 'arena>,
    },
    /// A type universally quantified over a set of bound variables.
    Schema {
        vars: Vec<BoundVariable<'sym>>,
        inner: TypeRef<'sym, 'arena>,
    },
    /// A type equal to it's inner type. References to this should be replaced with
    /// references to the inner type.
    Unification(TypeRef<'sym, 'arena>),
}

impl Display for AstType<'_, '_> {
    /// Format an [`AstType`].
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AstType::FreeVariable(idx) => write!(f, "#{}", idx),
            AstType::BoundVariable(v) => write!(f, "{}", v),
            AstType::Atom(symb) => write!(f, "{}", symb.as_scopes().join("::")),
            AstType::Application { ctor, arg } => write!(f, "{} ({})", ctor, arg),
            AstType::Schema { vars, inner } => {
                f.write_str("(for")?;
                for v in vars {
                    f.write_str(" ")?;
                    write!(f, "{}", v)?;
                }
                write!(f, ". {})", inner)
            }
            AstType::Unification(type_ref) => {
                write!(f, "{}", type_ref)
            }
        }
    }
}

/// A reference to an `AstType` in the context of some arena.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct TypeRef<'sym, 'arena>(&'arena RefCell<AstType<'sym, 'arena>>);

impl<'sym, 'arena> TypeRef<'sym, 'arena> {
    /// Creates a new type in the given arena.
    pub fn new_in(arena: &'arena TypeArena<'sym, 'arena>, typ: AstType<'sym, 'arena>) -> Self {
        TypeRef(arena.alloc(RefCell::new(typ)))
    }

    /// Whether both of these references refer to the same type.
    pub fn identity_eq(self, other: Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }

    pub fn accept<V: TypeVisitor<'sym, 'arena>>(self, visitor: &mut V, arg: V::Input) -> V::Output {
        let typ = self.0.borrow();
        match *typ {
            AstType::FreeVariable(v) => visitor.visit_free(v, self, arg),
            AstType::BoundVariable(v) => visitor.visit_bound(v, self, arg),
            AstType::Atom(sym) => visitor.visit_atom(sym, self, arg),
            AstType::Application { ctor, arg: a } => visitor.visit_apply(ctor, a, self, arg),
            AstType::Schema { ref vars, inner } => visitor.visit_schema(vars, inner, self, arg),
            AstType::Unification(type_ref) => type_ref.accept(visitor, arg),
        }
    }
}

impl Display for TypeRef<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.borrow())
    }
}

#[cfg(test)]
mod tests {
    use typed_arena::Arena;

    use crate::ast::symbol::SymbolSpace;
    use crate::ast::types::instantiation::InstantiationVisitor;

    use super::*;

    #[test]
    fn basic_format() {
        let int = AstSymbol::from_scopes(SymbolSpace::Type, &["sys", "intrinsic", "Int"]);
        let a = AstSymbol::from_scopes(SymbolSpace::Type, &["f", "a"]);
        let arena = Arena::new();
        let free_0 = TypeRef::new_in(&arena, AstType::FreeVariable(0));
        let bound_0 = TypeRef::new_in(&arena, AstType::BoundVariable(BoundVariable::Inferred(0)));
        let bound_d = TypeRef::new_in(&arena, AstType::BoundVariable(BoundVariable::Declared(&a)));
        let atom = TypeRef::new_in(&arena, AstType::Atom(&int));
        let app = TypeRef::new_in(
            &arena,
            AstType::Application {
                ctor: free_0,
                arg: bound_d,
            },
        );
        let app2 = TypeRef::new_in(
            &arena,
            AstType::Application {
                ctor: app,
                arg: bound_0,
            },
        );
        let schema = TypeRef::new_in(
            &arena,
            AstType::Schema {
                vars: vec![BoundVariable::Inferred(0)],
                inner: app2,
            },
        );
        let uni = TypeRef::new_in(&arena, AstType::Unification(schema));
        let expected = vec![
            "#0",
            "'0",
            "'a",
            "sys::intrinsic::Int",
            "#0 ('a)",
            "#0 ('a) ('0)",
            "(for '0. #0 ('a) ('0))",
            "(for '0. #0 ('a) ('0))",
        ];
        let types = vec![free_0, bound_0, bound_d, atom, app, app2, schema, uni]
            .into_iter()
            .map(|r| format!("{}", r))
            .collect::<Vec<_>>();
        assert_eq!(types, expected);
    }

    #[test]
    fn instantiation() {
        let a = AstSymbol::from_scopes(SymbolSpace::Type, &["a"]);
        let b = AstSymbol::from_scopes(SymbolSpace::Type, &["b"]);
        let foo_s = AstSymbol::from_scopes(SymbolSpace::Type, &["Foo"]);
        let arrow = AstSymbol::from_scopes(SymbolSpace::Type, &["->"]);
        let a = BoundVariable::Declared(&a);
        let b = BoundVariable::Declared(&b);

        let arena = Arena::new();
        let free = TypeRef::new_in(&arena, AstType::FreeVariable(0));
        let atom_foo = TypeRef::new_in(&arena, AstType::Atom(&foo_s));
        let arrow = TypeRef::new_in(&arena, AstType::Atom(&arrow));
        let bound = TypeRef::new_in(&arena, AstType::BoundVariable(a));
        let bound_b = TypeRef::new_in(&arena, AstType::BoundVariable(b));
        let b_a = TypeRef::new_in(
            &arena,
            AstType::Application {
                ctor: bound_b,
                arg: bound,
            },
        );
        let schema_inner = TypeRef::new_in(
            &arena,
            AstType::Schema {
                vars: vec![b],
                inner: b_a,
            },
        );
        let foo_a = TypeRef::new_in(
            &arena,
            AstType::Application {
                ctor: atom_foo,
                arg: bound,
            },
        );
        let uni = TypeRef::new_in(&arena, AstType::Unification(foo_a));
        let func_inner = TypeRef::new_in(
            &arena,
            AstType::Application {
                ctor: TypeRef::new_in(
                    &arena,
                    AstType::Application {
                        ctor: arrow,
                        arg: uni,
                    },
                ),
                arg: schema_inner,
            },
        );
        let foo_0 = TypeRef::new_in(
            &arena,
            AstType::Application {
                ctor: atom_foo,
                arg: free,
            },
        );
        let func = TypeRef::new_in(
            &arena,
            AstType::Application {
                ctor: TypeRef::new_in(
                    &arena,
                    AstType::Application {
                        ctor: arrow,
                        arg: foo_0,
                    },
                ),
                arg: func_inner,
            },
        );
        let schema = TypeRef::new_in(
            &arena,
            AstType::Schema {
                vars: vec![a],
                inner: func,
            },
        );
        assert_eq!(
            format!("{}", schema),
            "(for 'a. -> (Foo (#0)) (-> (Foo ('a)) ((for 'b. 'b ('a)))))"
        );

        let value = TypeRef::new_in(&arena, AstType::FreeVariable(1));
        let replace = vec![(a, value)];
        let mut visitor = InstantiationVisitor {
            arena: &arena,
            vars: replace,
        };

        let old_len = arena.len();
        let inst = func.accept(&mut visitor, ());
        let new_len = arena.len();
        assert_eq!(
            format!("{}", inst),
            "-> (Foo (#0)) (-> (Foo (#1)) ((for 'b. 'b (#1))))"
        );
        assert_eq!(new_len - old_len, 6);
    }
}
