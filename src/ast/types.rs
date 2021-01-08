//! Contains the intermediate type representations. These types are resolved during type
//! inference.

use std::cell::RefCell;
use std::fmt::{Display, Formatter};

use typed_arena::Arena;

use crate::ast::symbol::AstSymbol;

mod instantiation;

pub type TypeArena<'sym, 'arena> = Arena<RefCell<AstType<'sym, 'arena>>>;

/// Visitor for AST types.
pub trait TypeVisitor<'sym, 'arena> {
    type Input;
    type Output;

    fn visit_free(&mut self, v: u64, typ: TypeRef<'sym, 'arena>, arg: Self::Input) -> Self::Output;
    fn visit_bound(&mut self, v: BoundVariable<'sym>, typ: TypeRef<'sym, 'arena>, arg: Self::Input) -> Self::Output;
    fn visit_atom(&mut self, sym: &'sym AstSymbol, typ: TypeRef<'sym, 'arena>, arg: Self::Input) -> Self::Output;
    fn visit_func(&mut self,
                  param: TypeRef<'sym, 'arena>,
                  result: TypeRef<'sym, 'arena>,
                  typ: TypeRef<'sym, 'arena>,
                  arg: Self::Input) -> Self::Output;
    fn visit_apply(&mut self,
                   ctor: TypeRef<'sym, 'arena>,
                   par: TypeRef<'sym, 'arena>,
                   typ: TypeRef<'sym, 'arena>,
                   arg: Self::Input) -> Self::Output;
    fn visit_schema(&mut self,
                    vars: &[BoundVariable<'sym>],
                    inner: TypeRef<'sym, 'arena>,
                    typ: TypeRef<'sym, 'arena>,
                    arg: Self::Input) -> Self::Output;
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
    /// Format an [`AstType`], handling recursive references.
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AstType::FreeVariable(idx) => write!(f, "#{}", idx),
            AstType::BoundVariable(v) => write!(f, "{}", v),
            AstType::Atom(symb) => write!(f, "{}", symb.as_scopes().join("::")),
            AstType::Function { param, result } => write!(f, "({}) -> {}", param, result),
            AstType::Application { ctor, arg } => write!(f, "{} ({})", ctor, arg),
            AstType::Schema { vars, inner } => {
                let mut ret = f.write_str("(for");
                for v in vars {
                    ret = ret
                        .and_then(|_| f.write_str(" "))
                        .and_then(|_| write!(f, "{}", v));
                }
                ret.and_then(|_| write!(f, ". {})", inner))
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
            AstType::Function { param, result } => visitor.visit_func(param, result, self, arg),
            AstType::Application { ctor, arg: a } => visitor.visit_apply(ctor, a, self, arg),
            AstType::Schema { ref vars, inner } => visitor.visit_schema(vars, inner, self, arg),
            AstType::Unification(type_ref) => type_ref.accept(visitor, arg),
        }
    }

    /// Unifies the given types, or returns an error if they cannot be unified.
    pub fn unify(self,
                 arena: &'arena TypeArena<'sym, 'arena>,
                 bindings: &[(BoundVariable<'sym>, BoundVariable<'sym>)],
                 other: Self) -> Result<Self, ()>
    {
        match (&*self.0.borrow(), &*other.0.borrow()) {
            // free vars unify with everything
            (_, AstType::FreeVariable(_)) => Ok(self),
            (AstType::FreeVariable(_), _) => Ok(other),
            // unifications unify like their referents
            (_, &AstType::Unification(type_ref)) => self.unify(arena, bindings, type_ref),
            (&AstType::Unification(type_ref), _) => type_ref.unify(arena, bindings, other),
            // bound variables are unified according to the given bindings
            // (we choose the left binding always)
            (&AstType::BoundVariable(b1),
                &AstType::BoundVariable(b2)) if bindings.contains(&(b1, b2)) => Ok(self),
            // equality short circuit
            (typ, other) if typ == other => Ok(self),
            (&AstType::Function { param: p1, result: r1 },
                &AstType::Function { param: p2, result: r2 }) => {
                let unified = AstType::Function {
                    param: p1.unify(arena, bindings, p2)?,
                    result: r1.unify(arena, bindings, r2)?,
                };
                Ok(Self::new_in(arena, unified))
            }
            (&AstType::Application { ctor: c1, arg: a1 },
                &AstType::Application { ctor: c2, arg: a2 }) => {
                let unified = AstType::Application {
                    ctor: c1.unify(arena, bindings, c2)?,
                    arg: a1.unify(arena, bindings, a2)?,
                };
                Ok(Self::new_in(arena, unified))
            }
            // schemas (we choose the left bindings always)
            (&AstType::Schema { vars: ref v1, inner: i1 },
                &AstType::Schema { vars: ref v2, inner: i2 }) => {
                assert!(bindings.iter().all(|(l, r)| !v1.contains(l) && !v2.contains(r)),
                        "Duplicate bound variable in nested schema");
                let unified = AstType::Schema {
                    vars: v1.clone(),
                    inner: i1.unify(arena, bindings, i2)?,
                };
                Ok(Self::new_in(arena, unified))
            }
            // everything else does not unify
            _ => Err(())
        }
    }
}

impl Display for TypeRef<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // if we don't have unique access, we are in a cycle
        // we shouldn't have a cycle anyway...
        if let Ok(typ) = self.0.try_borrow_mut() {
            write!(f, "{}", typ)
        } else {
            f.write_str("...")
        }
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
        let func = TypeRef::new_in(&arena, AstType::Function {
            param: free_0,
            result: bound_0,
        });
        let app = TypeRef::new_in(&arena, AstType::Application {
            ctor: free_0,
            arg: bound_d,
        });
        let schema = TypeRef::new_in(&arena, AstType::Schema {
            vars: vec![BoundVariable::Inferred(0)],
            inner: func,
        });
        let uni = TypeRef::new_in(&arena, AstType::Unification(schema));
        let expected = vec![
            "#0",
            "'0",
            "'a",
            "sys::intrinsic::Int",
            "(#0) -> '0",
            "#0 ('a)",
            "(for '0. (#0) -> '0)",
            "(for '0. (#0) -> '0)",
        ];
        let types = vec![
            free_0,
            bound_0,
            bound_d,
            atom,
            func,
            app,
            schema,
            uni,
        ].into_iter().map(|r| format!("{}", r)).collect::<Vec<_>>();
        assert_eq!(types, expected);
    }

    #[test]
    fn recursive() {
        let arena = Arena::new();
        let type_ref = TypeRef::new_in(&arena, AstType::FreeVariable(0));
        *type_ref.0.borrow_mut() = AstType::Unification(type_ref);
        let expected = "...";
        let result = format!("{}", type_ref);
        assert_eq!(result, expected);
    }

    #[test]
    fn instantiation() {
        let a = AstSymbol::from_scopes(SymbolSpace::Type, &["a"]);
        let b = AstSymbol::from_scopes(SymbolSpace::Type, &["b"]);
        let foo_s = AstSymbol::from_scopes(SymbolSpace::Type, &["Foo"]);
        let a = BoundVariable::Declared(&a);
        let b = BoundVariable::Declared(&b);

        let arena = Arena::new();
        let free = TypeRef::new_in(&arena, AstType::FreeVariable(0));
        let atom_foo = TypeRef::new_in(&arena, AstType::Atom(&foo_s));
        let bound = TypeRef::new_in(&arena, AstType::BoundVariable(a));
        let bound_b = TypeRef::new_in(&arena, AstType::BoundVariable(b));
        let b_a = TypeRef::new_in(&arena, AstType::Application { ctor: bound_b, arg: bound });
        let schema_inner = TypeRef::new_in(&arena, AstType::Schema { vars: vec![b], inner: b_a });
        let foo_a = TypeRef::new_in(&arena, AstType::Application { ctor: atom_foo, arg: bound });
        let uni = TypeRef::new_in(&arena, AstType::Unification(foo_a));
        let func_inner = TypeRef::new_in(&arena, AstType::Function { param: uni, result: schema_inner });
        let foo_0 = TypeRef::new_in(&arena, AstType::Application { ctor: atom_foo, arg: free });
        let func = TypeRef::new_in(&arena, AstType::Function { param: foo_0, result: func_inner });
        let schema = TypeRef::new_in(&arena, AstType::Schema { vars: vec![a], inner: func });
        assert_eq!(format!("{}", schema), "(for 'a. (Foo (#0)) -> (Foo ('a)) -> (for 'b. 'b ('a)))");

        let value = TypeRef::new_in(&arena, AstType::FreeVariable(1));
        let replace = vec![(a, value)];
        let mut visitor = InstantiationVisitor { arena: &arena, vars: replace };

        let old_len = arena.len();
        let inst = func.accept(&mut visitor, ());
        let new_len = arena.len();
        assert_eq!(format!("{}", inst), "(Foo (#0)) -> (Foo (#1)) -> (for 'b. 'b (#1))");
        assert_eq!(new_len - old_len, 5);
    }

    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    struct TypeHolder<'sym, 'arena> {
        free: TypeRef<'sym, 'arena>,
        bound: TypeRef<'sym, 'arena>,
        atom: TypeRef<'sym, 'arena>,
        function: TypeRef<'sym, 'arena>,
        application: TypeRef<'sym, 'arena>,
        schema: TypeRef<'sym, 'arena>,
    }

    impl<'sym, 'arena> TypeHolder<'sym, 'arena> {
        fn new_in(arena: &'arena TypeArena<'sym, 'arena>) -> Self {
            // leak is ok, it's a test
            let a = &*Box::leak::<'sym>(Box::new(AstSymbol::new(SymbolSpace::Type, "Foo")));
            let free = TypeRef::new_in(arena, AstType::FreeVariable(0));
            let bound = TypeRef::new_in(arena, AstType::BoundVariable(BoundVariable::Inferred(0)));
            let atom = TypeRef::new_in(arena, AstType::Atom(a));
            let function = TypeRef::new_in(arena, AstType::Function { param: atom, result: bound });
            let application = TypeRef::new_in(arena, AstType::Application { ctor: atom, arg: free });
            let schema = TypeRef::new_in(arena, AstType::Schema { vars: vec![BoundVariable::Inferred(0)], inner: function });
            Self { free, bound, atom, function, application, schema }
        }
    }

    #[test]
    fn unify_free() {
        let arena = Arena::new();
        let holder = TypeHolder::new_in(&arena);
        let unify_with = TypeRef::new_in(&arena, AstType::FreeVariable(1));
        let bindings = &[];
        assert_eq!(holder.free.unify(&arena, bindings, unify_with), Ok(holder.free));
        assert_eq!(holder.bound.unify(&arena, bindings, unify_with), Ok(holder.bound));
        assert_eq!(holder.atom.unify(&arena, bindings, unify_with), Ok(holder.atom));
        assert_eq!(holder.function.unify(&arena, bindings, unify_with), Ok(holder.function));
        assert_eq!(holder.application.unify(&arena, bindings, unify_with), Ok(holder.application));
        assert_eq!(holder.schema.unify(&arena, bindings, unify_with), Ok(holder.schema));
    }
}
