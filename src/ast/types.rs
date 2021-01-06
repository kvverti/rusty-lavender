//! Contains the intermediate type representations. These types are resolved during type
//! inference.

use std::cell::RefCell;
use std::fmt::{Display, Formatter};

use typed_arena::Arena;

use crate::ast::symbol::AstSymbol;

pub type TypeArena<'sym, 'arena> = Arena<RefCell<AstType<'sym, 'arena>>>;

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

    /// Applies the given bound variable replacements to this type.
    pub fn instantiate(self, arena: &'arena TypeArena<'sym, 'arena>, vars: &[(BoundVariable<'sym>, Self)]) -> Self {
        let typ = self.0.borrow();
        match &*typ {
            AstType::FreeVariable(_) | AstType::Atom(_) => self,
            AstType::Unification(type_ref) => type_ref.instantiate(arena, vars),
            AstType::BoundVariable(var) => {
                let tp = vars.iter().find(|(v, _)| v == var);
                if let Some((_, value)) = tp {
                    *value
                } else {
                    self
                }
            }
            AstType::Application { ctor, arg } => {
                let typ = AstType::Application {
                    ctor: ctor.instantiate(arena, vars),
                    arg: arg.instantiate(arena, vars),
                };
                Self::new_in(arena, typ)
            }
            AstType::Function { param, result } => {
                let typ = AstType::Function {
                    param: param.instantiate(arena, vars),
                    result: result.instantiate(arena, vars),
                };
                Self::new_in(arena, typ)
            }
            AstType::Schema { vars: inner_vars, inner } => {
                let filtered_vars = vars.iter()
                    .filter(|(v, _)| !inner_vars.contains(v))
                    .copied()
                    .collect::<Vec<_>>();
                let typ = AstType::Schema {
                    vars: inner_vars.clone(),
                    inner: inner.instantiate(arena, &filtered_vars),
                };
                Self::new_in(arena, typ)
            }
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
        let foo_s = AstSymbol::from_scopes(SymbolSpace::Type, &["Foo"]);
        let a = BoundVariable::Declared(&a);
        let arena = Arena::new();
        let atom_foo = TypeRef::new_in(&arena, AstType::Atom(&foo_s));
        let free = TypeRef::new_in(&arena, AstType::FreeVariable(0));
        let bound = TypeRef::new_in(&arena, AstType::BoundVariable(a));
        let free_a = TypeRef::new_in(&arena, AstType::Application { ctor: free, arg: bound });
        let schema_inner = TypeRef::new_in(&arena, AstType::Schema { vars: vec![a], inner: free_a });
        let foo_a = TypeRef::new_in(&arena, AstType::Application { ctor: atom_foo, arg: bound });
        let uni = TypeRef::new_in(&arena, AstType::Unification(foo_a));
        let func_inner = TypeRef::new_in(&arena, AstType::Function { param: uni, result: schema_inner });
        let func = TypeRef::new_in(&arena, AstType::Function { param: bound, result: func_inner });
        let schema = TypeRef::new_in(&arena, AstType::Schema { vars: vec![a], inner: func });
        assert_eq!(format!("{}", schema), "(for 'a. ('a) -> (Foo ('a)) -> (for 'a. #0 ('a)))");
        let value = TypeRef::new_in(&arena, AstType::FreeVariable(1));
        let replace = vec![(a, value)];
        let inst = func.instantiate(&arena, &replace);
        assert_eq!(format!("{}", inst), "(#1) -> (Foo (#1)) -> (for 'a. #0 ('a))");
    }
}
