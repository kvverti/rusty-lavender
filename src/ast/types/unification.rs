use crate::ast::symbol::AstSymbol;
use crate::ast::types::{AstType, BoundVariable, TypeRef, TypeVisitor};

/// Performs unification (intrusively) and returns the unified type, if any.
pub struct UnificationVisitor<'sym> {
    pub bindings: Vec<(BoundVariable<'sym>, BoundVariable<'sym>)>,
}

impl<'sym> UnificationVisitor<'sym> {
    /// Attempts to unify the two types, and returns a result indicating success.
    pub fn unify<'arena>(
        &mut self,
        left: TypeRef<'sym, 'arena>,
        right: TypeRef<'sym, 'arena>,
    ) -> Result<(), ()> {
        if !left.identity_eq(right) {
            // unify inner type structure
            left.accept(self, right)?;
            // unify outer type structure
            let mut left_type = left.0.borrow_mut();
            let mut right_type = right.0.borrow_mut();
            if let AstType::FreeVariable(_) = &*left_type {
                // the free variable must be overwritten
                *left_type = AstType::Unification(right);
            } else {
                *right_type = AstType::Unification(left);
            }
        }
        Ok(())
    }
}

// Invariants: `typ` and `arg` are never identical
impl<'sym: 'arena, 'arena> TypeVisitor<'sym, 'arena> for UnificationVisitor<'sym> {
    type Input = TypeRef<'sym, 'arena>;
    type Output = Result<(), ()>;

    fn visit_free(
        &mut self,
        _v: u64,
        _typ: TypeRef<'sym, 'arena>,
        _arg: Self::Input,
    ) -> Self::Output {
        // free variables always unify
        Ok(())
    }

    fn visit_bound(
        &mut self,
        v: BoundVariable<'sym>,
        _typ: TypeRef<'sym, 'arena>,
        arg: Self::Input,
    ) -> Self::Output {
        // bound variables unify with their corresponding bindings
        let other = arg.0.borrow();
        match *other {
            AstType::FreeVariable(_) => Ok(()),
            AstType::BoundVariable(v2) if self.bindings.contains(&(v, v2)) => Ok(()),
            _ => Err(()),
        }
    }

    fn visit_atom(
        &mut self,
        sym: &'sym AstSymbol,
        _typ: TypeRef<'sym, 'arena>,
        arg: Self::Input,
    ) -> Self::Output {
        let other = arg.0.borrow();
        match *other {
            AstType::FreeVariable(_) => Ok(()),
            AstType::Atom(sym2) if sym == sym2 => Ok(()),
            _ => Err(()),
        }
    }

    fn visit_apply(
        &mut self,
        ctor: TypeRef<'sym, 'arena>,
        par: TypeRef<'sym, 'arena>,
        _typ: TypeRef<'sym, 'arena>,
        arg: Self::Input,
    ) -> Self::Output {
        let other = arg.0.borrow();
        match *other {
            AstType::FreeVariable(_) => Ok(()),
            AstType::Application { ctor: c2, arg: a2 } => {
                self.unify(ctor, c2)?;
                self.unify(par, a2)?;
                Ok(())
            }
            _ => Err(()),
        }
    }

    fn visit_schema(
        &mut self,
        vars: &[BoundVariable<'sym>],
        inner: TypeRef<'sym, 'arena>,
        _typ: TypeRef<'sym, 'arena>,
        arg: Self::Input,
    ) -> Self::Output {
        let other = arg.0.borrow();
        match *other {
            AstType::FreeVariable(_) => Ok(()),
            AstType::Schema {
                vars: ref v2,
                inner: i2,
            } => {
                assert!(
                    self.bindings
                        .iter()
                        .all(|(l, r)| !vars.contains(l) && !v2.contains(r)),
                    "Duplicate bound variable in nested schema"
                );
                // unify the declared bound variables
                let outer_len = self.bindings.len();
                let inner_vars = vars.iter().copied().zip(v2.iter().copied());
                self.bindings.extend(inner_vars);
                let ret = self.unify(inner, i2);
                self.bindings.truncate(outer_len);
                ret
            }
            _ => Err(()),
        }
    }
}
