use crate::ast::types::{AstType, BoundVariable, TypeArena, TypeRef, TypeVisitor};

/// Applies the given bound variable replacements to types.
pub struct InstantiationVisitor<'arena> {
    pub arena: &'arena TypeArena<'arena>,
    pub vars: Vec<(BoundVariable, TypeRef<'arena>)>,
}

impl<'arena> TypeVisitor<'arena> for InstantiationVisitor<'arena> {
    /// The type being instantiated from.
    type Input = ();
    /// The instantiated type.
    type Output = TypeRef<'arena>;

    fn visit_free(&mut self, _v: u64, typ: TypeRef<'arena>, _: Self::Input) -> Self::Output {
        typ
    }

    fn visit_bound(
        &mut self,
        var: BoundVariable,
        typ: TypeRef<'arena>,
        _: Self::Input,
    ) -> Self::Output {
        let tp = self.vars.iter().find(|(v, _)| *v == var);
        if let Some(&(_, value)) = tp {
            value
        } else {
            typ
        }
    }

    fn visit_atom(&mut self, _sym: usize, typ: TypeRef<'arena>, _: Self::Input) -> Self::Output {
        typ
    }

    fn visit_apply(
        &mut self,
        ctor: TypeRef<'arena>,
        par: TypeRef<'arena>,
        typ: TypeRef<'arena>,
        _: Self::Input,
    ) -> Self::Output {
        let ctor1 = ctor.accept(self, ());
        let arg1 = par.accept(self, ());
        if ctor.identity_eq(ctor1) && par.identity_eq(arg1) {
            typ
        } else {
            TypeRef::new_in(
                self.arena,
                AstType::Application {
                    ctor: ctor1,
                    arg: arg1,
                },
            )
        }
    }

    fn visit_schema(
        &mut self,
        vars: &[BoundVariable],
        inner: TypeRef<'arena>,
        typ: TypeRef<'arena>,
        _: Self::Input,
    ) -> Self::Output {
        assert!(
            self.vars.iter().all(|(v, _)| !vars.contains(v)),
            "Duplicate bound variable in nested schema"
        );
        let inner1 = inner.accept(self, ());
        if inner.identity_eq(inner1) {
            typ
        } else {
            TypeRef::new_in(
                self.arena,
                AstType::Schema {
                    vars: vars.to_vec(),
                    inner: inner1,
                },
            )
        }
    }
}
