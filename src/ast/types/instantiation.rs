use crate::ast::types::{TypeArena, BoundVariable, TypeRef, TypeVisitor, AstType};
use crate::ast::symbol::AstSymbol;

/// Applies the given bound variable replacements to types.
pub struct InstantiationVisitor<'sym, 'arena> {
    pub arena: &'arena TypeArena<'sym, 'arena>,
    pub vars: Vec<(BoundVariable<'sym>, TypeRef<'sym, 'arena>)>,
}

impl<'sym, 'arena> TypeVisitor<'sym, 'arena> for InstantiationVisitor<'sym, 'arena> {
    /// The type being instantiated from.
    type Input = ();
    /// The instantiated type.
    type Output = TypeRef<'sym, 'arena>;

    fn visit_free(&mut self, _v: u64, typ: TypeRef<'sym, 'arena>, _: Self::Input) -> Self::Output {
        typ
    }

    fn visit_bound(&mut self, var: BoundVariable<'sym>, typ: TypeRef<'sym, 'arena>, _: Self::Input) -> Self::Output {
        let tp = self.vars.iter().find(|(v, _)| *v == var);
        if let Some((_, value)) = tp {
            *value
        } else {
            typ
        }
    }

    fn visit_atom(&mut self, _sym: &'sym AstSymbol, typ: TypeRef<'sym, 'arena>, _: Self::Input) -> Self::Output {
        typ
    }

    fn visit_func(&mut self, param: TypeRef<'sym, 'arena>, result: TypeRef<'sym, 'arena>, _typ: TypeRef<'sym, 'arena>, _: Self::Input) -> Self::Output {
        let typ = AstType::Function {
            param: param.accept(self, ()),
            result: result.accept(self, ()),
        };
        TypeRef::new_in(self.arena, typ)
    }

    fn visit_apply(&mut self, ctor: TypeRef<'sym, 'arena>, par: TypeRef<'sym, 'arena>, _typ: TypeRef<'sym, 'arena>, _: Self::Input) -> Self::Output {
        let typ = AstType::Application {
            ctor: ctor.accept(self, ()),
            arg: par.accept(self, ()),
        };
        TypeRef::new_in(self.arena, typ)
    }

    fn visit_schema(&mut self, vars: &[BoundVariable<'sym>], inner: TypeRef<'sym, 'arena>, _typ: TypeRef<'sym, 'arena>, _: Self::Input) -> Self::Output {
        assert!(self.vars.iter().all(|(v, _)| !vars.contains(v)),
                "Duplicate bound variable in nested schema");
        let typ = AstType::Schema {
            vars: vars.to_vec(),
            inner: inner.accept(self, ()),
        };
        TypeRef::new_in(self.arena, typ)
    }
}
