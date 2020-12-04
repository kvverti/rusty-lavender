use crate::ast::symbol::{AstSymbol, ExtractSymbol, SymbolContext, SymbolData, SymbolSpace};
use crate::parser::fixity::{BasicFixity, InfixApply, InfixPrimary, PrefixApply};
use crate::parser::primary::Primary;

impl<P: Primary + ExtractSymbol> ExtractSymbol for BasicFixity<P> {
    /// Extract from the inner value.
    fn extract(&self, data: &mut SymbolData, ctx: SymbolContext) {
        match self {
            Self::Primary(p) => p.extract(data, ctx),
            Self::Infix(infix) => infix.extract(data, ctx),
            Self::Prefix(prefix) => prefix.extract(data, ctx),
        }
    }
}

impl<P: Primary + ExtractSymbol> ExtractSymbol for InfixPrimary<P> {
    /// Extracts data from the inner value.
    fn extract(&self, data: &mut SymbolData, ctx: SymbolContext) {
        match self {
            Self::Primary(p) => p.extract(data, ctx),
            Self::Application(prefix) => prefix.extract(data, ctx),
        }
    }
}

impl<P: Primary + ExtractSymbol> ExtractSymbol for InfixApply<P> {
    /// Extract the function name as unbound and extract the arguments.
    fn extract(&self, data: &mut SymbolData, ctx: SymbolContext) {
        for (idx, primary) in self.args.iter().enumerate() {
            let implicit = AstSymbol::in_scope(SymbolSpace::Value, ctx.implicit_scope, &idx.to_string());
            primary.extract(data, ctx.with_implicit_scope(&implicit));
        }
    }
}

impl<P: Primary + ExtractSymbol> ExtractSymbol for PrefixApply<P> {
    /// Extracts data from the function and its arguments.
    fn extract(&self, data: &mut SymbolData, ctx: SymbolContext) {
        let implicit = AstSymbol::in_scope(SymbolSpace::Value, ctx.implicit_scope, "0");
        self.func.extract(data, ctx.with_implicit_scope(&implicit));
        for (idx, arg) in self.args.iter().enumerate() {
            let implicit = AstSymbol::in_scope(SymbolSpace::Value, ctx.implicit_scope, &(1 + idx).to_string());
            arg.extract(data, ctx.with_implicit_scope(&implicit));
        }
    }
}

