use crate::ast::symbol::{AstSymbol, ExtractSymbol, SymbolContext, SymbolData, SymbolSpace};
use crate::parser::fixity::{BasicFixity, InfixApply, InfixPrimary, PrefixApply};
use crate::parser::primary::Primary;

/// Trait for determining which infix namespace to use.
pub trait InfixNamespace {
    const NAMESPACE: SymbolSpace;
}

impl<P: Primary + ExtractSymbol + InfixNamespace> ExtractSymbol for BasicFixity<P> {
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

impl<P: Primary + ExtractSymbol + InfixNamespace> ExtractSymbol for InfixApply<P> {
    /// Extract the function name as unbound and extract the arguments.
    fn extract(&self, data: &mut SymbolData, ctx: SymbolContext) {
        let func_symbol = AstSymbol::new(P::NAMESPACE, self.func.value.value());
        data.declare_unbound_symbol(ctx.enclosing_scope.clone(), func_symbol);
        for primary in &self.args {
            primary.extract(data, ctx);
        }
    }
}

impl<P: Primary + ExtractSymbol> ExtractSymbol for PrefixApply<P> {
    /// Extracts data from the function and its arguments.
    fn extract(&self, data: &mut SymbolData, ctx: SymbolContext) {
        self.func.extract(data, ctx);
        for arg in &self.args {
            arg.extract(data, ctx);
        }
    }
}

