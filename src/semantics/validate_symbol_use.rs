use crate::ast::*;
use crate::error::*;
use crate::semantics::{SemanticContext, SemanticProver};
use crate::string_interner::Symbol;
use crate::visitor::Visitor;
use std::collections::HashSet;

pub struct Prover<'a> {
    scopes: Vec<HashSet<Symbol>>,
    errors: Vec<Error>,
    context: &'a SemanticContext<'a>,
}

impl<'a> Prover<'a> {
    pub fn new(context: &'a SemanticContext) -> Self {
        Self {
            scopes: Vec::new(),
            errors: Vec::new(),
            context,
        }
    }

    fn is_sym_defined(&self, sym: &Symbol) -> bool {
        for scope in self.scopes.iter().rev() {
            if scope.contains(sym) {
                return true;
            }
        }

        false
    }

    fn validate_sym(&mut self, sym: &Symbol, id: &NodeId) {
        if !self.is_sym_defined(sym) {
            use ErrorInfo::*;

            let span = *self.context.spans.get(id).unwrap();
            self.errors.push(Error {
                source: ErrorSource::AstNode(*id),
                info: vec![Text("Undefined identifier used "), Identifier(*sym)],
            });
        }
    }
}

impl Visitor for Prover<'_> {
    fn on_items_enter(&mut self, items: &[Item]) {
        // TODO: Check for items with the same name.

        let global_scope = items.iter().map(|i| i.name_sym()).collect();
        self.scopes.push(global_scope);
    }

    fn on_function_enter(&mut self, function: &Item) {
        let ItemKind::Function { params, .. } = &function.kind else {
            unreachable!()
        };
        let param_scope_symbols = params.iter().map(|p| p.name.sym).collect();
        self.scopes.push(param_scope_symbols);
    }

    fn on_function_leave(&mut self, function: &Item) {
        self.scopes.pop();
    }

    fn on_block_enter(&mut self, block: &Block) {
        self.scopes.push(HashSet::new());
    }

    fn on_block_leave(&mut self, block: &Block) {
        self.scopes.pop();
    }

    fn on_statement_enter(&mut self, statement: &Statement) {
        use StatementKind::*;

        if let Let { name, init, .. } = &statement.kind {
            self.scopes.last_mut().unwrap().insert(name.sym);
        }
    }

    fn on_expression_enter(&mut self, expression: &Expression) {
        use ExpressionKind::*;

        let Expression { kind, id } = expression;

        match kind {
            Identifier(sym) => self.validate_sym(sym, id),
            Call { name, args } => {
                self.validate_sym(&name.sym, id);
            }
            _ => {}
        }
    }
}

impl SemanticProver for Prover<'_> {
    fn verify(&mut self, items: &[Item]) -> Result<(), Vec<Error>> {
        self.visit_items(items);

        if !self.errors.is_empty() {
            return Err(self.errors.clone());
        }

        Ok(())
    }
}
