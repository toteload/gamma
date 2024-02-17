use crate::ast::*;
use crate::ast_visitor::Visitor;
use crate::error::*;
use crate::semantics::{SemanticContext, SemanticProver};
use crate::string_interner::Symbol;
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
    fn visit_items(&mut self, items: &[Item]) {
        // TODO: Check for items with the same name.

        let global_scope = items.iter().map(|i| i.name_sym()).collect();
        self.scopes.push(global_scope);

        for item in items {
            self.visit_item(item);
        }
    }

    fn visit_function(
        &mut self,
        _name: &Name,
        params: &[Param],
        _return_type: &Type,
        body: &Block,
    ) {
        let param_scope_symbols = params.iter().map(|p| p.name.sym).collect();
        self.scopes.push(param_scope_symbols);
        self.visit_block(body);
        self.scopes.pop();
    }

    fn visit_block(&mut self, block: &Block) {
        self.scopes.push(HashSet::new());

        for statement in &block.statements {
            self.visit_statement(statement);
        }

        self.scopes.pop();
    }

    fn visit_statement(&mut self, stmt: &Statement) {
        use StatementKind::*;

        match &stmt.kind {
            Let { name, .. } => {
                self.scopes.last_mut().unwrap().insert(name.sym);
            }
            Set { dst, val } => {
                self.visit_expr(dst);
                self.visit_expr(val);
            }
            Expr(e) => self.visit_expr(e),
            If {
                cond,
                then,
                otherwise,
            } => {
                self.visit_expr(cond);
                self.visit_block(then);
                if let Some(otherwise) = otherwise {
                    self.visit_block(otherwise);
                }
            }
            Return(Some(e)) => self.visit_expr(e),
            Loop(body) => {
                self.visit_block(body);
            }
            Break | Continue | Return(None) => (),
        }
    }

    fn visit_expr(&mut self, e: &Expr) {
        use ExprKind::*;

        match &e.kind {
            IntLiteral(_) | BoolLiteral(_) => (),
            Identifier(sym) => self.validate_sym(sym, &e.id),
            BuiltinOp { args, .. } => {
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            Cast { e, .. } => self.visit_expr(e),
            Call { name, args } => {
                self.validate_sym(&name.sym, &e.id);

                for arg in args {
                    self.visit_expr(arg);
                }
            }
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
