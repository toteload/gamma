use crate::ast::*;
use crate::ast_visitor::Visitor;
use crate::semantics::{SemanticAnalyser, SemanticError};
use crate::source_location::SourceSpan;
use crate::string_interner::{StringInterner, Symbol};
use std::collections::{HashMap, HashSet};

// This should check for unused symbols and items with the same name.

#[derive(Debug, Clone, Copy)]
pub struct UndefinedSymbolAtLocation {
    pub span: SourceSpan,
    pub sym: Symbol,
}

pub struct Analyser<'a> {
    scopes: Vec<HashSet<Symbol>>,
    undefined_symbols: Vec<UndefinedSymbolAtLocation>,
    spans: &'a HashMap<NodeId, SourceSpan>,
}

impl<'a> Analyser<'a> {
    pub fn new(spans: &'a HashMap<NodeId, SourceSpan>) -> Analyser<'a> {
        Self {
            scopes: Vec::new(),
            undefined_symbols: Vec::new(),
            spans,
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
            let span = *self.spans.get(id).unwrap();
            self.undefined_symbols
                .push(UndefinedSymbolAtLocation { span, sym: *sym });
        }
    }
}

impl Visitor for Analyser<'_> {
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
            Let { name, init, .. } => {
                // Make sure all variables used in the initialization expression are defined.
                // This must be called before we add the variable to the scope, otherwise you could
                // define the variable in terms of itself, which is not allowed.
                self.visit_expr(init);
                self.scopes.last_mut().unwrap().insert(name.sym);
            }
            Assign { name, val } => {
                self.validate_sym(&name.sym, &stmt.id);
                self.visit_expr(val);
            }
            Expr(e) => self.visit_expr(e),
            Block(b) => self.visit_block(b),
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
            Loop(body) => self.visit_block(body),
            Return(Some(e)) => self.visit_expr(e),
            Empty | Break | Continue | Return(None) => (),
        }
    }

    fn visit_expr(&mut self, e: &Expr) {
        use ExprKind::*;

        match &e.kind {
            IntLiteral(_) => (),
            Identifier(sym) => self.validate_sym(sym, &e.id),
            UnaryOp { e, .. } => self.visit_expr(e),
            BinaryOp { lhs, rhs, .. } => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            Call { name, args } => {
                self.validate_sym(&name.sym, &e.id);

                for arg in args {
                    self.visit_expr(arg);
                }
            }
        }
    }
}

impl SemanticAnalyser for Analyser<'_> {
    fn check_for_error(&mut self, items: &[Item]) -> Option<Box<dyn SemanticError>> {
        self.visit_items(items);

        if !self.undefined_symbols.is_empty() {
            return Some(Box::new(UndefinedSymbolsError {
                undefined_symbols: self.undefined_symbols.clone(),
            }));
        }

        None
    }
}

struct UndefinedSymbolsError {
    undefined_symbols: Vec<UndefinedSymbolAtLocation>,
}

impl SemanticError for UndefinedSymbolsError {
    fn print(&self, symbols: &StringInterner) {
        for sym in self.undefined_symbols.iter() {
            let line = sym.span.start.line;
            let col = sym.span.start.col;
            let name = symbols.get_str(sym.sym);
            println!("ERROR: <source>:{line}:{col} Undefined variable \"{name}\"");
        }
    }
}
