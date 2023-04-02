use crate::ast::*;
use crate::parse::SourceSpan;
use crate::string_interner::{StringInterner, Symbol};
use crate::types::{TypeInterner, TypeToken};
use std::collections::{HashMap, HashSet};

trait Visitor {
    fn visit_items(&mut self, items: &[Item]) {
        for item in items {
            self.visit_item(item);
        }
    }

    fn visit_item(&mut self, item: &Item) {
        match &item.kind {
            ItemKind::Function(f) => self.visit_function(f),
        }
    }

    fn visit_function(&mut self, f: &Function) {
        self.visit_block(&f.body);
    }

    fn visit_block(&mut self, block: &Block) {
        for statement in &block.statements {
            self.visit_statement(statement);
        }
    }

    fn visit_statement(&mut self, stmt: &Statement) {
        todo!();
    }

    fn visit_expr(&mut self, e: &Expr) {
        todo!();
    }
}

struct HasMainFunctionVisitor {
    main_symbol: Symbol,
    found_main_function: bool,
}

impl Visitor for HasMainFunctionVisitor {
    fn visit_function(&mut self, f: &Function) {
        if f.name == self.main_symbol {
            self.found_main_function = true;
        }
    }
}

pub fn has_main_function(items: &[Item], symbols: &StringInterner) -> bool {
    let Some(main_symbol) = symbols.find_symbol("main") else { return false; };
    let mut visitor = HasMainFunctionVisitor {
        main_symbol,
        found_main_function: false,
    };
    visitor.visit_items(items);
    visitor.found_main_function
}

struct SymbolValidationVisitor {
    scopes: Vec<HashSet<Symbol>>,
    undefined_symbols: Vec<(SourceSpan, Symbol)>,
}

impl SymbolValidationVisitor {
    fn is_sym_defined(&self, sym: &Symbol) -> bool {
        for scope in self.scopes.iter().rev() {
            if scope.contains(sym) {
                return true;
            }
        }

        false
    }

    fn validate_sym(&mut self, sym: &Symbol, span: SourceSpan) {
        if !self.is_sym_defined(sym) {
            self.undefined_symbols.push((span, *sym));
        }
    }
}

impl Visitor for SymbolValidationVisitor {
    fn visit_items(&mut self, items: &[Item]) {
        // TODO: Check for items with the same name.

        let global_scope = items.iter().map(|i| i.name_sym()).collect();
        self.scopes.push(global_scope);

        for item in items {
            self.visit_item(item);
        }
    }

    fn visit_function(&mut self, f: &Function) {
        let param_scope = f.params.iter().map(|p| p.name).collect();
        self.scopes.push(param_scope);
        self.visit_block(&f.body);
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
            Let { sym, init, .. } => {
                // Make sure all variables used in the initialization expression are defined.
                // This must be called before we add the variable to the scope, otherwise you could
                // define the variable in terms of itself, which is not allowed.
                self.visit_expr(init);
                self.scopes.last_mut().unwrap().insert(*sym);
            }
            Assign { sym, val } => {
                self.validate_sym(sym, stmt.attr.span);
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
            _ => (),
        }
    }

    fn visit_expr(&mut self, e: &Expr) {
        use ExprKind::*;

        match &e.kind {
            IntLiteral(_) => (),
            Identifier(sym) => self.validate_sym(sym, e.attr.span),
            UnaryOp { e, .. } => self.visit_expr(e),
            BinaryOp { lhs, rhs, .. } => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            Call { sym, args } => {
                self.validate_sym(sym, e.attr.span);

                for arg in args {
                    self.visit_expr(arg);
                }
            }
        }
    }
}

pub fn do_name_validation(items: &[Item]) -> bool {
    // Check if all the variables and functions are defined before they are used.

    let mut visitor = SymbolValidationVisitor {
        scopes: Vec::new(),
        undefined_symbols: Vec::new(),
    };

    visitor.visit_items(items);

    for (span, sym) in &visitor.undefined_symbols {
        println!("span: {:?}, sym: {:?}", span, sym);
    }

    visitor.undefined_symbols.is_empty()
}

struct TypeCheckVisitor {
    types: TypeInterner,
    scopes: Vec<HashMap<Symbol, TypeToken>>,
}

impl Visitor for TypeCheckVisitor {
    fn visit_items(&mut self, items: &[Item]) {
        todo!();
    }
}

pub fn type_check(items: &[Item]) -> bool {
    todo!()
}
