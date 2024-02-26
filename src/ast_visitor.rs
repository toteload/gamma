use crate::ast::*;

pub trait Visitor {
    fn visit_items(&mut self, items: &[Item]) {
        for item in items {
            self.visit_item(item);
        }
    }

    fn visit_item(&mut self, item: &Item) {
        match &item.kind {
            ItemKind::Function {
                return_type,
                name,
                params,
                body,
            } => self.visit_function(name, params, return_type, body),
            ItemKind::ExternalFunction {
                return_type,
                name,
                params,
            } => self.visit_external_function(name, params, return_type),
            ItemKind::Layout {
                name,
                align,
                fields,
            } => self.visit_layout(name, *align, fields),
        }
    }

    fn visit_function(
        &mut self,
        _name: &Name,
        _params: &[Param],
        _return_type: &crate::ast::Type,
        body: &Block,
    ) {
        self.visit_block(body);
    }

    fn visit_external_function(&mut self, name: &Name, params: &[Param], return_type: &Type) {}

    fn visit_layout(&mut self, name: &Name, align: u32, fields: &[Field]) {}

    fn visit_block(&mut self, block: &Block) {
        for statement in &block.statements {
            self.visit_statement(statement);
        }
    }

    fn visit_statement(&mut self, stmt: &Statement) {}

    fn visit_expr(&mut self, e: &Expr) {}
}
