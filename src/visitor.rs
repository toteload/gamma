use crate::ast::*;

pub trait Visitor {
    fn on_items_enter(&mut self, items: &[Item]) {}
    fn on_items_leave(&mut self, items: &[Item]) {}

    fn on_item_enter(&mut self, item: &Item) {}
    fn on_item_leave(&mut self, item: &Item) {}

    fn on_function_enter(&mut self, function: &Item) {}
    fn on_function_leave(&mut self, function: &Item) {}

    fn on_type_enter(&mut self, ty: &Type) {}

    fn on_block_enter(&mut self, block: &Block) {}
    fn on_block_leave(&mut self, block: &Block) {}

    fn on_statement_enter(&mut self, statement: &Statement) {}
    fn on_statement_leave(&mut self, statement: &Statement) {}

    fn on_expression_enter(&mut self, expression: &Expression) {}
    fn on_expression_leave(&mut self, expression: &Expression) {}

    fn visit_items(&mut self, items: &[Item]) {
        self.on_items_enter(items);

        for item in items {
            self.visit_item(item);
        }

        self.on_items_leave(items);
    }

    fn visit_item(&mut self, item: &Item) {
        self.on_item_enter(item);

        match &item.kind {
            ItemKind::Function {
                return_type,
                params,
                ..
            } => {
                self.on_type_enter(return_type);

                for param in params.iter() {
                    self.on_type_enter(&param.ty)
                }

                self.visit_function(item);
            }
            ItemKind::ExternalFunction {
                return_type,
                params,
                ..
            } => {
                self.on_type_enter(return_type);

                for param in params.iter() {
                    self.on_type_enter(&param.ty)
                }
            }
            ItemKind::Layout { fields, .. } => {
                for Field { ty, .. } in fields.iter() {
                    self.on_type_enter(ty);
                }
            }
        }

        self.on_item_leave(item);
    }

    fn visit_function(&mut self, function: &Item) {
        let ItemKind::Function { body, .. } = &function.kind else {
            unreachable!()
        };

        self.on_function_enter(function);

        self.visit_block(body);

        self.on_function_leave(function);
    }

    fn visit_block(&mut self, block: &Block) {
        self.on_block_enter(block);

        for statement in &block.statements {
            self.visit_statement(statement);
        }

        self.on_block_leave(block);
    }

    fn visit_statement(&mut self, statement: &Statement) {
        use StatementKind::*;

        self.on_statement_enter(statement);

        match &statement.kind {
            Let { ty, init, .. } => {
                self.on_type_enter(ty);
                if let Some(e) = init {
                    self.visit_expression(e);
                }
            }
            Expression(e) | Return(Some(e)) => self.visit_expression(e),
            Set { dst, val } => {
                self.visit_expression(dst);
                self.visit_expression(val);
            }
            If {
                cond,
                then,
                otherwise,
            } => {
                self.visit_expression(cond);
                self.visit_block(then);
                if let Some(otherwise) = otherwise {
                    self.visit_block(otherwise);
                }
            }
            Loop(block, _) => self.visit_block(block),
            _ => (),
        }

        self.on_statement_leave(statement);
    }

    fn visit_expression(&mut self, expression: &Expression) {
        use ExpressionKind::*;

        self.on_expression_enter(expression);

        match &expression.kind {
            BuiltinOp { args, .. } | Call { args, .. } => {
                for arg in args {
                    self.visit_expression(arg);
                }
            }
            Cast { ty, e } => {
                self.on_type_enter(ty);
                self.visit_expression(e);
            }
            Access { base, accessors } => {
                self.visit_expression(base);
                for accessor in accessors {
                    if let Accessor::Expr(e) = accessor {
                        self.visit_expression(e);
                    }
                }
            }
            IntLiteral(_) | BoolLiteral(_) | Identifier(_) => {}
        }

        self.on_expression_leave(expression);
    }
}
