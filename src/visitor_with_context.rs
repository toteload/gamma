use crate::ast::*;

pub trait VisitorWithContext<Ctx> {
    fn on_items_enter(&mut self, ctx: &mut Ctx, items: &[Item]) {}
    fn on_items_leave(&mut self, ctx: &mut Ctx, items: &[Item]) {}

    fn on_item_enter(&mut self, ctx: &mut Ctx, item: &Item) {}
    fn on_item_leave(&mut self, ctx: &mut Ctx, item: &Item) {}

    fn on_function_enter(&mut self, ctx: &mut Ctx, function: &Item) {}
    fn on_function_leave(&mut self, ctx: &mut Ctx, function: &Item) {}

    fn on_type_enter(&mut self, ctx: &mut Ctx, ty: &Type) {}

    fn on_block_enter(&mut self, ctx: &mut Ctx, block: &Block) {}
    fn on_block_leave(&mut self, ctx: &mut Ctx, block: &Block) {}

    fn on_statement_enter(&mut self, ctx: &mut Ctx, statement: &Statement) {}
    fn on_statement_leave(&mut self, ctx: &mut Ctx, statement: &Statement) {}

    fn on_expression_enter(&mut self, ctx: &mut Ctx, expression: &Expression) {}
    fn on_expression_leave(&mut self, ctx: &mut Ctx, expression: &Expression) {}

    fn visit_items(&mut self, ctx: &mut Ctx, items: &[Item]) {
        self.on_items_enter(ctx, items);

        for item in items {
            self.visit_item(ctx, item);
        }

        self.on_items_leave(ctx, items);
    }

    fn visit_item(&mut self, ctx: &mut Ctx, item: &Item) {
        self.on_item_enter(ctx, item);

        match &item.kind {
            ItemKind::Function {
                return_type,
                params,
                ..
            } => {
                self.on_type_enter(ctx, return_type);

                for param in params.iter() {
                    self.on_type_enter(ctx, &param.ty)
                }

                self.visit_function(ctx, item);
            }
            ItemKind::ExternalFunction {
                return_type,
                params,
                ..
            } => {
                self.on_type_enter(ctx, return_type);

                for param in params.iter() {
                    self.on_type_enter(ctx, &param.ty)
                }
            }
            ItemKind::Layout { fields, .. } => {
                for Field { ty, .. } in fields.iter() {
                    self.on_type_enter(ctx, ty);
                }
            }
        }

        self.on_item_leave(ctx, item);
    }

    fn visit_function(&mut self, ctx: &mut Ctx, function: &Item) {
        let ItemKind::Function { body, .. } = &function.kind else {
            unreachable!()
        };

        self.on_function_enter(ctx, function);

        self.visit_block(ctx, body);

        self.on_function_leave(ctx, function);
    }

    fn visit_block(&mut self, ctx: &mut Ctx, block: &Block) {
        self.on_block_enter(ctx, block);

        for statement in &block.statements {
            self.visit_statement(ctx, statement);
        }

        self.on_block_leave(ctx, block);
    }

    fn visit_statement(&mut self, ctx: &mut Ctx, statement: &Statement) {
        use StatementKind::*;

        self.on_statement_enter(ctx, statement);

        match &statement.kind {
            Let { ty, init, .. } => {
                self.on_type_enter(ctx, ty);
                if let Some(e) = init {
                    self.visit_expression(ctx, e);
                }
            }
            Expression(e) | Return(Some(e)) => self.visit_expression(ctx, e),
            Set { dst, val } => {
                self.visit_expression(ctx, dst);
                self.visit_expression(ctx, val);
            }
            If {
                cond,
                then,
                otherwise,
            } => {
                self.visit_expression(ctx, cond);
                self.visit_block(ctx, then);
                if let Some(otherwise) = otherwise {
                    self.visit_block(ctx, otherwise);
                }
            }
            Loop(block, _) => self.visit_block(ctx, block),
            _ => (),
        }

        self.on_statement_leave(ctx, statement);
    }

    fn visit_expression(&mut self, ctx: &mut Ctx, expression: &Expression) {
        use ExpressionKind::*;

        self.on_expression_enter(ctx, expression);

        match &expression.kind {
            BuiltinOp { args, .. } | Call { args, .. } => {
                for arg in args {
                    self.visit_expression(ctx, arg);
                }
            }
            Cast { ty, e } => {
                self.on_type_enter(ctx, ty);
                self.visit_expression(ctx, e);
            }
            Access { base, accessors } => {
                self.visit_expression(ctx, base);
                for accessor in accessors {
                    if let Accessor::Expr(e) = accessor {
                        self.visit_expression(ctx, e);
                    }
                }
            }
            IntLiteral(_) | BoolLiteral(_) | Identifier(_) => {}
        }

        self.on_expression_leave(ctx, expression);
    }
}
