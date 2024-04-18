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
}

pub trait VisitorMut {
    fn on_items_enter(&mut self, items: &mut [Item]) {}
    fn on_items_leave(&mut self, items: &mut [Item]) {}

    fn on_item_enter(&mut self, item: &mut Item) {}
    fn on_item_leave(&mut self, item: &mut Item) {}

    fn on_function_enter(&mut self, function: &mut Item) {}
    fn on_function_leave(&mut self, function: &mut Item) {}

    fn on_type_enter(&mut self, ty: &mut Type) {}

    fn on_block_enter(&mut self, block: &mut Block) {}
    fn on_block_leave(&mut self, block: &mut Block) {}

    fn on_statement_enter(&mut self, statement: &mut Statement) {}
    fn on_statement_leave(&mut self, statement: &mut Statement) {}

    fn on_expression_enter(&mut self, expression: &mut Expression) {}
    fn on_expression_leave(&mut self, expression: &mut Expression) {}
}

pub fn visit<T: Visitor>(visitor: &mut T, items: &[Item]) {
    visitor.on_items_enter(items);
    for item in items {
        visit_item(visitor, item);
    }
    visitor.on_items_leave(items);
}

fn visit_item<T: Visitor>(visitor: &mut T, item: &Item) {
    visitor.on_item_enter(item);
    match &item.kind {
        ItemKind::Function { .. } => visit_function(visitor, item),
        _ => (),
    }
    visitor.on_item_leave(item);
}

fn visit_function<T: Visitor>(visitor: &mut T, function: &Item) {
    let ItemKind::Function { body, .. } = &function.kind else {
        unreachable!()
    };

    visitor.on_function_enter(function);
    visit_block(visitor, &body);
    visitor.on_function_leave(function);
}
fn visit_block<T: Visitor>(visitor: &mut T, block: &Block) {
    visitor.on_block_enter(block);
    for statement in &block.statements {
        visit_statement(visitor, statement);
    }
    visitor.on_block_leave(block);
}

fn visit_statement<T: Visitor>(visitor: &mut T, statement: &Statement) {
    use StatementKind::*;

    visitor.on_statement_enter(statement);

    match &statement.kind {
        Let { init: Some(e), .. } | Expression(e) | Return(Some(e)) => visit_expression(visitor, e),
        Set { dst, val } => {
            visit_expression(visitor, dst);
            visit_expression(visitor, val);
        }
        If {
            cond,
            then,
            otherwise,
        } => {
            visit_expression(visitor, cond);
            visit_block(visitor, then);
            if let Some(otherwise) = otherwise {
                visit_block(visitor, otherwise);
            }
        }
        Loop(block, _) => visit_block(visitor, block),
        _ => (),
    }

    visitor.on_statement_leave(statement);
}

fn visit_expression<T: Visitor>(visitor: &mut T, expression: &Expression) {
    use ExpressionKind::*;

    visitor.on_expression_enter(expression);

    match &expression.kind {
        BuiltinOp { args, .. } | Call { args, .. } => {
            for arg in args {
                visit_expression(visitor, arg);
            }
        }
        Cast { e, .. } => visit_expression(visitor, e),
        _ => (),
    }

    visitor.on_expression_leave(expression);
}
