use crate::{ast::*, visitor_mut_with_context::VisitorMutWithContext};

struct AstTransformer {}

struct AstTransformerContext<'a> {
    id_generator: &'a NodeIdGenerator,
}

impl VisitorMutWithContext<AstTransformerContext<'_>> for AstTransformer {
    fn on_expression_enter(
        &mut self,
        ctx: &mut AstTransformerContext,
        expression: &mut Expression,
    ) {
        match &mut expression.kind {
            ExpressionKind::Access { base, accessors } if accessors.is_empty() => {
                accessors.push(Accessor::Expr(Expression {
                    id: ctx.id_generator.gen_id(),
                    kind: ExpressionKind::IntLiteral(0),
                }));
            }
            _ => {}
        }
    }
}

pub fn desugar_ast(items: &mut [Item], id_generator: &NodeIdGenerator) {
    let mut ctx = AstTransformerContext { id_generator };

    let mut transformer = AstTransformer {};

    transformer.visit_items(&mut ctx, items);
}
