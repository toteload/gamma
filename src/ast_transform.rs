struct AstTransformer { }

impl VisitorMut for AstTransformer {
    fn on_expression_enter(&mut self, expression: &mut Expression) {
        match &mut expression.kind {
            ExpressionKind::Access { base, accessors } if accessors.len() == 0 {
                accessors.push(Accessor::Expr(todo!()));
            }
        }
    }
}
