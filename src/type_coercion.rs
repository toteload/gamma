use crate::{
    ast::{self, *},
    error::Error,
    string_interner::Symbol,
    types::{Type, TypeInterner, TypeToken, U64},
    visitor_mut::VisitorMut,
};
use std::collections::HashMap;

#[rustfmt::skip]
struct Context<'a> {
    typetokens:   &'a mut TypeInterner,
    ast_types:    &'a mut AstMap<TypeToken>,
    typetable:    &'a mut HashMap<Symbol, TypeToken>,
    id_generator: &'a mut NodeIdGenerator,
}

#[rustfmt::skip]
struct TypeCoercer {
    functions:            HashMap<Symbol, TypeToken>,
    errors:               Vec<Error>,
    declared_return_type: Option<TypeToken>,
}

impl TypeCoercer {
    fn new() -> TypeCoercer {
        TypeCoercer {
            functions: HashMap::new(),
            errors: Vec::new(),
            declared_return_type: None,
        }
    }
}

fn is_int_const(ctx: &Context, id: &NodeId) -> bool {
    matches!(
        ctx.typetokens.get(ctx.ast_types.get(id).expect("")),
        Type::IntConstant
    )
}

fn type_of_node<'a>(ctx: &'a Context, id: &NodeId) -> &'a Type {
    ctx.typetokens.get(ctx.ast_types.get(id).expect(""))
}

fn coerce_expression(ctx: &mut Context, dst_type: TypeToken, e: &mut Expression) {
    use ExpressionKind::*;

    match &mut e.kind {
        IntLiteral(_) => inject_coercion_type_cast(ctx, dst_type, e),
        BuiltinOp { op, args } => {
            for arg in args.iter_mut() {
                coerce_expression(ctx, dst_type, arg);
            }

            if is_int_const(ctx, &e.id) {
                inject_coercion_type_cast(ctx, dst_type, e);
            }
        }
        _ => {}
    }
}

fn inject_coercion_type_cast(ctx: &mut Context, dst_type: TypeToken, e: &mut Expression) {
    let expr_id = ctx.id_generator.gen_id();
    let type_id = ctx.id_generator.gen_id();

    ctx.ast_types.insert(expr_id, dst_type);
    ctx.ast_types.insert(type_id, dst_type);

    unsafe {
        let tmp = Box::new(std::ptr::read(e));

        let cast = ast::Expression {
            id: expr_id,
            kind: ExpressionKind::Cast {
                ty: ast::Type {
                    id: type_id,
                    kind: TypeKind::Internal,
                },
                e: tmp,
            },
        };

        std::ptr::write(e, cast);
    }
}

impl VisitorMut<Context<'_>> for TypeCoercer {
    fn on_items_enter(&mut self, ctx: &mut Context, items: &mut [Item]) {
        for item in items {
            match &item.kind {
                ItemKind::Function { name, .. } | ItemKind::ExternalFunction { name, .. } => {
                    self.functions
                        .insert(name.sym, *ctx.ast_types.get(&item.id).unwrap());
                }
                _ => {}
            }
        }
    }

    fn on_function_enter(&mut self, ctx: &mut Context, function: &mut Item) {
        let Type::Function { return_type, .. } = type_of_node(ctx, &function.id) else {
            panic!()
        };
        self.declared_return_type = Some(*return_type);
    }

    fn visit_statement(&mut self, ctx: &mut Context, statement: &mut Statement) {
        use StatementKind::*;

        match &mut statement.kind {
            Let {
                ty,
                init: Some(init),
                ..
            } if is_int_const(ctx, &init.id) => {
                let dst_type = ctx.ast_types.get(&ty.id).expect("");
                coerce_expression(ctx, *dst_type, init);
            }
            Set { dst, val } if is_int_const(ctx, &val.id) => {
                todo!()
            }
            Return(Some(e)) if is_int_const(ctx, &e.id) => {
                coerce_expression(ctx, self.declared_return_type.unwrap(), e);
            }

            Let { init: Some(e), .. } => self.visit_expression(ctx, e),
            Set { dst, val } => {
                self.visit_expression(ctx, dst);
                self.visit_expression(ctx, val);
            }
            Expression(e) | Return(Some(e)) => self.visit_expression(ctx, e),
            Loop(block, _) => self.visit_block(ctx, block),
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
            _ => (),
        }
    }

    fn visit_expression(&mut self, ctx: &mut Context, expression: &mut Expression) {
        use BuiltinOpKind::*;
        use ExpressionKind::*;

        match &mut expression.kind {
            BuiltinOp { op, args } => match op {
                Equals | NotEquals | LessThan | GreaterThan | LessEquals | GreaterEquals | Add
                | Sub | Mul | Div | Remainder => {
                    let Some(non_const_arg_typetok) = args
                        .iter()
                        .map(|arg| ctx.ast_types.get(&arg.id).expect(""))
                        .find(|ty| !matches!(ctx.typetokens.get(ty), Type::IntConstant))
                        .copied()
                    else {
                        panic!()
                    };

                    for arg in args.iter_mut() {
                        coerce_expression(ctx, non_const_arg_typetok, arg);
                    }
                }
                At => {
                    if let [_, idx] = args.as_mut_slice() {
                        let u = ctx.typetokens.add(U64);
                        coerce_expression(ctx, u, idx);
                    }
                }
                AddressOf => todo!("type coercion address of"),
                _ => todo!("op {op:?}"),
            },
            Call { name, args } => {
                let params = {
                    let Type::Function { params, .. } =
                        ctx.typetokens.get(self.functions.get(&name.sym).unwrap())
                    else {
                        panic!()
                    };
                    params.clone()
                };

                for (ty, arg) in params.iter().zip(args.iter_mut()) {
                    coerce_expression(ctx, *ty, arg);
                }
            }
            Cast { ty, e } => {
                self.on_type_enter(ctx, ty);
                self.visit_expression(ctx, e);
            }
            _ => {}
        }
    }
}

#[rustfmt::skip]
pub fn type_coerce(
    items:        &mut [Item],
    typetokens:   &mut TypeInterner,
    ast_types:    &mut HashMap<NodeId, TypeToken>,
    typetable:    &mut HashMap<Symbol, TypeToken>,
    id_generator: &mut NodeIdGenerator,
) -> Result<(), Vec<Error>> {
    let mut ctx = Context {
        typetokens,
        ast_types,
        typetable,
        id_generator,
    };

    let mut type_coercer = TypeCoercer::new();

    type_coercer.visit_items(&mut ctx, items);

    if !type_coercer.errors.is_empty() {
        return Err(type_coercer.errors);
    }

    Ok(())
}
