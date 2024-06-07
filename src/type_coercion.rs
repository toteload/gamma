use crate::{
    ast::{self, *},
    ast_helpers::{coerce_expression, is_int_const, type_of_node},
    error::Error,
    string_interner::Symbol,
    type_interner::{TypeInterner, TypeToken},
    types::{Type, U64},
    visitor_mut_with_context::VisitorMutWithContext,
};
use std::collections::HashMap;

#[rustfmt::skip]
struct Context<'a> {
    typetokens:   &'a TypeInterner,
    id_generator: &'a NodeIdGenerator,
    ast_types:    &'a mut AstMap<TypeToken>,
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

impl VisitorMutWithContext<Context<'_>> for TypeCoercer {
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
        let Type::Function { return_type, .. } =
            type_of_node(ctx.typetokens, ctx.ast_types, &function.id)
        else {
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
            } if is_int_const(ctx.typetokens, ctx.ast_types, &init.id) => {
                let dst_type = ctx.ast_types.get(&ty.id).expect("");
                coerce_expression(
                    ctx.id_generator,
                    ctx.typetokens,
                    ctx.ast_types,
                    *dst_type,
                    init,
                );
            }
            Set { dst, val } if is_int_const(ctx.typetokens, ctx.ast_types, &val.id) => {
                let dst_type = todo!();
                coerce_expression(ctx.id_generator, ctx.typetokens, ctx.ast_types, dst_type, val);
            }
            Return(Some(e)) if is_int_const(ctx.typetokens, ctx.ast_types, &e.id) => {
                coerce_expression(
                    ctx.id_generator,
                    ctx.typetokens,
                    ctx.ast_types,
                    self.declared_return_type.unwrap(),
                    e,
                );
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
                        coerce_expression(
                            ctx.id_generator,
                            ctx.typetokens,
                            ctx.ast_types,
                            non_const_arg_typetok,
                            arg,
                        );
                    }
                }
                At => {
                    if let [_, idx] = args.as_mut_slice() {
                        let u = ctx.typetokens.add(U64);
                        coerce_expression(ctx.id_generator, ctx.typetokens, ctx.ast_types, u, idx);
                    }
                }
                AddressOf => {}
                _ => todo!("op {op:?}"),
            },
            Call { name, args } => {
                let Type::Function { params, .. } =
                    ctx.typetokens.get(self.functions.get(&name.sym).unwrap())
                else {
                    panic!()
                };

                for (ty, arg) in params.iter().zip(args.iter_mut()) {
                    coerce_expression(ctx.id_generator, ctx.typetokens, ctx.ast_types, *ty, arg);
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
    typetokens:   &TypeInterner,
    ast_types:    &mut HashMap<NodeId, TypeToken>,
    id_generator: &NodeIdGenerator,
) -> Result<(), Vec<Error>> {
    let mut ctx = Context {
        typetokens,
        ast_types,
        id_generator,
    };

    let mut type_coercer = TypeCoercer::new();

    type_coercer.visit_items(&mut ctx, items);

    if !type_coercer.errors.is_empty() {
        return Err(type_coercer.errors);
    }

    Ok(())
}
