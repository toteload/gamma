use crate::{
    ast::{self, *},
    error::Error,
    scope_stack::ScopeStack,
    string_interner::Symbol,
    types::{Type, TypeInterner, TypeToken},
    visitor_mut::VisitorMut,
};
use std::collections::HashMap;

#[rustfmt::skip]
struct TypeCoercer<'a> {
    typetokens:   &'a mut TypeInterner,
    ast_types:    &'a mut AstMap<TypeToken>,
    typetable:    &'a mut HashMap<Symbol, TypeToken>,
    id_generator: &'a mut NodeIdGenerator,

    scopes:               ScopeStack<Symbol, TypeToken>,
    errors:               Vec<Error>,
    declared_return_type: Option<TypeToken>,
}

impl TypeCoercer<'_> {
    #[rustfmt::skip]
    fn new<'a>(
        typetokens:   &'a mut TypeInterner,
        ast_types:    &'a mut AstMap<TypeToken>,
        typetable:    &'a mut HashMap<Symbol, TypeToken>,
        id_generator: &'a mut NodeIdGenerator,
    ) -> TypeCoercer<'a> {
        TypeCoercer {
            typetokens,
            ast_types,
            typetable,
            id_generator,

            scopes: ScopeStack::new(),
            errors: Vec::new(),
            declared_return_type: None,
        }
    }

    fn is_int_const(&self, id: &NodeId) -> bool {
        matches!(
            self.typetokens.get(self.ast_types.get(id).expect("")),
            Type::IntConstant
        )
    }

    fn type_of_node(&self, id: &NodeId) -> &Type {
        self.typetokens.get(self.ast_types.get(id).expect(""))
    }

    fn coerce_expression(&mut self, dst_type: TypeToken, e: &mut Expression) {
        use ExpressionKind::*;

        match &mut e.kind {
            IntLiteral(_) => self.inject_coercion_type_cast(dst_type, e),
            BuiltinOp { op, args } => {
                for arg in args.iter_mut() {
                    self.coerce_expression(dst_type, arg);
                }

                if self.is_int_const(&e.id) {
                    self.inject_coercion_type_cast(dst_type, e);
                }
            }
            _ => {}
        }
    }

    fn inject_coercion_type_cast(&mut self, dst_type: TypeToken, e: &mut Expression) {
        let expr_id = self.id_generator.gen_id();
        let type_id = self.id_generator.gen_id();

        self.ast_types.insert(expr_id, dst_type);
        self.ast_types.insert(type_id, dst_type);

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
}

impl VisitorMut for TypeCoercer<'_> {
    fn on_function_enter(&mut self, function: &mut Item) {
        let Type::Function { return_type, .. } = self.type_of_node(&function.id) else {
            panic!()
        };
        self.declared_return_type = Some(*return_type);
    }

    fn visit_statement(&mut self, statement: &mut Statement) {
        use StatementKind::*;

        match &mut statement.kind {
            Let {
                ty,
                init: Some(init),
                ..
            } if self.is_int_const(&init.id) => {
                let dst_type = self.ast_types.get(&ty.id).expect("");
                self.coerce_expression(*dst_type, init);
            }
            Set { dst, val } if self.is_int_const(&val.id) => {
                todo!()
            }
            Return(Some(e)) if self.is_int_const(&e.id) => {
                self.coerce_expression(self.declared_return_type.unwrap(), e);
            }

            Let { init: Some(e), .. } => self.visit_expression(e),
            Set { dst, val } => {
                self.visit_expression(dst);
                self.visit_expression(val);
            }
            Expression(e) | Return(Some(e)) => self.visit_expression(e),
            Loop(block, _) => self.visit_block(block),
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
            _ => (),
        }
    }

    fn visit_expression(&mut self, expression: &mut Expression) {
        use BuiltinOpKind::*;
        use ExpressionKind::*;

        match &mut expression.kind {
            BuiltinOp { op, args } => match op {
                Equals | NotEquals | LessThan | GreaterThan | LessEquals | GreaterEquals | Add
                | Sub | Mul | Div | Remainder => {
                    let Some(non_const_arg_typetok) = args
                        .iter()
                        .map(|arg| self.ast_types.get(&arg.id).expect(""))
                        .find(|ty| !matches!(self.typetokens.get(ty), Type::IntConstant))
                        .copied()
                    else {
                        panic!()
                    };

                    for arg in args.iter_mut() {
                        self.coerce_expression(non_const_arg_typetok, arg);
                    }
                }
                _ => todo!("op {op:?}"),
            },
            Call { .. } => todo!("implement function calls"),
            Cast { ty, e } => {
                self.on_type_enter(ty);
                self.visit_expression(e);
            }
            _ => {}
        }
    }
}

pub fn type_coerce(
    items: &mut [Item],
    typetokens: &mut TypeInterner,
    ast_types: &mut HashMap<NodeId, TypeToken>,
    typetable: &mut HashMap<Symbol, TypeToken>,
    id_generator: &mut NodeIdGenerator,
) -> Result<(), Vec<Error>> {
    let mut type_coercer = TypeCoercer::new(typetokens, ast_types, typetable, id_generator);

    type_coercer.visit_items(items);

    if !type_coercer.errors.is_empty() {
        return Err(type_coercer.errors);
    }

    Ok(())
}
