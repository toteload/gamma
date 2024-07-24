use crate::{
    ast::*,
    ast_helpers::{type_of_node, typetoken_of_node},
    error::*,
    scope_stack::ScopeStack,
    string_interner::Symbol,
    type_interner::{TypeInterner, TypeToken},
    types::Type,
    visitor_with_context::VisitorWithContext,
};
use std::collections::HashMap;

fn verify_access(
    typetokens: &TypeInterner,
    ast_types: &AstMap<TypeToken>,
    t: TypeToken,
    accessor: &Accessor,
) -> Result<TypeToken, Error> {
    let ty = typetokens.get(&t);

    match (ty, accessor) {
        (Type::Pointer(t) | Type::Array(_, t), Accessor::Expr(e)) => {
            if type_of_node(typetokens, ast_types, &e.id).is_integer() {
                Ok(*t)
            } else {
                todo!()
            }
        }
        (Type::Layout(layout), Accessor::Field(field)) => {
            if let Some(accessed_field) = layout.fields.iter().find(|f| f.name == field.field) {
                Ok(accessed_field.ty)
            } else {
                todo!()
            }
        }
        _ => {
            todo!()
        }
    }
}

struct TypeAnnotaterContext<'a> {
    typetokens: &'a TypeInterner,
    typetable: &'a HashMap<Symbol, TypeToken>,
    ast_types: &'a mut AstMap<TypeToken>,
}

struct TypeAnnotater {
    scopes: ScopeStack<Symbol, TypeToken>,
    errors: Vec<Error>,
}

impl TypeAnnotater {
    fn new() -> TypeAnnotater {
        TypeAnnotater {
            scopes: ScopeStack::new(),
            errors: Vec::new(),
        }
    }

    fn get_typetoken_of_identifier(&self, identifier: &Symbol) -> TypeToken {
        self.scopes
            .get(identifier)
            .copied()
            .unwrap_or_else(|| panic!("Symbol {:?} should be found in scope", identifier))
    }
}

impl VisitorWithContext<TypeAnnotaterContext<'_>> for TypeAnnotater {
    fn on_items_enter(&mut self, ctx: &mut TypeAnnotaterContext, items: &[Item]) {
        // Add all functions and external functions as globally accessible variables.

        let mut global_scope = HashMap::new();

        for item in items {
            match &item.kind {
                ItemKind::Function { name, .. } | ItemKind::ExternalFunction { name, .. } => {
                    global_scope.insert(
                        name.sym,
                        ctx.ast_types
                            .get(&item.id)
                            .copied()
                            .expect("Item should have a type"),
                    );
                }
                _ => {}
            }
        }

        self.scopes.push_scope(global_scope);
    }

    fn on_function_enter(&mut self, ctx: &mut TypeAnnotaterContext, function: &Item) {
        let ItemKind::Function { params, .. } = &function.kind else {
            unreachable!()
        };

        let param_scope = params
            .iter()
            .map(|p| (p.name.sym, *ctx.ast_types.get(&p.ty.id).expect("")))
            .collect();

        self.scopes.push_scope(param_scope);
    }

    fn on_function_leave(&mut self, ctx: &mut TypeAnnotaterContext, _: &Item) {
        self.scopes.pop();
    }

    fn on_block_enter(&mut self, ctx: &mut TypeAnnotaterContext, _: &Block) {
        self.scopes.push_empty_scope();
    }

    fn on_block_leave(&mut self, ctx: &mut TypeAnnotaterContext, _: &Block) {
        self.scopes.pop();
    }

    fn on_statement_enter(&mut self, ctx: &mut TypeAnnotaterContext, statement: &Statement) {
        use StatementKind::*;

        match statement.kind {
            Let {
                name: Name { sym, .. },
                ty: crate::ast::Type { id, .. },
                ..
            } => {
                self.scopes.insert(sym, *ctx.ast_types.get(&id).unwrap());
            }
            _ => {}
        }
    }

    fn on_expression_leave(&mut self, ctx: &mut TypeAnnotaterContext, expression: &Expression) {
        use BuiltinOpKind::*;
        use ExpressionKind::*;

        let typetoken: TypeToken = match &expression.kind {
            IntLiteral(_) => ctx.typetokens.add(Type::IntConstant),
            BoolLiteral(_) => ctx.typetokens.add(Type::Bool),
            Identifier(sym) => self.scopes.get(sym).copied().expect(""),
            Access { base, accessors } => {
                let base_type = typetoken_of_node(ctx.ast_types, &base.id);

                if accessors.is_empty() {
                    let ty = ctx.typetokens.get(&base_type);
                    if let Type::Pointer(inner) = ty {
                        *inner
                    } else {
                        todo!()
                    }
                } else {
                    let mut tok = base_type;
                    for accessor in accessors.iter() {
                        match verify_access(ctx.typetokens, ctx.ast_types, tok, accessor) {
                            Ok(t) => tok = t,
                            Err(err) => {
                                self.errors.push(err);
                                break;
                            }
                        }
                    }
                    tok
                }
            }
            Cast { ty, e } => *ctx.ast_types.get(&ty.id).expect(""),
            BuiltinOp { op, args } => match op {
                Not | Or | And | Equals | NotEquals | LessThan | GreaterThan | LessEquals
                | GreaterEquals => ctx.typetokens.add(Type::Bool),
                AddressOf => {
                    let arg = &args[0];
                    let ty = ctx.ast_types.get(&arg.id).expect("");
                    ctx.typetokens.add(Type::Pointer(*ty))
                }
                Xor | BitwiseAnd | BitwiseOr | Add | Sub | Mul | Div | Remainder => args
                    .iter()
                    .map(|arg| ctx.ast_types.get(&arg.id).expect(""))
                    .find(|arg_type| matches!(ctx.typetokens.get(arg_type), Type::Int { .. }))
                    .copied()
                    .unwrap_or(ctx.typetokens.add(Type::IntConstant)),
            },
            Call { name, args } => {
                let function_type = ctx
                    .typetokens
                    .get(&self.get_typetoken_of_identifier(&name.sym));
                let Type::Function { return_type, .. } = function_type else {
                    panic!()
                };

                *return_type
            }
        };

        ctx.ast_types.insert(expression.id, typetoken);
    }
}

struct AccessAnnotaterContext<'a> {
    typetokens: &'a TypeInterner,
    ast_types: &'a AstMap<TypeToken>,
}

struct AccessAnnotater {
    errors: Vec<Error>,
}

impl AccessAnnotater {
    fn new() -> AccessAnnotater {
        AccessAnnotater { errors: Vec::new() }
    }
}

//impl VisitorWithContext<AccessAnnotaterContext<'_>> for AccessAnnotater {
//    fn on_expression_leave(&mut self, ctx: AccessAnnotaterContext, expression: &Expression) {
//        use ExpressionKind::*;
//
//        match &expression.kind {
//        }
//    }
//}

pub fn type_annotate(
    items: &[Item],
    typetokens: &mut TypeInterner,
    ast_types: &mut AstMap<TypeToken>,
    typetable: &mut HashMap<Symbol, TypeToken>,
) -> Result<(), Vec<Error>> {
    {
        let mut annotater = TypeAnnotater::new();
        let mut ctx = TypeAnnotaterContext {
            typetokens,
            typetable,
            ast_types,
        };
        annotater.visit_items(&mut ctx, items);
        if !annotater.errors.is_empty() {
            return Err(annotater.errors);
        }
    }

    Ok(())
}
