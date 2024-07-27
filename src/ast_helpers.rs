use crate::{
    ast::{self, *},
    string_interner::Symbol,
    type_interner::{TypeInterner, TypeToken},
    types::Type,
};
use std::collections::HashMap;

pub fn get_typetoken_of_typekind(
    typetokens: &TypeInterner,
    typetable: &HashMap<Symbol, TypeToken>,
    kind: &TypeKind,
) -> Result<TypeToken, Symbol> {
    match kind {
        TypeKind::Identifier(sym) => typetable.get(sym).copied().ok_or(*sym),
        TypeKind::Pointer(inner) => {
            let inner = get_typetoken_of_typekind(typetokens, typetable, inner)?;
            Ok(typetokens.add(Type::Pointer(inner)))
        }
        TypeKind::Array(size, inner) => {
            let inner = get_typetoken_of_typekind(typetokens, typetable, inner)?;
            Ok(typetokens.add(Type::Array(*size, inner)))
        }
        _ => unreachable!(),
    }
}

pub fn is_int_const(typetokens: &TypeInterner, ast_types: &AstMap<TypeToken>, id: &NodeId) -> bool {
    matches!(
        typetokens.get(ast_types.get(id).expect("")),
        Type::IntConstant
    )
}

pub fn typetoken_of_node(ast_types: &AstMap<TypeToken>, id: &NodeId) -> TypeToken {
    *ast_types
        .get(id)
        .expect("Node should have an associated TypeToken")
}

pub fn type_of_node<'a>(
    typetokens: &'a TypeInterner,
    ast_types: &AstMap<TypeToken>,
    id: &NodeId,
) -> &'a Type {
    typetokens.get(&typetoken_of_node(ast_types, id))
}

pub fn coerce_expression(
    id_generator: &NodeIdGenerator,
    typetokens: &TypeInterner,
    ast_types: &mut AstMap<TypeToken>,
    dst_type: TypeToken,
    e: &mut Expression,
) {
    use ExpressionKind::*;

    match &mut e.kind {
        IntLiteral(_) => inject_coercion_type_cast(id_generator, ast_types, dst_type, e),
        BuiltinOp { op, args } => {
            for arg in args.iter_mut() {
                coerce_expression(id_generator, typetokens, ast_types, dst_type, arg);
            }

            if is_int_const(typetokens, ast_types, &e.id) {
                inject_coercion_type_cast(id_generator, ast_types, dst_type, e);
            }
        }
        _ => {}
    }
}

pub fn inject_coercion_type_cast(
    id_generator: &NodeIdGenerator,
    ast_types: &mut AstMap<TypeToken>,
    dst_type: TypeToken,
    e: &mut Expression,
) {
    let expr_id = id_generator.gen_id();
    let type_id = id_generator.gen_id();

    ast_types.insert(expr_id, dst_type);
    ast_types.insert(type_id, dst_type);

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

fn box_interject<T, F: Fn(Box<T>) -> T>(b: &mut Box<T>, f: F) {
    unsafe {
        let p: &mut T = b;
        let cpy = Box::from_raw(p);
        let val = f(cpy);
        std::ptr::write(p, val);
    }
}
