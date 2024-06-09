use crate::{
    ast::{self, *},
    ast_helpers::get_typetoken_of_typekind,
    error::*,
    string_interner::Symbol,
    type_interner::{TypeInterner, TypeToken},
    types::{Layout, LayoutField, Type},
    visitor_with_context::VisitorWithContext,
};
use std::collections::HashMap;

struct TypeNodeAnnotater {
    userdefined_type_refs: Vec<(NodeId, TypeKind)>,
    errors: Vec<Error>,
}

fn resolve_layout(
    typetokens: &TypeInterner,
    typetable: &mut HashMap<Symbol, TypeToken>,
    ast_types: &mut AstMap<TypeToken>,
    to_resolve: &Symbol,
    layouts: &HashMap<Symbol, (NodeId, &[Field])>,
    history: &mut Vec<Symbol>,
) -> Result<TypeToken, Vec<Symbol>> {
    if let Some(tok) = typetable.get(to_resolve) {
        return Ok(*tok);
    }

    if let Some(idx) = history.iter().position(|x| x == to_resolve) {
        return Err(history[idx..].to_vec());
    }

    history.push(*to_resolve);

    let Some((id, ast_fields)) = layouts.get(to_resolve) else {
        todo!("symbol {} should be for a layout", to_resolve.0)
    };

    fn find_type_identifier(ty: &TypeKind) -> Option<Symbol> {
        use TypeKind::*;

        match ty {
            Internal => None,
            Identifier(sym) => Some(*sym),
            Pointer(base) | Array(_, base) => find_type_identifier(base),
        }
    }

    for field in ast_fields.iter() {
        if let Some(sym) = find_type_identifier(&field.ty.kind) {
            let tok = resolve_layout(typetokens, typetable, ast_types, &sym, layouts, history)?;
            ast_types.insert(field.ty.id, tok);
        }
    }

    history.pop();

    let fields = ast_fields
        .iter()
        .map(|field| LayoutField {
            name: field.name.sym,
            offset: field.offset,
            ty: *ast_types.get(&field.ty.id).unwrap(),
        })
        .collect();

    let layout_type = Type::Layout(Layout { fields });
    let tok = typetokens.add(layout_type);

    typetable.insert(*to_resolve, tok);
    ast_types.insert(*id, tok);

    Ok(tok)
}

fn annotate_functions(
    typetokens: &TypeInterner,
    ast_types: &mut AstMap<TypeToken>,
    items: &[Item],
) {
    for item in items {
        match &item.kind {
            ItemKind::Function {
                return_type,
                params,
                ..
            }
            | ItemKind::ExternalFunction {
                return_type,
                params,
                ..
            } => {
                let param_typetokens = params
                    .iter()
                    .map(|param| *ast_types.get(&param.ty.id).unwrap())
                    .collect::<Vec<_>>();
                let return_type = *ast_types.get(&return_type.id).unwrap();
                let function_type = Type::Function {
                    params: param_typetokens,
                    return_type,
                };
                let function_typetoken = typetokens.add(function_type);
                ast_types.insert(item.id, function_typetoken);
            }
            _ => {}
        }
    }
}

impl TypeNodeAnnotater {
    fn new() -> TypeNodeAnnotater {
        TypeNodeAnnotater {
            userdefined_type_refs: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn try_annotate_type_node(
        &mut self,
        typetokens: &TypeInterner,
        typetable: &HashMap<Symbol, TypeToken>,
        ast_types: &mut AstMap<TypeToken>,
        ty: &ast::Type,
    ) {
        match get_typetoken_of_typekind(typetokens, typetable, &ty.kind) {
            Ok(typetoken) => _ = ast_types.insert(ty.id, typetoken),
            Err(_) => self.userdefined_type_refs.push((ty.id, ty.kind.clone())),
        }
    }

    fn annotate_simple_type_nodes(
        &mut self,
        typetokens: &TypeInterner,
        typetable: &HashMap<Symbol, TypeToken>,
        ast_types: &mut AstMap<TypeToken>,
        items: &[Item],
    ) {
        let mut ctx = Context {
            typetokens,
            typetable,
            ast_types,
        };

        self.visit_items(&mut ctx, items);
    }

    fn resolve_typenodes_with_userdefined_types(
        &mut self,
        typetokens: &TypeInterner,
        ast_types: &mut AstMap<TypeToken>,
        typetable: &mut HashMap<Symbol, TypeToken>,
        items: &[Item],
    ) {
        // The only user defined types are layouts, so when we have resolved all layouts we should
        // be able find a type for each unrecognized type symbol.

        let mut layouts: HashMap<Symbol, (NodeId, &[Field])> = HashMap::new();

        for item in items {
            if let ItemKind::Layout { name, fields } = &item.kind {
                layouts.insert(name.sym, (item.id, &fields));
            }
        }

        let mut history = Vec::new();
        for layout in layouts.keys() {
            history.clear();

            if let Err(cycle) = resolve_layout(
                typetokens,
                typetable,
                ast_types,
                layout,
                &layouts,
                &mut history,
            ) {
                todo!()
            }
        }

        for (id, tykind) in self.userdefined_type_refs.iter() {
            let Ok(tok) = get_typetoken_of_typekind(typetokens, typetable, &tykind) else {
                todo!()
            };
            ast_types.insert(*id, tok);
        }
    }
}

struct Context<'a> {
    typetokens: &'a TypeInterner,
    typetable: &'a HashMap<Symbol, TypeToken>,
    ast_types: &'a mut AstMap<TypeToken>,
}

impl VisitorWithContext<Context<'_>> for TypeNodeAnnotater {
    fn on_type_enter(&mut self, ctx: &mut Context, ty: &ast::Type) {
        self.try_annotate_type_node(ctx.typetokens, ctx.typetable, ctx.ast_types, ty);
    }
}

pub fn annotate_type_nodes(
    items: &[Item],
    typetokens: &mut TypeInterner,
    ast_types: &mut AstMap<TypeToken>,
    typetable: &mut HashMap<Symbol, TypeToken>,
) -> Result<(), Vec<Error>> {
    let mut annotater = TypeNodeAnnotater::new();

    annotater.annotate_simple_type_nodes(typetokens, typetable, ast_types, items);
    annotater.resolve_typenodes_with_userdefined_types(typetokens, ast_types, typetable, items);

    if !annotater.errors.is_empty() {
        todo!()
    }

    annotate_functions(typetokens, ast_types, items);

    Ok(())
}
