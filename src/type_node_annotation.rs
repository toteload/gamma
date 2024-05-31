use crate::{
    ast::{self, *},
    error::*,
    string_interner::Symbol,
    types::{Layout, LayoutField, Type, TypeInterner, TypeToken},
    visitor::Visitor,
};
use std::collections::HashMap;

// Annotates all the Item and Type nodes
struct TypeNodeAnnotater<'a> {
    typetokens: &'a mut TypeInterner,
    ast_types: &'a mut AstMap<TypeToken>,
    typetable: &'a mut HashMap<Symbol, TypeToken>,
    userdefined_type_refs: Vec<(NodeId, Symbol)>,
    errors: Vec<Error>,
}

impl TypeNodeAnnotater<'_> {
    fn new<'a>(
        typetokens: &'a mut TypeInterner,
        ast_types: &'a mut AstMap<TypeToken>,
        typetable: &'a mut HashMap<Symbol, TypeToken>,
    ) -> TypeNodeAnnotater<'a> {
        TypeNodeAnnotater {
            typetokens,
            typetable,
            ast_types,
            userdefined_type_refs: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn get_typetoken_of_typekind(&mut self, kind: &TypeKind) -> Result<TypeToken, Symbol> {
        match kind {
            TypeKind::Identifier(sym) => self.typetable.get(sym).copied().ok_or(*sym),
            TypeKind::Pointer(inner) => {
                let inner = self.get_typetoken_of_typekind(inner)?;
                Ok(self.typetokens.add(Type::Pointer(inner)))
            }
            TypeKind::Array(size, inner) => {
                let inner = self.get_typetoken_of_typekind(inner)?;
                Ok(self.typetokens.add(Type::Array(*size, inner)))
            }
            _ => unreachable!(),
        }
    }

    fn try_annotate_type_node(&mut self, ty: &ast::Type) {
        match self.get_typetoken_of_typekind(&ty.kind) {
            Ok(typetoken) => _ = self.ast_types.insert(ty.id, typetoken),
            Err(sym) => self.userdefined_type_refs.push((ty.id, sym)),
        }
    }

    fn resolve_layout(
        &mut self,
        to_resolve: &Symbol,
        layouts: &HashMap<Symbol, (NodeId, &[Field])>,
        history: &mut Vec<Symbol>,
    ) -> Result<(), Vec<Symbol>> {
        if self.typetable.contains_key(to_resolve) {
            return Ok(());
        }

        if let Some(idx) = history.iter().position(|x| x == to_resolve) {
            return Err(history[idx..].to_vec());
        }

        history.push(*to_resolve);

        let Some((id, ast_fields)) = layouts.get(to_resolve) else {
            todo!()
        };

        for field in ast_fields.iter() {
            self.resolve_layout(&field.name.sym, layouts, history)?;
        }

        history.pop();

        let fields = ast_fields
            .iter()
            .map(|field| {
                let name = field.name.sym;

                LayoutField {
                    name,
                    offset: field.offset,
                    ty: *self.typetable.get(&name).unwrap(),
                }
            })
            .collect();
        let layout_type = Type::Layout(Layout { fields });
        let tok = self.typetokens.add(layout_type);

        self.typetable.insert(*to_resolve, tok);

        return Ok(());
    }

    fn resolve_typenodes_with_userdefined_types(&mut self, items: &[Item]) {
        let mut layouts: HashMap<Symbol, (NodeId, &[Field])> = HashMap::new();

        for item in items {
            if let ItemKind::Layout { name, fields } = &item.kind {
                layouts.insert(name.sym, (item.id, &fields));
            }
        }

        let mut history = Vec::new();
        for layout in layouts.keys() {
            history.clear();

            if let Err(cycle) = self.resolve_layout(layout, &layouts, &mut history) {
                todo!()
            }
        }

        for (id, sym) in self.userdefined_type_refs.iter() {
            self.ast_types
                .insert(*id, *self.typetable.get(sym).unwrap());
        }
    }

    fn annotate_functions(&mut self, items: &[Item]) {
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
                        .map(|param| *self.ast_types.get(&param.ty.id).unwrap())
                        .collect::<Vec<_>>();
                    let return_type = *self.ast_types.get(&return_type.id).unwrap();
                    let function_type = Type::Function {
                        params: param_typetokens,
                        return_type,
                    };
                    let function_typetoken = self.typetokens.add(function_type);
                    self.ast_types.insert(item.id, function_typetoken);
                }
                _ => {}
            }
        }
    }
}

impl Visitor for TypeNodeAnnotater<'_> {
    fn on_type_enter(&mut self, ty: &ast::Type) {
        self.try_annotate_type_node(ty);
    }
}

pub fn annotate_type_nodes(
    items: &[Item],
    typetokens: &mut TypeInterner,
    ast_types: &mut AstMap<TypeToken>,
    typetable: &mut HashMap<Symbol, TypeToken>,
) -> Result<(), Vec<Error>> {
    let mut annotater = TypeNodeAnnotater::new(typetokens, ast_types, typetable);

    annotater.visit_items(items);

    annotater.resolve_typenodes_with_userdefined_types(items);

    if !annotater.errors.is_empty() {
        todo!()
    }

    annotater.annotate_functions(items);

    Ok(())
}
