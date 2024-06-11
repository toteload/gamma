use crate::{
    ast::*,
    error::*,
    scope_stack::ScopeStack,
    string_interner::Symbol,
    type_interner::{TypeInterner, TypeToken},
    types::Type,
    visitor::Visitor,
};
use std::collections::HashMap;

struct TypeAnnotater<'a> {
    typetokens: &'a TypeInterner,

    ast_types: &'a mut AstMap<TypeToken>,
    typetable: &'a HashMap<Symbol, TypeToken>,

    scopes: ScopeStack<Symbol, TypeToken>,
    errors: Vec<Error>,
}

impl TypeAnnotater<'_> {
    fn new<'a>(
        typetokens: &'a mut TypeInterner,
        ast_types: &'a mut AstMap<TypeToken>,
        typetable: &'a mut HashMap<Symbol, TypeToken>,
    ) -> TypeAnnotater<'a> {
        TypeAnnotater {
            typetokens,
            ast_types,
            typetable,
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

    fn get_type_of_identifier(&self, identifier: &Symbol) -> &Type {
        self.typetokens
            .get(&self.get_typetoken_of_identifier(identifier))
    }
}

impl Visitor for TypeAnnotater<'_> {
    fn on_items_enter(&mut self, items: &[Item]) {
        // Add all functions and external functions as globally accessible variables.

        let mut global_scope = HashMap::new();

        for item in items {
            match &item.kind {
                ItemKind::Function { name, .. } | ItemKind::ExternalFunction { name, .. } => {
                    global_scope.insert(
                        name.sym,
                        self.ast_types
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

    fn on_function_enter(&mut self, function: &Item) {
        let ItemKind::Function { params, .. } = &function.kind else {
            unreachable!()
        };

        let param_scope = params
            .iter()
            .map(|p| (p.name.sym, *self.ast_types.get(&p.ty.id).expect("")))
            .collect();

        self.scopes.push_scope(param_scope);
    }

    fn on_function_leave(&mut self, _: &Item) {
        self.scopes.pop();
    }

    fn on_block_enter(&mut self, _: &Block) {
        self.scopes.push_empty_scope();
    }

    fn on_block_leave(&mut self, _: &Block) {
        self.scopes.pop();
    }

    fn on_statement_enter(&mut self, statement: &Statement) {
        use StatementKind::*;

        match statement.kind {
            Let {
                name: Name { sym, .. },
                ty: crate::ast::Type { id, .. },
                ..
            } => {
                self.scopes.insert(sym, *self.ast_types.get(&id).unwrap());
            }
            _ => {}
        }
    }

    fn on_expression_leave(&mut self, expression: &Expression) {
        use BuiltinOpKind::*;
        use ExpressionKind::*;

        let typetoken = match &expression.kind {
            IntLiteral(_) => self.typetokens.add(Type::IntConstant),
            BoolLiteral(_) => self.typetokens.add(Type::Bool),
            Identifier(sym) => self.scopes.get(sym).copied().expect(""),
            CompoundIdentifier(idents) => {
                let Some(t) = self.scopes.get(&idents[0]) else {
                    panic!();
                };

                let mut tok = *t;
                for sym in idents[1..].iter() {
                    let Type::Layout(layout) = self.typetokens.get(&tok) else {
                        panic!()
                    };
                    let Some(field) = layout.fields.iter().find(|field| field.name == *sym) else {
                        todo!("referenced field not found in layout");
                    };
                    tok = field.ty;
                }

                tok
            }
            Cast { ty, e } => *self.ast_types.get(&ty.id).expect(""),
            BuiltinOp { op, args } => match op {
                Not | Or | And | Equals | NotEquals | LessThan | GreaterThan | LessEquals
                | GreaterEquals => self.typetokens.add(Type::Bool),
                AddressOf => {
                    let arg = &args[0];
                    let ty = self.ast_types.get(&arg.id).expect("");
                    self.typetokens.add(Type::Pointer(*ty))
                }
                At => {
                    let pointee = &args[0];

                    let ty = self
                        .typetokens
                        .get(self.ast_types.get(&pointee.id).expect(""));

                    match ty {
                        Type::Pointer(t) | Type::Array(_, t) => *t,
                        _ => self.typetokens.add(Type::Invalid),
                    }
                }
                Xor | BitwiseAnd | BitwiseOr | Add | Sub | Mul | Div | Remainder => args
                    .iter()
                    .map(|arg| self.ast_types.get(&arg.id).expect(""))
                    .find(|arg_type| matches!(self.typetokens.get(arg_type), Type::Int { .. }))
                    .copied()
                    .unwrap_or(self.typetokens.add(Type::IntConstant)),
            },
            Call { name, args } => {
                //self.get_typetoken_of_identifier(&name.sym)
                let function_type = self.get_type_of_identifier(&name.sym);
                let Type::Function { return_type, .. } = function_type else {
                    panic!()
                };

                *return_type
            }
        };

        self.ast_types.insert(expression.id, typetoken);
    }
}

pub fn type_annotate(
    items: &[Item],
    typetokens: &mut TypeInterner,
    ast_types: &mut AstMap<TypeToken>,
    typetable: &mut HashMap<Symbol, TypeToken>,
) -> Result<(), Vec<Error>> {
    let mut annotater = TypeAnnotater::new(typetokens, ast_types, typetable);

    annotater.visit_items(items);

    if !annotater.errors.is_empty() {
        return Err(annotater.errors);
    }

    Ok(())
}
