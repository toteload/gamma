use crate::types::Type;
use serde::Serialize;
use std::cell::Cell;
use std::collections::HashMap;
use std::ops::Deref;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct TypeToken(u32);

pub struct TypeInterner {
    tokens: Cell<HashMap<&'static Type, TypeToken>>,
    types: Cell<Vec<Box<Type>>>,
}

impl Default for TypeInterner {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeInterner {
    pub fn new() -> TypeInterner {
        TypeInterner {
            tokens: Cell::new(HashMap::new()),
            types: Cell::new(Vec::new()),
        }
    }

    pub fn add(&self, ty: Type) -> TypeToken {
        let tokens = unsafe { &mut (*self.tokens.as_ptr()) };
        let types = unsafe { &mut (*self.types.as_ptr()) };

        if let Some(tok) = tokens.get(&ty) {
            return *tok;
        }

        let tok = TypeToken(types.len() as u32);

        let ty = Box::new(ty);

        let ty_ref: &Type = unsafe { &*(ty.deref() as *const Type) };

        types.push(ty);

        tokens.insert(ty_ref, tok);

        tok
    }

    pub fn find_token(&self, ty: &Type) -> Option<TypeToken> {
        let tokens = unsafe { &(*self.tokens.as_ptr()) };
        tokens.get(ty).copied()
    }

    pub fn find_token_unchecked(&self, ty: &Type) -> TypeToken {
        let tokens = unsafe { &(*self.tokens.as_ptr()) };
        *tokens.get(ty).expect("Type should be interned")
    }

    pub fn get(&self, tok: &TypeToken) -> &Type {
        let types = unsafe { &(*self.types.as_ptr()) };
        &types[tok.0 as usize]
    }
}
