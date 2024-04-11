use crate::string_interner::{StringInterner, Symbol};
use serde::Serialize;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum Signedness {
    Signed,
    Unsigned,
}

#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq, Serialize)]
pub struct LayoutField {
    pub name: Symbol,
    pub offset: u32,
    pub ty: TypeToken,
}

#[derive(Clone, Debug, Eq, Serialize)]
pub struct Layout {
    pub fields: Vec<LayoutField>,
}

#[derive(Clone, Debug, Eq, Serialize)]
pub enum Type {
    Layout(Layout),
    Void,
    Bool,
    Int {
        signedness: Signedness,
        width: u32,
    },
    Function {
        params: Vec<TypeToken>,
        return_type: TypeToken,
    },
    Pointer(TypeToken),
    Array(i64, TypeToken),
}

impl Type {
    pub fn kind_u8(&self) -> u8 {
        use Type::*;

        match self {
            Void => 0,
            Bool => 1,
            Int { .. } => 2,
            Function { .. } => 3,
            Pointer(_) => 4,
            Array(..) => 5,
            Layout(..) => 6,
        }
    }

    pub fn align(&self, type_interner: &TypeInterner) -> u32 {
        use Type::*;

        match self {
            Void | Function { .. } => panic!(),
            Bool => 1,
            Layout(crate::types::Layout { fields }) => fields
                .iter()
                .map(|LayoutField { ty, .. }| type_interner.get(ty).align(type_interner))
                .max()
                .expect("A layout should have fields"),
            Int { width, .. } => match width {
                8 | 16 | 32 | 64 => width / 8,
                _ => panic!(),
            },
            Pointer(_) => 8,
            Array(_, base) => type_interner.get(base).align(type_interner),
        }
    }

    pub fn byte_size(&self, type_interner: &TypeInterner) -> u32 {
        use Type::*;

        match self {
            Void | Function { .. } => panic!(),
            Bool => 1,
            Layout(crate::types::Layout { fields, .. }) => fields
                .iter()
                .map(|LayoutField { offset, ty, .. }| {
                    offset + type_interner.get(ty).byte_size(type_interner)
                })
                .max()
                .expect("A layout should have fields"),
            Int { width, .. } => match width {
                8 | 16 | 32 | 64 => width / 8,
                _ => panic!(),
            },
            Pointer(_) => 8,
            Array(n, base) => {
                let base = type_interner.get(base);
                let base_with_padding_size = base
                    .byte_size(type_interner)
                    .next_multiple_of(base.align(type_interner));
                *n as u32 * base_with_padding_size
            }
        }
    }

    pub fn to_string(
        &self,
        type_interner: &TypeInterner,
        string_interner: &StringInterner,
    ) -> String {
        use Type::*;

        match self {
            Void => "void".to_string(),
            Bool => "bool".to_string(),
            Int { signedness, width } => format!(
                "{}{}",
                if *signedness == Signedness::Signed {
                    "i"
                } else {
                    "u"
                },
                width
            ),
            Pointer(x) => {
                let mut s = "^".to_string();
                s += &type_interner
                    .get(x)
                    .to_string(type_interner, string_interner);
                s
            }
            Array(size, x) => {
                let mut s = format!("[{}]", size);
                s += &type_interner
                    .get(x)
                    .to_string(type_interner, string_interner);
                s
            }
            Function {
                params,
                return_type,
            } => {
                let mut s = "fn(".to_string();

                for param in params {
                    s += &type_interner
                        .get(param)
                        .to_string(type_interner, string_interner);
                    s.push_str(", ");
                }

                s.push_str("): ");
                s += &type_interner
                    .get(return_type)
                    .to_string(type_interner, string_interner);
                s
            }
            Layout(crate::types::Layout { fields }) => {
                let mut s = "layout { ".to_string();
                for LayoutField { name, offset, ty } in fields {
                    s.push_str(&format!(
                        "{}: {offset} {}, ",
                        string_interner.get(name),
                        type_interner
                            .get(ty)
                            .to_string(type_interner, string_interner)
                    ));
                }
                s.push_str("}");

                s
            }
        }
    }
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let x = self.kind_u8();

        state.write_u8(x);

        match self {
            Type::Void | Type::Bool => (),
            Type::Function {
                params,
                return_type,
            } => {
                for param in params {
                    param.hash(state);
                }

                return_type.hash(state);
            }
            Type::Pointer(x) => x.hash(state),
            Type::Array(size, x) => {
                state.write_i64(*size);
                x.hash(state);
            }
            Type::Int { signedness, width } => {
                state.write_u8(*signedness as u8);
                width.hash(state);
            }
            Type::Layout(Layout { fields }) => {
                let mut fields = fields.clone();
                fields.sort_unstable_by_key(|field| field.offset);
                for field in fields {
                    state.write_u32(field.offset);
                    field.ty.hash(state);
                    field.name.hash(state);
                }
            }
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        use Type::*;

        let self_kind = self.kind_u8();
        let other_kind = other.kind_u8();

        if self_kind != other_kind {
            return false;
        }

        match self {
            Bool | Void => true,
            Pointer(a) => {
                let Pointer(b) = other else { unreachable!() };
                a == b
            }
            Array(n, a) => {
                let Array(m, b) = other else { unreachable!() };
                n == m && a == b
            }
            Layout(crate::types::Layout { fields }) => {
                let Layout(crate::types::Layout { fields: fs }) = other else {
                    unreachable!()
                };

                let mut fs = fs.clone();
                fs.sort_unstable_by_key(|field| field.offset);

                let mut fields = fields.clone();
                fields.sort_unstable_by_key(|field| field.offset);

                let same_fields = fields
                    .iter()
                    .zip(&fs)
                    .fold(true, |acc, (x, y)| acc && x == y);

                fs.len() == fields.len() && same_fields
            }
            Function {
                params,
                return_type,
            } => {
                let Function {
                    params: p,
                    return_type: r,
                } = other
                else {
                    unreachable!()
                };
                let same_return_type = return_type == r;

                let same_param_types = params.iter().zip(p).fold(true, |acc, (x, y)| acc && x == y);

                same_return_type && params.len() == p.len() && same_param_types
            }
            Int { signedness, width } => {
                let Int {
                    signedness: s,
                    width: w,
                } = other
                else {
                    unreachable!()
                };
                signedness == s && width == w
            }
        }
    }
}

impl PartialEq for Layout {
    fn eq(&self, other: &Self) -> bool {
        let mut a = self.fields.clone();
        let mut b = other.fields.clone();

        a.sort();
        b.sort();

        a.into_iter().eq(b)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct TypeToken(u32);

#[derive(Serialize)]
pub struct TypeInterner {
    tokens: HashMap<Type, TypeToken>,
    types: Vec<Type>,
}

impl Default for TypeInterner {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeInterner {
    pub fn new() -> TypeInterner {
        TypeInterner {
            tokens: HashMap::new(),
            types: Vec::new(),
        }
    }

    pub fn add(&mut self, ty: Type) -> TypeToken {
        if let Some(tok) = self.tokens.get(&ty) {
            return *tok;
        }

        let tok = TypeToken(self.types.len() as u32);

        self.types.push(ty.clone());

        self.tokens.insert(ty, tok);

        tok
    }

    pub fn find_token(&self, ty: &Type) -> Option<TypeToken> {
        self.tokens.get(ty).copied()
    }

    pub fn find_token_unchecked(&self, ty: &Type) -> TypeToken {
        *self.tokens.get(ty).expect("Type should be interned")
    }

    pub fn get<'a>(&'a self, tok: &TypeToken) -> &'a Type {
        &self.types[tok.0 as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_type_interner_usage() {
        use Type::*;

        let mut interner = TypeInterner::new();

        let a = interner.add(Void);
        let b = interner.add(Bool);
        let c = interner.add(Int {
            signedness: Signedness::Unsigned,
            width: 32,
        });

        assert_eq!(a.0, 0);
        assert_eq!(b.0, 1);
        assert_eq!(c.0, 2);

        assert_ne!(a, b);
        assert_ne!(a, c);
        assert_ne!(b, c);

        assert_eq!(interner.get(&a), &Void);
        assert_eq!(interner.get(&b), &Bool);
        assert_eq!(
            interner.get(&c),
            &Int {
                signedness: Signedness::Unsigned,
                width: 32
            }
        );

        let d = interner.add(Function {
            params: vec![c, c, c],
            return_type: b,
        });

        assert_ne!(a, d);
        assert_ne!(b, d);
        assert_ne!(c, d);

        assert_eq!(d.0, 3);

        let e = interner.add(Bool);

        assert_eq!(b, e);

        let f = interner.add(Function {
            params: vec![c, c, c, c],
            return_type: b,
        });

        assert_ne!(d, f);

        assert_eq!(f.0, 4);
    }
}
