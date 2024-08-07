use crate::ast;
use crate::string_interner::{StringInterner, Symbol};
use crate::type_interner::{TypeInterner, TypeToken};
use serde::Serialize;
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

impl Layout {
    pub fn find_field(&self, field: Symbol) -> Option<LayoutField> {
        self.fields.iter().copied().find(|f| f.name == field)
    }
}

#[derive(Clone, Debug, Eq, Serialize)]
pub enum Type {
    Invalid,
    Layout(Layout),
    Void,
    Bool,
    Int {
        signedness: Signedness,
        width: u32,
    },
    IntConstant,
    Function {
        params: Vec<TypeToken>,
        return_type: TypeToken,
    },
    Pointer(TypeToken),
    Array(i64, TypeToken),
}

pub const U64: Type = Type::Int {
    signedness: Signedness::Unsigned,
    width: 64,
};

impl Type {
    pub fn is_integer(&self) -> bool {
        matches!(self, Type::Int { .. } | Type::IntConstant)
    }

    pub fn is_layout(&self) -> bool {
        matches!(self, Type::Layout { .. })
    }

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
            IntConstant => 7,
            Invalid => 8,
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
            IntConstant | Invalid => panic!(),
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
            IntConstant | Invalid => panic!(),
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
                s.push('}');

                s
            }
            IntConstant => "int-constant".to_string(),
            Invalid => "invalid".to_string(),
        }
    }
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let x = self.kind_u8();

        state.write_u8(x);

        match self {
            Type::Void | Type::Bool | Type::IntConstant | Type::Invalid => (),
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
            Bool | Void | IntConstant | Invalid => true,
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

pub fn is_type_coercible_to(type_tokens: &TypeInterner, from: TypeToken, to: TypeToken) -> bool {
    if from == to {
        return true;
    }

    if matches!(
        (type_tokens.get(&from), type_tokens.get(&to)),
        (Type::IntConstant, Type::Int { .. })
    ) {
        return true;
    }

    false
}

#[rustfmt::skip]
pub fn is_valid_type_cast(from: &Type, to: &Type) -> bool {
    use Type::*;

    match (from, to) {
        (IntConstant , Int { .. }) => true,
        (IntConstant , Pointer(_)) => true,
        (Int { .. }  , Int { .. }) => true,
        (Int { .. }  , Bool      ) => true,
        (Int { .. }  , Pointer(_)) => true,
        (Bool        , Int { .. }) => true,

        (Pointer(ta), Array(_, tb)) if ta == tb => true,

        _ => false,
    }
}

pub fn is_addressable(e: &ast::Expression) -> bool {
    matches!(e.kind, ast::ExpressionKind::Identifier(_))
}
