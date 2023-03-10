#[derive(Clone, Debug)]
pub enum Type {
    Unit,
    NonUnit(NonUnitType),
}

#[derive(Clone, Debug)]
pub enum NonUnitType {
    Bool,
    Int {
        is_signed: bool,
        width: u64,
    },
    Function {
        params: Vec<NonUnitType>,
        return_type: Box<NonUnitType>,
    },
}
