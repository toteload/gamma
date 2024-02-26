struct LayoutData {
    byte_size: u32,
    align: u32,
}

enum LayoutElement {
    Atom(LayoutData),
    Compound(Vec<LayoutElement>),
}
