use crate::source_location::SourceSpan;
use crate::string_interner::Symbol;
use crate::types::TypeToken;

#[derive(Clone, Copy)]
pub enum ErrorKind {
    Parse,
    Type,
    Semantic,
}

#[derive(Clone)]
pub enum ErrorInfo {
    Text(&'static str),
    Identifier(Symbol),
    Type(TypeToken),
    SourceText(SourceSpan),
}

#[derive(Clone)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Option<SourceSpan>,
    pub info: Vec<ErrorInfo>,
}
