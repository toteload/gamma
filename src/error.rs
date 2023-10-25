use crate::source_location::SourceSpan;
use crate::string_interner::{StringInterner, Symbol};
use crate::types::*;

#[derive(Clone, Copy, Debug)]
pub enum ErrorKind {
    Parse,
    Type,
    Semantic,
}

#[derive(Clone, Copy, Debug)]
pub enum ErrorInfo {
    Text(&'static str),
    Identifier(Symbol),
    Type(TypeToken),
    SourceText(SourceSpan),
}

#[derive(Clone, Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Option<SourceSpan>,
    pub info: Vec<ErrorInfo>,
}

impl Error {
    pub fn print(&self, source: &str, symbols: &StringInterner, type_tokens: &TypeInterner) {
        print!("[ERROR] ");

        match self.kind {
            ErrorKind::Parse => print!("Parsing error: "),
            ErrorKind::Type => print!("Type error: "),
            ErrorKind::Semantic => print!("Semantic error: "),
        }

        for info in self.info.iter() {
            use ErrorInfo::*;

            match info {
                Text(s) => print!("{}", s),
                Identifier(sym) => {
                    let s = symbols.get(sym);
                    print!("\"{}\"", s);
                }
                Type(tok) => {
                    let ty = type_tokens.get(tok);
                    print!("{}", ty.to_string(type_tokens));
                }
                SourceText(span) => {
                    print!(
                        "at {}:{} - {}:{}",
                        span.start.line, span.start.col, span.end.line, span.end.col
                    );
                    // TODO(david) This approach is very naive and could be better.
                    //let line = source.lines().skip(span.start.line - 1);
                }
            }
        }

        println!("");
    }
}
