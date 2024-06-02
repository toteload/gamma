use crate::ast::NodeId;
use crate::source_location::SourceSpan;
use crate::string_interner::{StringInterner, Symbol};
use crate::types::*;
use std::collections::HashMap;

#[derive(Clone, Copy, Debug)]
pub enum ErrorInfo {
    Text(&'static str),
    Identifier(Symbol),
    Type(TypeToken),
    SourceText(SourceSpan),
    AstNode(NodeId),
}

#[derive(Debug, Copy, Clone)]
pub enum ErrorSource {
    Span(SourceSpan),
    AstNode(NodeId),
    Unspecified,
}

#[derive(Clone, Debug)]
pub struct Error {
    pub source: ErrorSource,
    pub info: Vec<ErrorInfo>,
}

impl Error {
    pub fn print(
        &self,
        source: &str,
        spans: &HashMap<NodeId, SourceSpan>,
        symbols: &StringInterner,
        type_tokens: &TypeInterner,
    ) {
        print!("[ERROR] ");

        match self.source {
            ErrorSource::AstNode(id) => {
                let SourceSpan { start, end } = spans.get(&id).unwrap();
                print!("{}:{} | ", start.line, start.col);
            }
            ErrorSource::Span(SourceSpan { start, end }) => {
                print!("{}:{} | ", start.line, start.col)
            }
            ErrorSource::Unspecified => (),
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
                    print!("{}", ty.to_string(type_tokens, symbols));
                }
                SourceText(span) => {
                    //print!(
                    //    "at {}:{} - {}:{}",
                    //    span.start.line, span.start.col, span.end.line, span.end.col
                    //);
                    // TODO(david) This approach is very naive and could be better.
                    let line = source.lines().nth(span.start.line as usize - 1).unwrap();
                    let text = &line[span.start.col as usize - 1..span.end.col as usize - 1];
                    print!("\"{text}\"");
                }
                AstNode(id) => {
                    let span = spans.get(id).unwrap();
                    let line = source.lines().nth(span.start.line as usize - 1).unwrap();
                    let text = &line[span.start.col as usize - 1..span.end.col as usize];
                    print!("\"{text}\"");
                }
            }
        }

        println!();
    }
}
