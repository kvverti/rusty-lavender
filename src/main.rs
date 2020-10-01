#![allow(dead_code)]

use std::env;
use std::iter::FromIterator;

use nom::error::VerboseErrorKind;

use crate::parser::item::Definition;
use crate::parser::token::{Token, TokenStream, TokenValue};
use nom::multi::many1;

mod ast;
mod code;
mod parser;
mod runtime;
mod value;

/// Creates a vector of (begin source index, length) for lines,
/// not including the line terminator.
fn create_line_mapping(source: &str) -> Vec<(usize, usize)> {
    let lines = source.split("\n")
        .collect::<Vec<_>>();
    let mut res = Vec::new();
    let mut col = 0usize;
    for line in lines {
        res.push((col, line.len()));
        col += 1 + line.len();
    }
    res
}

fn main() {
    let source = env::args().into_iter().nth(1);
    if let Some(source) = source {
        let source_lines = create_line_mapping(&source);
        let tokens = Token::parse_sequence(&source);
        let errs = tokens.iter()
            .filter_map(|Token { value, col, len }| {
                if let TokenValue::Unrecognized(chr) = value {
                    Some((chr, col, len))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        if !errs.is_empty() {
            for (chr, col, len) in errs {
                let line = match source_lines.binary_search_by_key(col, |t| t.0) {
                    Ok(idx) => idx,
                    Err(idx) => idx - 1,
                };
                let (line_idx, line_len) = source_lines[line];
                let col = col - line_idx;
                let mut highlight = vec![' '; line_len];
                highlight[col..col + len].iter_mut().for_each(|c| *c = '^');
                eprintln!("[{}:{}] Error: Unrecognized token '{}'", line + 1, col + 1, chr);
                eprintln!("\t{}", &source[line_idx..line_idx + line_len]);
                eprintln!("\t{}", String::from_iter(highlight));
            }
            return;
        }
        let parser = many1(Definition::regular);
        match parser(TokenStream(&tokens)) {
            Ok((_, items)) => {
                println!("{:?}", items);
            }
            Err(err) => match err {
                nom::Err::Error(err) => {
                    for (TokenStream(tokens), kind) in err.errors {
                        let msg = match kind {
                            VerboseErrorKind::Context(ctx) => ctx.to_owned(),
                            VerboseErrorKind::Char(chr) => format!("Expected character: {}", chr),
                            VerboseErrorKind::Nom(kind) => kind.description().to_owned(),
                        };
                        if let Some(&Token { col, len, .. }) = tokens.first() {
                            println!("[{}:{}] Error: {}", "?", col, msg);
                            println!("\t{}", &source[col..col + len]);
                        } else {
                            println!("Error parsing (no message)");
                        }
                    }
                }
                _ => println!("Error parsing (no message)"),
            }
        }
    } else {
        println!("No source");
    }
}
