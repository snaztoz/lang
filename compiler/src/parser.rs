use self::error::Error;
use crate::token::{Token, TokenKind};
use ast::{Ast, Context, PackageNameTokens};

pub mod ast;
pub mod error;

type Result<T> = std::result::Result<T, Error>;
type TokenStream = dyn Iterator<Item = Token>;

pub fn parse(tokens: Vec<Token>) -> Ast {
    let Context { ast, .. } = parse_program(tokens).unwrap();
    ast
}

fn parse_program(tokens: Vec<Token>) -> Result<Context> {
    let mut tokens = tokens.into_iter();
    let mut context = Context::default();
    parse_package(&mut context, &mut tokens)?;
    parse_imports(&mut context, &mut tokens)?;
    Ok(context)
}

// Example:
//      package std.io;
//      package ext;
//
fn parse_package(context: &mut Context, tokens: &mut TokenStream) -> Result<()> {
    let kind = tokens.next().map(|t| t.kind);
    if matches!(kind, Some(k) if k == TokenKind::Package) {
        let package = parse_package_name(tokens)?;
        context.ast.package = Some(package);
    }
    Ok(())
}

// Example:
//      import std.collection.Map;
//      import std.log.Logger;
//
fn parse_imports(_: &mut Context, _: &mut TokenStream) -> Result<()> {
    todo!();
}

fn parse_package_name(tokens: &mut TokenStream) -> Result<PackageNameTokens> {
    let mut buffer = vec![];
    loop {
        // expecting identifier
        match tokens.next() {
            Some(t) if t.kind == TokenKind::Ident => {
                buffer.push(t);
            }
            Some(t) => return Err(Error::UnexpectedToken(t)),
            None => return Err(Error::UnexpectedEOF),
        }

        // expecting either period (.) or semicolon (;)
        match tokens.next() {
            Some(t) if t.kind == TokenKind::Period => {
                continue;
            }
            Some(t) if t.kind == TokenKind::Semicolon => {
                return Ok(buffer);
            }
            Some(t) => return Err(Error::UnexpectedToken(t)),
            None => return Err(Error::UnexpectedEOF),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer;

    mod success {
        use super::*;

        #[test]
        fn test_package_parsing() {
            let mut tokens = lexer::lex("package std.io;").into_iter();
            let mut context = Context::default();
            let res = parse_package(&mut context, &mut tokens);
            assert!(res.is_ok());
            assert_eq!(
                context.ast.package,
                Some(vec![
                    Token {
                        kind: TokenKind::Ident,
                        span: 8..11,
                        value: String::from("std"),
                    },
                    Token {
                        kind: TokenKind::Ident,
                        span: 12..14,
                        value: String::from("io"),
                    }
                ]),
            )
        }
    }

    mod failed {
        use super::*;

        #[test]
        fn test_package_parsing() {
            let tests = vec![
                ("package ", Error::UnexpectedEOF),
                (
                    "package std,io;",
                    Error::UnexpectedToken(Token {
                        kind: TokenKind::Comma,
                        span: 11..12,
                        value: String::from(","),
                    }),
                ),
                ("package std.io", Error::UnexpectedEOF),
            ];
            for (test, err) in tests {
                let mut tokens = lexer::lex(test).into_iter();
                let mut context = Context::default();
                let res = parse_package(&mut context, &mut tokens);
                assert_eq!(res, Err(err),)
            }
        }
    }
}
