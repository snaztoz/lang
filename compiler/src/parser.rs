use self::error::Error;
use crate::token::{Token, TokenKind};
use ast::{Ast, PackageNameTokens};
use std::iter::Peekable;

pub mod ast;
pub mod error;

type Result<T> = std::result::Result<T, Error>;

pub fn parse(tokens: Vec<Token>) -> Ast {
    let tokens = tokens.into_iter();
    Parser::new(tokens).parse_program().unwrap()
}

struct Parser<I>
where
    I: Iterator<Item = Token>,
{
    tokens: Peekable<I>,
    ast: Ast,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
            ast: Ast::default(),
        }
    }

    fn parse_program(mut self) -> Result<Ast> {
        while self.tokens.peek().is_some() {
            self.parse_statement()?;
        }
        Ok(self.ast)
    }

    fn parse_statement(&mut self) -> Result<()> {
        match self.tokens.peek().unwrap().kind {
            TokenKind::Package => self.parse_package(),
            _ => todo!(),
        }
    }

    // Example:
    //      package std.io;
    //      package ext;
    //
    fn parse_package(&mut self) -> Result<()> {
        let kind = self.tokens.next().map(|t| t.kind);
        if matches!(kind, Some(k) if k == TokenKind::Package) {
            let package = self.parse_package_name()?;
            self.ast.package = Some(package);
        }
        Ok(())
    }

    // Example:
    //      import std.collection.Map;
    //      import std.log.Logger;
    //
    fn parse_imports(&mut self) -> Result<()> {
        todo!();
    }

    fn parse_package_name(&mut self) -> Result<PackageNameTokens> {
        let mut buffer = vec![];
        loop {
            // expecting identifier
            match self.tokens.next() {
                Some(t) if t.kind == TokenKind::Ident => {
                    buffer.push(t);
                }
                Some(t) => return Err(Error::UnexpectedToken(t)),
                None => return Err(Error::UnexpectedEOF),
            }

            // expecting either period (.) or semicolon (;)
            match self.tokens.next() {
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer;

    mod success {
        use super::*;

        #[test]
        fn test_package_parsing() {
            let tokens = lexer::lex("package std.io;").into_iter();
            let res = Parser::new(tokens).parse_program();
            assert_eq!(
                res.unwrap().package,
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
                let tokens = lexer::lex(test).into_iter();
                let res = Parser::new(tokens).parse_program();
                assert_eq!(res, Err(err));
            }
        }
    }
}
