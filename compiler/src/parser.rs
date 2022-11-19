use self::expression::ExpressionParser;
use crate::{
    ast::{Ast, AstNode, PackageNameTokens},
    error::Error,
    token::{Token, TokenKind},
    Result,
};
use std::iter::Peekable;

mod expression;

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
            TokenKind::Import => self.parse_import(),
            _ => self.parse_expression(),
        }
    }

    // Example:
    //      package std.io;
    //      package ext;
    //
    fn parse_package(&mut self) -> Result<()> {
        self.tokens.next();
        let package = self.parse_package_name()?;
        self.ast.statements.push(AstNode::Package(package));
        Ok(())
    }

    // Example:
    //      import std.collection.Map;
    //      import std.log.Logger;
    //
    fn parse_import(&mut self) -> Result<()> {
        self.tokens.next();
        let package = self.parse_package_name()?;
        self.ast.statements.push(AstNode::Import(package));
        Ok(())
    }

    // Expressions are parsed based on operator precedences.
    //
    // Example:
    //  x != 5 && y << 2 + 1 == x * 5 + 10
    //
    fn parse_expression(&mut self) -> Result<()> {
        let mut _expr_parser =
            ExpressionParser::new(&mut self.tokens, vec![TokenKind::Semicolon], true);
        _expr_parser.parse().unwrap();
        Ok(())
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
            let ast = Parser::new(tokens).parse_program().unwrap();
            assert_eq!(ast.statements.len(), 1);
            assert_eq!(
                ast.statements[0],
                AstNode::Package(vec![
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

        #[test]
        fn test_import_parsing() {
            let tokens = lexer::lex("import std.io;").into_iter();
            let ast = Parser::new(tokens).parse_program().unwrap();
            assert_eq!(ast.statements.len(), 1);
            assert_eq!(
                ast.statements[0],
                AstNode::Import(vec![
                    Token {
                        kind: TokenKind::Ident,
                        span: 7..10,
                        value: String::from("std"),
                    },
                    Token {
                        kind: TokenKind::Ident,
                        span: 11..13,
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

        #[test]
        fn test_import_parsing() {
            let tests = vec![
                ("import ", Error::UnexpectedEOF),
                (
                    "import std,io;",
                    Error::UnexpectedToken(Token {
                        kind: TokenKind::Comma,
                        span: 10..11,
                        value: String::from(","),
                    }),
                ),
                ("import std.io", Error::UnexpectedEOF),
            ];
            for (test, err) in tests {
                let tokens = lexer::lex(test).into_iter();
                let res = Parser::new(tokens).parse_program();
                assert_eq!(res, Err(err));
            }
        }
    }
}
