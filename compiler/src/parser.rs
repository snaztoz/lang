use self::{expression::ExpressionParser, package::PackageParser};
use crate::{
    ast::Ast,
    token::{Token, TokenKind},
    Result,
};
use std::iter::Peekable;

mod expression;
mod package;

pub fn parse(tokens: Vec<Token>) -> Ast {
    let tokens = tokens.into_iter();
    Parser::new(tokens).parse().unwrap()
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

    fn parse(mut self) -> Result<Ast> {
        while self.tokens.peek().is_some() {
            self.parse_statement()?;
        }
        Ok(self.ast)
    }

    fn parse_statement(&mut self) -> Result<()> {
        let statement = match self.tokens.peek().unwrap().kind {
            TokenKind::Package => PackageParser::new(&mut self.tokens).parse_package()?,
            TokenKind::Import => PackageParser::new(&mut self.tokens).parse_import()?,
            _ => {
                ExpressionParser::new(&mut self.tokens, vec![TokenKind::Semicolon], true).parse()?
            }
        };
        self.ast.statements.push(statement);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::AstNode, error::Error, lexer};

    mod success {
        use super::*;

        #[test]
        fn test_package_parsing() {
            let tokens = lexer::lex("package std.io;").into_iter();
            let ast = Parser::new(tokens).parse().unwrap();
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
            let ast = Parser::new(tokens).parse().unwrap();
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
                let res = Parser::new(tokens).parse();
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
                let res = Parser::new(tokens).parse();
                assert_eq!(res, Err(err));
            }
        }
    }
}
