use crate::{
    ast::{AstNode, PackageNameTokens},
    error::Error,
    token::{Token, TokenKind},
    Result,
};
use std::iter::Peekable;

pub struct PackageParser<'a, I>
where
    I: Iterator<Item = Token>,
{
    tokens: &'a mut Peekable<I>,
}

impl<'a, I> PackageParser<'a, I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(tokens: &'a mut Peekable<I>) -> Self {
        Self { tokens }
    }

    pub fn parse_package(&mut self) -> Result<AstNode> {
        assert!(self.tokens.next().unwrap().kind == TokenKind::Package);
        let package = self.parse_package_name()?;
        Ok(AstNode::Package(package))
    }

    pub fn parse_import(&mut self) -> Result<AstNode> {
        assert!(self.tokens.next().unwrap().kind == TokenKind::Import);
        let package = self.parse_package_name()?;
        Ok(AstNode::Import(package))
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
    use crate::{ast::AstNode, error::Error, lexer};

    #[test]
    fn test_successful_package_parsing() {
        let input = "package std.io;";
        let tokens = lexer::lex(input).into_iter();
        let ast = PackageParser::new(&mut tokens.peekable()).parse_package();
        assert!(ast.is_ok());
        assert_eq!(
            ast.unwrap(),
            AstNode::Package(vec![
                Token {
                    kind: TokenKind::Ident,
                    span: 8..11,
                    value: "std".to_string(),
                },
                Token {
                    kind: TokenKind::Ident,
                    span: 12..14,
                    value: "io".to_string(),
                }
            ]),
        )
    }

    #[test]
    fn test_successful_import_parsing() {
        let input = "import std.io;";
        let tokens = lexer::lex(input).into_iter();
        let ast = PackageParser::new(&mut tokens.peekable()).parse_import();
        assert!(ast.is_ok());
        assert_eq!(
            ast.unwrap(),
            AstNode::Import(vec![
                Token {
                    kind: TokenKind::Ident,
                    span: 7..10,
                    value: "std".to_string(),
                },
                Token {
                    kind: TokenKind::Ident,
                    span: 11..13,
                    value: "io".to_string(),
                }
            ]),
        )
    }

    #[test]
    fn test_failed_package_parsing() {
        let tests = vec![
            ("package ", Error::UnexpectedEOF),
            (
                "package std,io;",
                Error::UnexpectedToken(Token {
                    kind: TokenKind::Comma,
                    span: 11..12,
                    value: ",".to_string(),
                }),
            ),
            ("package std.io", Error::UnexpectedEOF),
        ];
        for (test, err) in tests {
            let tokens = lexer::lex(test).into_iter();
            let ast = PackageParser::new(&mut tokens.peekable()).parse_package();
            assert_eq!(ast, Err(err));
        }
    }

    #[test]
    fn test_failed_import_parsing() {
        let tests = vec![
            ("import ", Error::UnexpectedEOF),
            (
                "import std,io;",
                Error::UnexpectedToken(Token {
                    kind: TokenKind::Comma,
                    span: 10..11,
                    value: ",".to_string(),
                }),
            ),
            ("import std.io", Error::UnexpectedEOF),
        ];
        for (test, err) in tests {
            let tokens = lexer::lex(test).into_iter();
            let ast = PackageParser::new(&mut tokens.peekable()).parse_import();
            assert_eq!(ast, Err(err));
        }
    }
}
