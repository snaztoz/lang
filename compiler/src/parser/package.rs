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
