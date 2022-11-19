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
