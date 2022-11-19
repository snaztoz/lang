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
    Parser::new(tokens, None).parse().unwrap()
}

struct Parser<I>
where
    I: Iterator<Item = Token>,
{
    tokens: Peekable<I>,
    ast: Ast,
    delimiter: Option<TokenKind>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    fn new(tokens: I, delimiter: Option<TokenKind>) -> Self {
        Self {
            tokens: tokens.peekable(),
            ast: Ast::default(),
            delimiter,
        }
    }

    fn parse(mut self) -> Result<Ast> {
        while self.tokens.peek().map(|t| &t.kind) != self.delimiter.as_ref() {
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
