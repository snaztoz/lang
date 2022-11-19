use self::{condition::ConditionParser, expression::ExpressionParser, package::PackageParser};
use crate::{
    ast::Ast,
    error::Error,
    token::{Token, TokenKind},
    Result,
};
use std::iter::Peekable;

mod condition;
mod expression;
mod package;

pub fn parse(tokens: Vec<Token>) -> Ast {
    let mut tokens = tokens.into_iter().peekable();
    Parser::new(&mut tokens, None).parse().unwrap()
}

struct Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    tokens: &'a mut Peekable<I>,
    ast: Ast,
    delimiter: Option<TokenKind>,
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    fn new(tokens: &'a mut Peekable<I>, delimiter: Option<TokenKind>) -> Self {
        Self {
            tokens,
            ast: Ast::default(),
            delimiter,
        }
    }

    fn parse(mut self) -> Result<Ast> {
        while self.tokens.peek().map(|t| &t.kind) != self.delimiter.as_ref() {
            self.parse_statement()?;
        }
        self.tokens.next();
        Ok(self.ast)
    }

    fn parse_statement(&mut self) -> Result<()> {
        let statement = match self.tokens.peek().unwrap().kind {
            TokenKind::Package => PackageParser::new(self.tokens).parse_package()?,
            TokenKind::Import => PackageParser::new(self.tokens).parse_import()?,
            TokenKind::If => ConditionParser::new(self.tokens).parse_if()?,
            TokenKind::Else => {
                self.tokens.next();
                let next = self.tokens.next().ok_or(Error::UnexpectedEOF)?;
                match next.kind {
                    TokenKind::If => ConditionParser::new(self.tokens).parse_else_if()?,
                    TokenKind::LBrace => ConditionParser::new(self.tokens).parse_else()?,
                    _ => return Err(Error::UnexpectedToken(next)),
                }
            }
            _ => ExpressionParser::new(self.tokens, vec![TokenKind::Semicolon], true).parse()?,
        };
        self.ast.statements.push(statement);
        Ok(())
    }
}
