use super::{ast::AstNode, error::Error, Result};
use crate::token::{Token, TokenKind};
use std::iter::Peekable;

pub struct ExpressionParser<'a, I>
where
    I: Iterator<Item = Token>,
{
    tokens: &'a mut Peekable<I>,
}

impl<'a, I> ExpressionParser<'a, I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(tokens: &'a mut Peekable<I>) -> Self {
        Self { tokens }
    }

    pub fn parse(&mut self) -> Result<AstNode> {
        self.parse_term(None)
    }

    fn parse_term(&mut self, child: Option<AstNode>) -> Result<AstNode> {
        if child.is_none() {
            let factor = self.parse_factor()?;
            return self.parse_term(Some(factor));
        }
        let sym = self.tokens.peek().ok_or(Error::UnexpectedEOF)?;
        if sym.kind != TokenKind::Mul && sym.kind != TokenKind::Div {
            return Ok(child.unwrap());
        }
        let sym = self.tokens.next().unwrap();
        let factor = self.parse_factor()?;
        let node = match sym.kind {
            TokenKind::Mul => AstNode::Mul(child.unwrap().boxed(), factor.boxed()),
            TokenKind::Div => AstNode::Div(child.unwrap().boxed(), factor.boxed()),
            _ => unreachable!(),
        };
        self.parse_term(Some(node))
    }

    fn parse_factor(&mut self) -> Result<AstNode> {
        let next = self.tokens.next().ok_or(Error::UnexpectedEOF)?;
        match next.kind {
            TokenKind::Integer | TokenKind::Ident => Ok(AstNode::Factor(next)),
            TokenKind::LParen => ExpressionParser::new(self.tokens).parse(),
            _ => Err(Error::UnexpectedToken(next)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer;

    #[test]
    fn test_parse_term() {
        let tests = [
            (
                "foo;",
                AstNode::Factor(Token {
                    kind: TokenKind::Ident,
                    span: 0..3,
                    value: "foo".to_string(),
                }),
            ),
            (
                "foo * 10;",
                AstNode::Mul(
                    AstNode::Factor(Token {
                        kind: TokenKind::Ident,
                        span: 0..3,
                        value: "foo".to_string(),
                    })
                    .boxed(),
                    AstNode::Factor(Token {
                        kind: TokenKind::Integer,
                        span: 6..8,
                        value: "10".to_string(),
                    })
                    .boxed(),
                ),
            ),
            (
                "2 * foo / bla;",
                AstNode::Div(
                    AstNode::Mul(
                        AstNode::Factor(Token {
                            kind: TokenKind::Integer,
                            span: 0..1,
                            value: "2".to_string(),
                        })
                        .boxed(),
                        AstNode::Factor(Token {
                            kind: TokenKind::Ident,
                            span: 4..7,
                            value: "foo".to_string(),
                        })
                        .boxed(),
                    )
                    .boxed(),
                    AstNode::Factor(Token {
                        kind: TokenKind::Ident,
                        span: 10..13,
                        value: "bla".to_string(),
                    })
                    .boxed(),
                ),
            ),
        ];
        for (i, (input, expected)) in tests.iter().enumerate() {
            let tokens = lexer::lex(input).into_iter();
            let ast = ExpressionParser::new(&mut tokens.peekable()).parse();
            assert!(ast.is_ok());
            assert_eq!(ast.unwrap(), *expected, "Fail at case #{}", i + 1);
        }
    }
}
