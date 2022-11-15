use super::{ast::AstNode, error::Error, Result};
use crate::token::{Token, TokenKind};
use std::iter::Peekable;

pub struct ExpressionParser<'a, I>
where
    I: Iterator<Item = Token>,
{
    tokens: &'a mut Peekable<I>,
    delimiter: TokenKind,
}

impl<'a, I> ExpressionParser<'a, I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(tokens: &'a mut Peekable<I>, delimiter: TokenKind) -> Self {
        Self { tokens, delimiter }
    }

    pub fn parse(&mut self) -> Result<AstNode> {
        let result = self.parse_mul_div(None)?;
        let d = self.tokens.next().unwrap();
        if d.kind != self.delimiter {
            return Err(Error::UnexpectedToken(d));
        }
        Ok(result)
    }

    fn parse_mul_div(&mut self, child: Option<AstNode>) -> Result<AstNode> {
        if child.is_none() {
            let factor = self.parse_factor()?;
            return self.parse_mul_div(Some(factor));
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
        self.parse_mul_div(Some(node))
    }

    fn parse_factor(&mut self) -> Result<AstNode> {
        let next = self.tokens.next().ok_or(Error::UnexpectedEOF)?;
        match next.kind {
            TokenKind::Integer | TokenKind::Ident => Ok(AstNode::Factor(next)),
            TokenKind::LParen => ExpressionParser::new(self.tokens, TokenKind::RParen).parse(),
            _ => Err(Error::UnexpectedToken(next)),
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
        fn test_parse_mul_div() {
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
                (
                    "(foo);",
                    AstNode::Factor(Token {
                        kind: TokenKind::Ident,
                        span: 1..4,
                        value: "foo".to_string(),
                    }),
                ),
                (
                    "(foo) * 12 / (6 / bar);",
                    AstNode::Div(
                        AstNode::Mul(
                            AstNode::Factor(Token {
                                kind: TokenKind::Ident,
                                span: 1..4,
                                value: "foo".to_string(),
                            })
                            .boxed(),
                            AstNode::Factor(Token {
                                kind: TokenKind::Integer,
                                span: 8..10,
                                value: "12".to_string(),
                            })
                            .boxed(),
                        )
                        .boxed(),
                        AstNode::Div(
                            AstNode::Factor(Token {
                                kind: TokenKind::Integer,
                                span: 14..15,
                                value: "6".to_string(),
                            })
                            .boxed(),
                            AstNode::Factor(Token {
                                kind: TokenKind::Ident,
                                span: 18..21,
                                value: "bar".to_string(),
                            })
                            .boxed(),
                        )
                        .boxed(),
                    ),
                ),
            ];
            for (i, (input, expected)) in tests.iter().enumerate() {
                let tokens = lexer::lex(input).into_iter();
                let ast =
                    ExpressionParser::new(&mut tokens.peekable(), TokenKind::Semicolon).parse();
                assert!(ast.is_ok());
                assert_eq!(ast.unwrap(), *expected, "Failed at case #{}", i + 1);
            }
        }
    }

    mod failed {
        use super::*;

        #[test]
        fn test_parse_mul_div() {
            let tests = [
                (
                    "(123;",
                    Error::UnexpectedToken(Token {
                        kind: TokenKind::Semicolon,
                        span: 4..5,
                        value: ";".to_string(),
                    }),
                ),
                (
                    "1 * 5 / ((123 * 7);",
                    Error::UnexpectedToken(Token {
                        kind: TokenKind::Semicolon,
                        span: 18..19,
                        value: ";".to_string(),
                    }),
                ),
            ];
            for (i, (input, err)) in tests.iter().enumerate() {
                let tokens = lexer::lex(input).into_iter();
                let ast =
                    ExpressionParser::new(&mut tokens.peekable(), TokenKind::Semicolon).parse();
                assert_eq!(&ast.unwrap_err(), err, "Failed at case #{}", i + 1);
            }
        }
    }
}
