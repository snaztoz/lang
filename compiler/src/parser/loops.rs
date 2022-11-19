use crate::{
    ast::AstNode,
    error::Error,
    parser::{expression::ExpressionParser, Parser},
    token::{Token, TokenKind},
    Result,
};
use std::iter::Peekable;

pub struct LoopsParser<'a, I>
where
    I: Iterator<Item = Token>,
{
    tokens: &'a mut Peekable<I>,
}

impl<'a, I> LoopsParser<'a, I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(tokens: &'a mut Peekable<I>) -> Self {
        Self { tokens }
    }

    pub fn parse_while(&mut self) -> Result<AstNode> {
        assert!(self.tokens.next().unwrap().kind == TokenKind::While);
        let condition =
            ExpressionParser::new(self.tokens, vec![TokenKind::LBrace], true).parse()?;
        let block = Parser::new(self.tokens, Some(TokenKind::RBrace)).parse()?;
        Ok(AstNode::While {
            condition: condition.boxed(),
            block,
        })
    }

    pub fn parse_break(&mut self) -> Result<AstNode> {
        assert!(self.tokens.next().unwrap().kind == TokenKind::Break);
        let next = self.tokens.next().ok_or(Error::UnexpectedEOF)?;
        if next.kind != TokenKind::Semicolon {
            return Err(Error::UnexpectedToken(next));
        }
        Ok(AstNode::Break)
    }

    pub fn parse_continue(&mut self) -> Result<AstNode> {
        assert!(self.tokens.next().unwrap().kind == TokenKind::Continue);
        let next = self.tokens.next().ok_or(Error::UnexpectedEOF)?;
        if next.kind != TokenKind::Semicolon {
            return Err(Error::UnexpectedToken(next));
        }
        Ok(AstNode::Continue)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::Ast, lexer};
    use indoc::indoc;

    #[test]
    fn test_successful_while_statement() {
        let input = indoc! {"
            while foo {
                if bar {
                    break;
                } else if baz {
                    continue;
                }
            }
        "};
        let tokens = lexer::lex(input).into_iter();
        let ast = LoopsParser::new(&mut tokens.peekable()).parse_while();
        assert!(ast.is_ok());
        assert_eq!(
            ast.unwrap(),
            AstNode::While {
                condition: AstNode::Factor(Token {
                    kind: TokenKind::Ident,
                    span: 6..9,
                    value: "foo".to_string(),
                })
                .boxed(),
                block: Ast {
                    statements: vec![
                        AstNode::If {
                            condition: AstNode::Factor(Token {
                                kind: TokenKind::Ident,
                                span: 19..22,
                                value: "bar".to_string(),
                            })
                            .boxed(),
                            block: Ast {
                                statements: vec![AstNode::Break,],
                            }
                        },
                        AstNode::ElseIf {
                            condition: AstNode::Factor(Token {
                                kind: TokenKind::Ident,
                                span: 54..57,
                                value: "baz".to_string(),
                            })
                            .boxed(),
                            block: Ast {
                                statements: vec![AstNode::Continue,],
                            }
                        },
                    ],
                }
            }
        )
    }
}
