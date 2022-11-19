use crate::{
    ast::AstNode,
    parser::{expression::ExpressionParser, Parser},
    token::{Token, TokenKind},
    Result,
};
use std::iter::Peekable;

pub struct ConditionParser<'a, I>
where
    I: Iterator<Item = Token>,
{
    tokens: &'a mut Peekable<I>,
}

impl<'a, I> ConditionParser<'a, I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(tokens: &'a mut Peekable<I>) -> Self {
        Self { tokens }
    }

    pub fn parse_if(&mut self) -> Result<AstNode> {
        assert!(self.tokens.next().unwrap().kind == TokenKind::If);
        let condition =
            ExpressionParser::new(self.tokens, vec![TokenKind::LBrace], true).parse()?;
        let block = Parser::new(self.tokens, Some(TokenKind::RBrace)).parse()?;
        Ok(AstNode::If {
            condition: condition.boxed(),
            block,
        })
    }

    pub fn parse_else_if(&mut self) -> Result<AstNode> {
        let condition =
            ExpressionParser::new(self.tokens, vec![TokenKind::LBrace], true).parse()?;
        let block = Parser::new(self.tokens, Some(TokenKind::RBrace)).parse()?;
        Ok(AstNode::ElseIf {
            condition: condition.boxed(),
            block,
        })
    }

    pub fn parse_else(&mut self) -> Result<AstNode> {
        let block = Parser::new(self.tokens, Some(TokenKind::RBrace)).parse()?;
        Ok(AstNode::Else { block })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::Ast, lexer, parser::Parser};
    use indoc::indoc;

    #[test]
    fn test_successful_conditional_statement() {
        let input = indoc! {"
            if foo > 5 && foo != 10 {
                if bar {
                    io.print(5);
                }
            } else if bar {
                io.print(10);
            } else {
                io.print(15);
            }
        "};
        let tokens = lexer::lex(input).into_iter();
        // we use Parser directly here because ConditionParser can't
        // parse all if-else blocks at the same time (it must be called
        // manually one-by-one)
        let ast = Parser::new(&mut tokens.peekable(), None).parse();
        assert!(ast.is_ok());
        assert_eq!(
            ast.unwrap().statements,
            vec![
                AstNode::If {
                    condition: AstNode::LogicalAnd(
                        AstNode::Greater(
                            AstNode::Factor(Token {
                                kind: TokenKind::Ident,
                                span: 3..6,
                                value: "foo".to_string(),
                            })
                            .boxed(),
                            AstNode::Factor(Token {
                                kind: TokenKind::Integer,
                                span: 9..10,
                                value: "5".to_string(),
                            })
                            .boxed(),
                        )
                        .boxed(),
                        AstNode::NotEqual(
                            AstNode::Factor(Token {
                                kind: TokenKind::Ident,
                                span: 14..17,
                                value: "foo".to_string(),
                            })
                            .boxed(),
                            AstNode::Factor(Token {
                                kind: TokenKind::Integer,
                                span: 21..23,
                                value: "10".to_string(),
                            })
                            .boxed(),
                        )
                        .boxed(),
                    )
                    .boxed(),
                    block: Ast {
                        statements: vec![AstNode::If {
                            condition: AstNode::Factor(Token {
                                kind: TokenKind::Ident,
                                span: 33..36,
                                value: "bar".to_string(),
                            })
                            .boxed(),
                            block: Ast {
                                statements: vec![AstNode::FunctionCall {
                                    func: AstNode::MemberAccess {
                                        accessed: AstNode::Factor(Token {
                                            kind: TokenKind::Ident,
                                            span: 47..49,
                                            value: "io".to_string(),
                                        })
                                        .boxed(),
                                        member: AstNode::Factor(Token {
                                            kind: TokenKind::Ident,
                                            span: 50..55,
                                            value: "print".to_string(),
                                        })
                                        .boxed(),
                                    }
                                    .boxed(),
                                    args: vec![AstNode::Factor(Token {
                                        kind: TokenKind::Integer,
                                        span: 56..57,
                                        value: "5".to_string(),
                                    })],
                                }]
                            },
                        },],
                    },
                },
                AstNode::ElseIf {
                    condition: AstNode::Factor(Token {
                        kind: TokenKind::Ident,
                        span: 76..79,
                        value: "bar".to_string(),
                    })
                    .boxed(),
                    block: Ast {
                        statements: vec![AstNode::FunctionCall {
                            func: AstNode::MemberAccess {
                                accessed: AstNode::Factor(Token {
                                    kind: TokenKind::Ident,
                                    span: 86..88,
                                    value: "io".to_string(),
                                })
                                .boxed(),
                                member: AstNode::Factor(Token {
                                    kind: TokenKind::Ident,
                                    span: 89..94,
                                    value: "print".to_string(),
                                })
                                .boxed(),
                            }
                            .boxed(),
                            args: vec![AstNode::Factor(Token {
                                kind: TokenKind::Integer,
                                span: 95..97,
                                value: "10".to_string(),
                            })],
                        }]
                    }
                },
                AstNode::Else {
                    block: Ast {
                        statements: vec![AstNode::FunctionCall {
                            func: AstNode::MemberAccess {
                                accessed: AstNode::Factor(Token {
                                    kind: TokenKind::Ident,
                                    span: 113..115,
                                    value: "io".to_string(),
                                })
                                .boxed(),
                                member: AstNode::Factor(Token {
                                    kind: TokenKind::Ident,
                                    span: 116..121,
                                    value: "print".to_string(),
                                })
                                .boxed(),
                            }
                            .boxed(),
                            args: vec![AstNode::Factor(Token {
                                kind: TokenKind::Integer,
                                span: 122..124,
                                value: "15".to_string(),
                            })],
                        }]
                    }
                }
            ]
        );
    }
}
