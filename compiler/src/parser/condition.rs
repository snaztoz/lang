use super::Parser;
use crate::{
    ast::AstNode,
    parser::expression::ExpressionParser,
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::Ast, lexer};
    use indoc::indoc;

    #[test]
    fn test_successful_if_statement() {
        let input = indoc! {"
            if foo > 5 && foo != 10 {
                if bar {
                    io.print(5);
                }
            }
        "};
        let tokens = lexer::lex(input).into_iter();
        let ast = ConditionParser::new(&mut tokens.peekable()).parse_if();
        assert!(ast.is_ok());
        assert_eq!(
            ast.unwrap(),
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
            }
        );
    }
}
