use super::expression::ExpressionParser;
use crate::{
    ast::{AstNode, VariableDeclarationKind},
    error::Error,
    token::{Token, TokenKind},
    Result,
};
use std::iter::Peekable;

pub struct DeclarationParser<'a, I>
where
    I: Iterator<Item = Token>,
{
    tokens: &'a mut Peekable<I>,
}

impl<'a, I> DeclarationParser<'a, I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(tokens: &'a mut Peekable<I>) -> Self {
        Self { tokens }
    }

    pub fn parse_variable(&mut self) -> Result<AstNode> {
        let kind = match self.tokens.next().unwrap().kind {
            TokenKind::Const => VariableDeclarationKind::Const,
            TokenKind::Var => VariableDeclarationKind::Var,
            _ => unreachable!(),
        };
        let next = self.tokens.next().ok_or(Error::UnexpectedEOF)?;
        let ident = match next.kind {
            TokenKind::Ident => next,
            _ => return Err(Error::UnexpectedToken(next)),
        };
        let next = self.tokens.next().ok_or(Error::UnexpectedEOF)?;
        if next.kind == TokenKind::Semicolon {
            return Ok(AstNode::VariableDeclaration {
                kind,
                ident,
                value: None,
            });
        }
        if next.kind != TokenKind::Assign {
            return Err(Error::UnexpectedToken(next));
        }
        let value = ExpressionParser::new(self.tokens, vec![TokenKind::Semicolon], true)
            .parse()?
            .boxed();
        Ok(AstNode::VariableDeclaration {
            kind,
            ident,
            value: Some(value),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer;

    #[test]
    fn test_successful_variable_declaration_parsing() {
        let tests = vec![
            (
                "const foo = 5 + 10;",
                AstNode::VariableDeclaration {
                    kind: VariableDeclarationKind::Const,
                    ident: Token {
                        kind: TokenKind::Ident,
                        span: 6..9,
                        value: "foo".to_string(),
                    },
                    value: Some(
                        AstNode::Add(
                            AstNode::Factor(Token {
                                kind: TokenKind::Integer,
                                span: 12..13,
                                value: "5".to_string(),
                            })
                            .boxed(),
                            AstNode::Factor(Token {
                                kind: TokenKind::Integer,
                                span: 16..18,
                                value: "10".to_string(),
                            })
                            .boxed(),
                        )
                        .boxed(),
                    ),
                },
            ),
            (
                "var bar;",
                AstNode::VariableDeclaration {
                    kind: VariableDeclarationKind::Var,
                    ident: Token {
                        kind: TokenKind::Ident,
                        span: 4..7,
                        value: "bar".to_string(),
                    },
                    value: None,
                },
            ),
        ];
        for (i, (input, expected)) in tests.iter().enumerate() {
            let tokens = lexer::lex(input).into_iter();
            let ast = DeclarationParser::new(&mut tokens.peekable()).parse_variable();
            assert_eq!(ast.unwrap(), *expected, "Failed at case #{}", i + 1,)
        }
    }
}
