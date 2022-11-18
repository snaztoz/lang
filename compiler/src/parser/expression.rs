use super::{ast::AstNode, error::Error, Result};
use crate::token::{Token, TokenKind};
use std::{collections::HashMap, iter::Peekable};

type AcceptedKinds = Vec<TokenKind>;
type Rule = &'static str;
type NextRule = Rule;

pub struct ExpressionParser<'a, I>
where
    I: Iterator<Item = Token>,
{
    tokens: &'a mut Peekable<I>,
    delimiters: Vec<TokenKind>,
    consume_delimiter: bool,
    rules: HashMap<Rule, (AcceptedKinds, NextRule)>,
}

impl<'a, I> ExpressionParser<'a, I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(
        tokens: &'a mut Peekable<I>,
        delimiters: Vec<TokenKind>,
        consume_delimiter: bool,
    ) -> Self {
        let rules = HashMap::from([
            (
                "logical",
                (vec![TokenKind::LogicalAnd, TokenKind::LogicalOr], "bitwise"),
            ),
            (
                "bitwise",
                (
                    vec![TokenKind::And, TokenKind::Or, TokenKind::Xor],
                    "equality",
                ),
            ),
            (
                "equality",
                (vec![TokenKind::Equal, TokenKind::NotEqual], "relational"),
            ),
            (
                "relational",
                (
                    vec![
                        TokenKind::Less,
                        TokenKind::LessEqual,
                        TokenKind::Greater,
                        TokenKind::GreaterEqual,
                    ],
                    "shift",
                ),
            ),
            ("shift", (vec![TokenKind::Shl, TokenKind::Shr], "add_sub")),
            ("add_sub", (vec![TokenKind::Add, TokenKind::Sub], "mul_div")),
            ("mul_div", (vec![TokenKind::Mul, TokenKind::Div], "factor")),
        ]);
        Self {
            tokens,
            delimiters,
            consume_delimiter,
            rules,
        }
    }

    pub fn parse(&mut self) -> Result<AstNode> {
        let result = self.parse_rule("logical", None)?;
        if !self.consume_delimiter {
            // this is for soft-stop handling, encountering
            // one of the specified delimiters but without
            // consuming it
            let d = self.tokens.peek().unwrap();
            if self.delimiters.contains(&d.kind) {
                return Ok(result);
            }
        }
        let d = self.tokens.next().unwrap();
        if !self.delimiters.contains(&d.kind) {
            return Err(Error::UnexpectedToken(d));
        }
        Ok(result)
    }

    fn parse_rule(&mut self, rule: Rule, child: Option<AstNode>) -> Result<AstNode> {
        if rule == "factor" {
            // special rule
            return FactorExpressionParser::new(self.tokens).parse();
        }
        let (accepted_kinds, next_rule) = &self.rules[rule];
        if child.is_none() {
            let child = self.parse_rule(next_rule, None)?;
            return self.parse_rule(rule, Some(child));
        }
        let sym = self.tokens.peek().ok_or(Error::UnexpectedEOF)?;
        if !accepted_kinds.contains(&sym.kind) {
            return Ok(child.unwrap());
        }
        let sym = self.tokens.next().unwrap();
        let right = self.parse_rule(next_rule, None)?;
        let node = sym
            .kind
            .as_ast_parent(HashMap::from([("left", child.unwrap()), ("right", right)]));
        self.parse_rule(rule, Some(node))
    }
}

pub struct FactorExpressionParser<'a, I>
where
    I: Iterator<Item = Token>,
{
    tokens: &'a mut Peekable<I>,
}

impl<'a, I> FactorExpressionParser<'a, I>
where
    I: Iterator<Item = Token>,
{
    fn new(tokens: &'a mut Peekable<I>) -> Self {
        Self { tokens }
    }

    fn parse(&mut self) -> Result<AstNode> {
        let next = self.tokens.next().ok_or(Error::UnexpectedEOF)?;
        if next.kind == TokenKind::Sub {
            return Ok(AstNode::Neg(self.parse()?.boxed()));
        } else if next.kind == TokenKind::Not {
            return Ok(AstNode::Not(self.parse()?.boxed()));
        }
        let base = match next.kind {
            TokenKind::Integer | TokenKind::Ident => Ok(AstNode::Factor(next)),
            TokenKind::LParen => {
                ExpressionParser::new(self.tokens, vec![TokenKind::RParen], true).parse()
            }
            _ => Err(Error::UnexpectedToken(next)),
        };
        self._parse(base?)
    }

    fn _parse(&mut self, child: AstNode) -> Result<AstNode> {
        let next = self.tokens.peek();
        if next.is_none() {
            return Ok(child);
        }
        let node = match next.unwrap().kind {
            TokenKind::Period => self.parse_member_access(child)?,
            TokenKind::LBrack => self.parse_key_access(child)?,
            TokenKind::LParen => self.parse_function_call(child)?,
            _ => return Ok(child),
        };
        self._parse(node)
    }

    fn parse_member_access(&mut self, accessed: AstNode) -> Result<AstNode> {
        // skip period symbol
        assert!(self.tokens.next().unwrap().kind == TokenKind::Period);
        let next = self.tokens.next().ok_or(Error::UnexpectedEOF)?;
        if next.kind != TokenKind::Ident {
            return Err(Error::UnexpectedToken(next));
        }
        Ok(AstNode::MemberAccess {
            accessed: accessed.boxed(),
            member: AstNode::Factor(next).boxed(),
        })
    }

    fn parse_key_access(&mut self, accessed: AstNode) -> Result<AstNode> {
        // skip left bracket symbol
        assert!(self.tokens.next().unwrap().kind == TokenKind::LBrack);
        let node = ExpressionParser::new(self.tokens, vec![TokenKind::RBrack], true).parse()?;
        Ok(AstNode::KeyAccess {
            accessed: accessed.boxed(),
            key: node.boxed(),
        })
    }

    fn parse_function_call(&mut self, func: AstNode) -> Result<AstNode> {
        // skip left parentheses symbol
        assert!(self.tokens.next().unwrap().kind == TokenKind::LParen);
        let mut args = vec![];
        let next = self.tokens.peek().ok_or(Error::UnexpectedEOF)?;
        if next.kind == TokenKind::RParen {
            return Ok(AstNode::FunctionCall {
                func: func.boxed(),
                args,
            });
        }
        loop {
            let arg = ExpressionParser::new(
                self.tokens,
                vec![TokenKind::Comma, TokenKind::RParen],
                false,
            )
            .parse()?;
            args.push(arg);
            let next = self.tokens.next().ok_or(Error::UnexpectedEOF)?;
            match next.kind {
                TokenKind::Comma => {
                    // make it possible that the last argument
                    // can still has a comma following after it
                    let next = self.tokens.peek().ok_or(Error::UnexpectedEOF)?;
                    if next.kind == TokenKind::RParen {
                        self.tokens.next().unwrap();
                        break;
                    }
                }
                TokenKind::RParen => break,
                _ => unreachable!(),
            }
        }
        Ok(AstNode::FunctionCall {
            func: func.boxed(),
            args,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer;

    #[test]
    fn test_successful_expression_parsing() {
        let input = "32 / 2 >= foo && foo != -bar || (2 << 8 ^ 1024 + 32);";
        let tokens = lexer::lex(input).into_iter();
        let ast =
            ExpressionParser::new(&mut tokens.peekable(), vec![TokenKind::Semicolon], true).parse();
        assert!(ast.is_ok());
        assert_eq!(
            ast.unwrap(),
            AstNode::LogicalOr(
                AstNode::LogicalAnd(
                    AstNode::GreaterEqual(
                        AstNode::Div(
                            AstNode::Factor(Token {
                                kind: TokenKind::Integer,
                                span: 0..2,
                                value: "32".to_string(),
                            })
                            .boxed(),
                            AstNode::Factor(Token {
                                kind: TokenKind::Integer,
                                span: 5..6,
                                value: "2".to_string(),
                            })
                            .boxed(),
                        )
                        .boxed(),
                        AstNode::Factor(Token {
                            kind: TokenKind::Ident,
                            span: 10..13,
                            value: "foo".to_string(),
                        })
                        .boxed(),
                    )
                    .boxed(),
                    AstNode::NotEqual(
                        AstNode::Factor(Token {
                            kind: TokenKind::Ident,
                            span: 17..20,
                            value: "foo".to_string(),
                        })
                        .boxed(),
                        AstNode::Neg(
                            AstNode::Factor(Token {
                                kind: TokenKind::Ident,
                                span: 25..28,
                                value: "bar".to_string(),
                            })
                            .boxed()
                        )
                        .boxed()
                    )
                    .boxed(),
                )
                .boxed(),
                AstNode::Xor(
                    AstNode::Shl(
                        AstNode::Factor(Token {
                            kind: TokenKind::Integer,
                            span: 33..34,
                            value: "2".to_string(),
                        })
                        .boxed(),
                        AstNode::Factor(Token {
                            kind: TokenKind::Integer,
                            span: 38..39,
                            value: "8".to_string(),
                        })
                        .boxed(),
                    )
                    .boxed(),
                    AstNode::Add(
                        AstNode::Factor(Token {
                            kind: TokenKind::Integer,
                            span: 42..46,
                            value: "1024".to_string(),
                        })
                        .boxed(),
                        AstNode::Factor(Token {
                            kind: TokenKind::Integer,
                            span: 49..51,
                            value: "32".to_string(),
                        })
                        .boxed(),
                    )
                    .boxed(),
                )
                .boxed(),
            ),
        );
    }

    #[test]
    fn test_failed_expression_parsing() {
        let tests = [
            (
                "(123||5;);",
                Error::UnexpectedToken(Token {
                    kind: TokenKind::Semicolon,
                    span: 7..8,
                    value: ";".to_string(),
                }),
            ),
            (
                "(1 *5 >> >> 7 + 6);",
                Error::UnexpectedToken(Token {
                    kind: TokenKind::Shr,
                    span: 9..11,
                    value: ">>".to_string(),
                }),
            ),
        ];
        for (i, (input, err)) in tests.iter().enumerate() {
            let tokens = lexer::lex(input).into_iter();
            let ast =
                ExpressionParser::new(&mut tokens.peekable(), vec![TokenKind::Semicolon], true)
                    .parse();
            assert_eq!(&ast.unwrap_err(), err, "Failed at case #{}", i + 1);
        }
    }

    #[test]
    fn test_successful_factor_expression_parsing() {
        let input = "-(!foo).bar[baz].bar(2 << 5 == abc, foo,);";
        let tokens = lexer::lex(input).into_iter();
        let ast =
            ExpressionParser::new(&mut tokens.peekable(), vec![TokenKind::Semicolon], true).parse();
        assert!(ast.is_ok());
        assert_eq!(
            ast.unwrap(),
            AstNode::Neg(
                AstNode::FunctionCall {
                    func: AstNode::MemberAccess {
                        accessed: AstNode::KeyAccess {
                            accessed: AstNode::MemberAccess {
                                accessed: AstNode::Not(
                                    AstNode::Factor(Token {
                                        kind: TokenKind::Ident,
                                        span: 3..6,
                                        value: "foo".to_string(),
                                    })
                                    .boxed()
                                )
                                .boxed(),
                                member: AstNode::Factor(Token {
                                    kind: TokenKind::Ident,
                                    span: 8..11,
                                    value: "bar".to_string(),
                                })
                                .boxed(),
                            }
                            .boxed(),
                            key: AstNode::Factor(Token {
                                kind: TokenKind::Ident,
                                span: 12..15,
                                value: "baz".to_string(),
                            })
                            .boxed(),
                        }
                        .boxed(),
                        member: AstNode::Factor(Token {
                            kind: TokenKind::Ident,
                            span: 17..20,
                            value: "bar".to_string(),
                        })
                        .boxed(),
                    }
                    .boxed(),
                    args: vec![
                        AstNode::Equal(
                            AstNode::Shl(
                                AstNode::Factor(Token {
                                    kind: TokenKind::Integer,
                                    span: 21..22,
                                    value: "2".to_string(),
                                })
                                .boxed(),
                                AstNode::Factor(Token {
                                    kind: TokenKind::Integer,
                                    span: 26..27,
                                    value: "5".to_string(),
                                })
                                .boxed(),
                            )
                            .boxed(),
                            AstNode::Factor(Token {
                                kind: TokenKind::Ident,
                                span: 31..34,
                                value: "abc".to_string(),
                            })
                            .boxed(),
                        ),
                        AstNode::Factor(Token {
                            kind: TokenKind::Ident,
                            span: 36..39,
                            value: "foo".to_string(),
                        }),
                    ]
                }
                .boxed()
            )
        )
    }
}
