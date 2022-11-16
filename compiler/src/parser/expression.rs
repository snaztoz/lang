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
    delimiter: TokenKind,
    rules: HashMap<Rule, (AcceptedKinds, NextRule)>,
}

impl<'a, I> ExpressionParser<'a, I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(tokens: &'a mut Peekable<I>, delimiter: TokenKind) -> Self {
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
            delimiter,
            rules,
        }
    }

    pub fn parse(&mut self) -> Result<AstNode> {
        let result = self.parse_rule("logical", None)?;
        let d = self.tokens.next().unwrap();
        if d.kind != self.delimiter {
            return Err(Error::UnexpectedToken(d));
        }
        Ok(result)
    }

    fn parse_rule(&mut self, rule: Rule, child: Option<AstNode>) -> Result<AstNode> {
        if rule == "factor" {
            // special rule
            return self.parse_factor();
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
        fn test_parse_expression() {
            let input = "32 / 2 >= foo && foo != bar || (2 << 8 ^ 1024 + 32);";
            let tokens = lexer::lex(input).into_iter();
            let ast = ExpressionParser::new(&mut tokens.peekable(), TokenKind::Semicolon).parse();
            dbg!(&ast);
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
                            AstNode::Factor(Token {
                                kind: TokenKind::Ident,
                                span: 24..27,
                                value: "bar".to_string(),
                            })
                            .boxed(),
                        )
                        .boxed(),
                    )
                    .boxed(),
                    AstNode::Xor(
                        AstNode::Shl(
                            AstNode::Factor(Token {
                                kind: TokenKind::Integer,
                                span: 32..33,
                                value: "2".to_string(),
                            })
                            .boxed(),
                            AstNode::Factor(Token {
                                kind: TokenKind::Integer,
                                span: 37..38,
                                value: "8".to_string(),
                            })
                            .boxed(),
                        )
                        .boxed(),
                        AstNode::Add(
                            AstNode::Factor(Token {
                                kind: TokenKind::Integer,
                                span: 41..45,
                                value: "1024".to_string(),
                            })
                            .boxed(),
                            AstNode::Factor(Token {
                                kind: TokenKind::Integer,
                                span: 48..50,
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
        fn test_parse_shift() {
            let tests = [
                (
                    "foo << 5;",
                    AstNode::Shl(
                        AstNode::Factor(Token {
                            kind: TokenKind::Ident,
                            span: 0..3,
                            value: "foo".to_string(),
                        })
                        .boxed(),
                        AstNode::Factor(Token {
                            kind: TokenKind::Integer,
                            span: 7..8,
                            value: "5".to_string(),
                        })
                        .boxed(),
                    ),
                ),
                (
                    "foo << 5 >> bar / 99;",
                    AstNode::Shr(
                        AstNode::Shl(
                            AstNode::Factor(Token {
                                kind: TokenKind::Ident,
                                span: 0..3,
                                value: "foo".to_string(),
                            })
                            .boxed(),
                            AstNode::Factor(Token {
                                kind: TokenKind::Integer,
                                span: 7..8,
                                value: "5".to_string(),
                            })
                            .boxed(),
                        )
                        .boxed(),
                        AstNode::Div(
                            AstNode::Factor(Token {
                                kind: TokenKind::Ident,
                                span: 12..15,
                                value: "bar".to_string(),
                            })
                            .boxed(),
                            AstNode::Factor(Token {
                                kind: TokenKind::Integer,
                                span: 18..20,
                                value: "99".to_string(),
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

        #[test]
        fn test_parse_add_sub() {
            let tests = [
                (
                    "foo + 1 * 5;",
                    AstNode::Add(
                        AstNode::Factor(Token {
                            kind: TokenKind::Ident,
                            span: 0..3,
                            value: "foo".to_string(),
                        })
                        .boxed(),
                        AstNode::Mul(
                            AstNode::Factor(Token {
                                kind: TokenKind::Integer,
                                span: 6..7,
                                value: "1".to_string(),
                            })
                            .boxed(),
                            AstNode::Factor(Token {
                                kind: TokenKind::Integer,
                                span: 10..11,
                                value: "5".to_string(),
                            })
                            .boxed(),
                        )
                        .boxed(),
                    ),
                ),
                (
                    "(1 - 5) / (foo + 2 / 5);",
                    AstNode::Div(
                        AstNode::Sub(
                            AstNode::Factor(Token {
                                kind: TokenKind::Integer,
                                span: 1..2,
                                value: "1".to_string(),
                            })
                            .boxed(),
                            AstNode::Factor(Token {
                                kind: TokenKind::Integer,
                                span: 5..6,
                                value: "5".to_string(),
                            })
                            .boxed(),
                        )
                        .boxed(),
                        AstNode::Add(
                            AstNode::Factor(Token {
                                kind: TokenKind::Ident,
                                span: 11..14,
                                value: "foo".to_string(),
                            })
                            .boxed(),
                            AstNode::Div(
                                AstNode::Factor(Token {
                                    kind: TokenKind::Integer,
                                    span: 17..18,
                                    value: "2".to_string(),
                                })
                                .boxed(),
                                AstNode::Factor(Token {
                                    kind: TokenKind::Integer,
                                    span: 21..22,
                                    value: "5".to_string(),
                                })
                                .boxed(),
                            )
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
        fn test_parse_add_sub() {
            let tests = [
                (
                    "(123+;",
                    Error::UnexpectedToken(Token {
                        kind: TokenKind::Semicolon,
                        span: 5..6,
                        value: ";".to_string(),
                    }),
                ),
                (
                    "(1 *5 + + 6);",
                    Error::UnexpectedToken(Token {
                        kind: TokenKind::Add,
                        span: 8..9,
                        value: "+".to_string(),
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
