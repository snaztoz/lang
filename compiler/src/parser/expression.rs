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

    #[test]
    fn test_successful_expression_parsing() {
        let input = "32 / 2 >= foo && foo != -bar || (2 << 8 ^ 1024 + 32);";
        let tokens = lexer::lex(input).into_iter();
        let ast = ExpressionParser::new(&mut tokens.peekable(), TokenKind::Semicolon).parse();
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
            let ast = ExpressionParser::new(&mut tokens.peekable(), TokenKind::Semicolon).parse();
            assert_eq!(&ast.unwrap_err(), err, "Failed at case #{}", i + 1);
        }
    }
}
