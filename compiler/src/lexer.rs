use crate::token::{Token, TokenKind};
use logos::Logos;

pub fn lex(input: &str) -> Vec<Token> {
    let mut lexer = TokenKind::lexer(input);
    let mut tokens = vec![];
    while let Some(kind) = lexer.next() {
        assert!(kind != TokenKind::Error);
        tokens.push(Token {
            kind,
            span: lexer.span(),
            value: lexer.slice().to_string(),
        })
    }
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexing() {
        let input = "const numApple = 15;";
        let tokens = lex(input);
        assert_eq!(
            &tokens,
            &[
                Token {
                    kind: TokenKind::Const,
                    span: 0..5,
                    value: "const".to_string(),
                },
                Token {
                    kind: TokenKind::Ident,
                    span: 6..14,
                    value: "numApple".to_string(),
                },
                Token {
                    kind: TokenKind::Assign,
                    span: 15..16,
                    value: "=".to_string(),
                },
                Token {
                    kind: TokenKind::Integer,
                    span: 17..19,
                    value: "15".to_string(),
                },
                Token {
                    kind: TokenKind::Semicolon,
                    span: 19..20,
                    value: ";".to_string(),
                },
            ],
        )
    }
}
