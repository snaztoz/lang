use logos::Logos;

#[derive(Debug, Logos, PartialEq)]
enum TokenKind {
    #[regex("[A-Za-z_][A-Za-z0-9_]*")]
    Ident,

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run_tests(tests: &[&str], kind: TokenKind, is_expected: bool) {
        for (i, input) in tests.into_iter().enumerate() {
            let tokens = TokenKind::lexer(input).collect::<Vec<_>>();
            let pass = if is_expected {
                tokens.len() == 1 && tokens[0] == kind
            } else {
                tokens.len() != 1 || tokens[0] != kind
            };
            if !pass {
                assert!(false, "[#{}] input: `{}`", i + 1, input);
            }
        }
    }

    mod success {
        use super::*;

        #[test]
        fn test_identifier() {
            run_tests(&["abc123", "_123_abc_00__", "_"], TokenKind::Ident, true);
        }
    }

    mod failed {
        use super::*;

        #[test]
        fn test_identifier() {
            run_tests(&["$abc", "123abc"], TokenKind::Ident, false);
        }
    }
}
