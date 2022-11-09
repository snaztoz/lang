use logos::Logos;

#[derive(Debug, Logos, PartialEq)]
enum TokenKind {
    #[regex("[A-Za-z_][A-Za-z0-9_]*")]
    Ident,

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,

    /**
     * Literals
     */

    #[regex("[0-9]+")]
    #[regex("0(?i)x[a-f0-9]+")] // hex
    #[regex("0(?i)o[0-7]+")] // oct
    #[regex("0(?i)b[0-1]+")] // bin
    Integer,
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

        #[test]
        fn test_integer() {
            run_tests(
                &["255", "0xFf256", "0o77", "0B0110"],
                TokenKind::Integer,
                true,
            );
        }
    }

    mod failed {
        use super::*;

        #[test]
        fn test_identifier() {
            run_tests(&["$abc", "123abc"], TokenKind::Ident, false);
        }

        #[test]
        fn test_integer() {
            run_tests(
                &["255g", "0xFf256g", "0o80", "00B0110", "0Z10"],
                TokenKind::Integer,
                false,
            );
        }
    }
}
