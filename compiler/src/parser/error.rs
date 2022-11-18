use crate::token::Token;

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    UnexpectedToken(Token),
    UnexpectedEOF,
}
