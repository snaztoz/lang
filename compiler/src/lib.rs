use error::Error;

mod ast;
mod error;
mod lexer;
mod parser;
mod token;

type Result<T> = std::result::Result<T, Error>;

pub fn greet() {
    let tokens = lexer::lex("package foo;");
    let _ast = parser::parse(tokens);
}
