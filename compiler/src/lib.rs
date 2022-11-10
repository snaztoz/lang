mod lexer;
mod parser;
mod token;

pub fn greet() {
    let tokens = lexer::lex("package foo;");
    let _ast = parser::parse(tokens);
}
