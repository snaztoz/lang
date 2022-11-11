use crate::token::Token;

pub type PackageNameTokens = Vec<Token>;

#[derive(Debug, Default, PartialEq)]
pub struct Ast {
    pub statements: Vec<AstNode>,
}

#[derive(Debug, PartialEq)]
pub enum AstNode {
    Package(PackageNameTokens),
    Import(PackageNameTokens),
}
