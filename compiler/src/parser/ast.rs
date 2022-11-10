use crate::token::Token;

pub type PackageNameTokens = Vec<Token>;

#[derive(Debug, Default, PartialEq)]
pub struct Context {
    pub ast: Ast,
}

#[derive(Debug, Default, PartialEq)]
pub struct Ast {
    pub package: Option<PackageNameTokens>,
    pub imports: Vec<PackageNameTokens>,
    pub statements: Vec<String>,
}
