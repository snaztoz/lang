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
    Mul(Box<AstNode>, Box<AstNode>),
    Div(Box<AstNode>, Box<AstNode>),
    Add(Box<AstNode>, Box<AstNode>),
    Sub(Box<AstNode>, Box<AstNode>),
    Shl(Box<AstNode>, Box<AstNode>),
    Shr(Box<AstNode>, Box<AstNode>),
    Factor(Token),
}

impl AstNode {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}
