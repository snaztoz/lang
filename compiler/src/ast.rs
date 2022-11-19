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

    If {
        condition: Box<AstNode>,
        block: Ast,
    },
    ElseIf {
        condition: Box<AstNode>,
        block: Ast,
    },
    Else {
        block: Ast,
    },

    MemberAccess {
        accessed: Box<AstNode>,
        member: Box<AstNode>,
    },
    KeyAccess {
        accessed: Box<AstNode>,
        key: Box<AstNode>,
    },
    FunctionCall {
        func: Box<AstNode>,
        args: Vec<AstNode>,
    },

    Neg(Box<AstNode>),
    Not(Box<AstNode>),

    Mul(Box<AstNode>, Box<AstNode>),
    Div(Box<AstNode>, Box<AstNode>),
    Add(Box<AstNode>, Box<AstNode>),
    Sub(Box<AstNode>, Box<AstNode>),
    Shl(Box<AstNode>, Box<AstNode>),
    Shr(Box<AstNode>, Box<AstNode>),

    Less(Box<AstNode>, Box<AstNode>),
    LessEqual(Box<AstNode>, Box<AstNode>),
    Greater(Box<AstNode>, Box<AstNode>),
    GreaterEqual(Box<AstNode>, Box<AstNode>),
    Equal(Box<AstNode>, Box<AstNode>),
    NotEqual(Box<AstNode>, Box<AstNode>),

    And(Box<AstNode>, Box<AstNode>),
    Or(Box<AstNode>, Box<AstNode>),
    Xor(Box<AstNode>, Box<AstNode>),

    LogicalAnd(Box<AstNode>, Box<AstNode>),
    LogicalOr(Box<AstNode>, Box<AstNode>),

    Factor(Token),
}

impl AstNode {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}
