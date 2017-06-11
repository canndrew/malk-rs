use std::fmt;
use lexer::Span;
use parse::{Ident, Expr, Pattern};

#[derive(Debug, Clone, PartialEq)]
pub enum FuncType {
    Singular(SingularFuncType),
    Enum(EnumFuncType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SingularFuncType {
    pub pattern: Box<Pattern>,
    pub body_type: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumFuncType {
    pub left_branches: Vec<EnumFuncTypeLeft>,
    pub right_branch: Option<Box<FuncType>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumFuncTypeLeft {
    pub left_name: Option<Ident>,
    pub func: FuncType,
    pub span: Span,
}

impl fmt::Display for FuncType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FuncType::Singular(ref singular_func_type) => write!(f, "{}", singular_func_type),
            FuncType::Enum(ref enum_func_type) => write!(f, "{}", enum_func_type),
        }
    }
}

impl fmt::Display for SingularFuncType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} -> {}", self.pattern, self.body_type)
    }
}

impl fmt::Display for EnumFuncType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        for branch in &self.left_branches {
            if let Some(ref name) = branch.left_name {
                write!(f, "{} = ", name)?;
            }
            write!(f, "{},", branch.func)?;
        }
        if let Some(ref branch) = self.right_branch {
            write!(f, "... {}", branch)?;
        }
        write!(f, "]")
    }
}


