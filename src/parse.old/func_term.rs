use std::fmt;
use lexer::Span;
use parse::{Ident, Expr, Pattern};

#[derive(Debug, Clone, PartialEq)]
pub enum FuncTerm {
    Singular(SingularFuncTerm),
    Enum(EnumFuncTerm),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SingularFuncTerm {
    pub pattern: Box<Pattern>,
    pub body: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumFuncTerm {
    pub left_branches: Vec<EnumFuncTermLeft>,
    pub right_branch: Option<Box<FuncTerm>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumFuncTermLeft {
    pub left_name: Option<Ident>,
    pub func: FuncTerm,
    pub span: Span,
}


/*
pub fn parse_enum_func_term_inner<'t, 's: 't>(
        tokens: TokensRef<'t, 's>,
        left_branches: &mut Vec<EnumFuncTermLeft>
    ) -> Result<EnumFuncTerm, ParseError>
{
    for (i, token) in ts.tokens.iter() {
        match token {
            TokenKind::Symbol("..") => {
                let 
            },
            TokenKind::Whitespace(_) => (),
            _ => break,
        }
    }

    for (i, token) in ts.tokens.iter().enumerate() {
        match token {
            TokenKind::Symbol(",") => {
            },
        }
    };

    if contains_anything {

    }


                let left_tokens = ts.range(elem_start, i);
                let left = parse_left_branch(left_tokens)?;
                left_branches.push(left);
                elem_start = i + 1;
            },
            TokenKind::Symbol(";") => {
            },
        }
    }
    unimplemented!()
}
*/

impl fmt::Display for FuncTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FuncTerm::Singular(ref singular_func_term) => write!(f, "{}", singular_func_term),
            FuncTerm::Enum(ref enum_func_term) => write!(f, "{}", enum_func_term),
        }
    }
}

impl fmt::Display for SingularFuncTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} => {}", self.pattern, self.body)
    }
}

impl fmt::Display for EnumFuncTerm {
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

