use std::rc::Rc;
use num::BigUint;

use core::{Ctx, Term};

#[derive(Debug, Clone, PartialEq, Copy, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span {
            start: start,
            end: end,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr<K> {
    pub kind: K,
    pub term: Option<Term>,
    pub ctx: Option<Ctx>,
}

// TODO: find out why it can't infer these
unsafe impl<K: Send> Send for Expr<K> {}
unsafe impl Send for BlockPrecedenceExprKind {}
unsafe impl Send for OperatorPrecedenceExprKind {}
unsafe impl Send for AppPrecedenceExprKind {}
unsafe impl Send for EnclosedPrecedenceExprKind {}
unsafe impl Send for FuncTerm {}
unsafe impl Send for FuncType {}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockPrecedenceExprKind {
    LetExpr(Box<LetExpr>),
    OperatorPrecedence(OperatorPrecedenceExprKind),
}

#[derive(Debug, Clone, PartialEq)]
pub enum OperatorPrecedenceExprKind {
    SingularFuncTerm(Box<SingularFuncTerm>),
    SingularFuncType(Box<SingularFuncType>),
    EqualityTerm(Box<EqualityTerm>),
    EqualityType(Box<EqualityType>),
    Typed(Box<TypedExpr>),
    RecType(Box<RecType>),
    AppPrecedence(AppPrecedenceExprKind),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AppPrecedenceExprKind {
    FuncApp(Box<FuncApp>),
    EnclosedPrecedence(EnclosedPrecedenceExprKind),
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnclosedPrecedenceExprKind {
    EnumFuncTerm(Box<EnumFuncTerm>),
    EnumFuncType(Box<EnumFuncType>),
    Parens(Box<ParensExpr>),
    StructTerm(Box<StructTerm>),
    StructType(Box<StructType>),
    EnumType(Box<EnumType>),
    EnumTerm(Box<EnumTerm>),
    Variable(Ident),
    TypeLiteral(TypeLiteral),
    LevelLiteral(LevelLiteral),
    IntLiteral(IntLiteral),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParensExpr {
    pub sub_expr: Expr<BlockPrecedenceExprKind>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetExpr {
    pub pattern: Pattern,
    pub let_to: Expr<OperatorPrecedenceExprKind>,
    pub let_in: Expr<BlockPrecedenceExprKind>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub text: Rc<String>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FuncTerm {
    Singular(SingularFuncTerm),
    Enum(EnumFuncTerm),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SingularFuncTerm {
    pub pattern: Box<Pattern>,
    pub res: Box<Expr<OperatorPrecedenceExprKind>>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub kind: PatternKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    Parens(Box<ParensPattern>),
    Bind(Ident),
    Typed(Box<TypedPattern>),
    Struct(Box<StructPattern>),
    Equality(Box<EqualityPattern>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParensPattern {
    pub sub_pattern: Pattern,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedPattern {
    pub sub_pattern: Pattern,
    pub pattern_type: Expr<OperatorPrecedenceExprKind>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructPattern {
    pub head_elems: Vec<CompositePatternElem>,
    pub tail: Option<Pattern>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompositePatternElem {
    pub name: Option<Ident>,
    pub sub_pattern: Pattern,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EqualityPattern {
    pub a: Expr<AppPrecedenceExprKind>,
    pub b: Expr<AppPrecedenceExprKind>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructTerm {
    pub head_elems: Vec<CompositeTermElem>,
    pub tail: Option<Expr<OperatorPrecedenceExprKind>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompositeTermElem {
    pub ident: Option<Ident>,
    pub expr: Expr<OperatorPrecedenceExprKind>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumTerm {
    pub kind: EnumTermKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumTermKind {
    Left(CompositeTermElem),
    Right(Expr<OperatorPrecedenceExprKind>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructType {
    pub head_elems: Vec<CompositeTypeElem>,
    pub tail: Option<Expr<OperatorPrecedenceExprKind>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompositeTypeElem {
    pub ident: Option<Ident>,
    pub expr: Expr<OperatorPrecedenceExprKind>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumType {
    pub head_elems: Vec<CompositeTypeElem>,
    pub tail: Option<Expr<OperatorPrecedenceExprKind>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FuncType {
    Singular(SingularFuncType),
    Enum(EnumFuncType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SingularFuncType {
    pub pattern: Box<Pattern>,
    pub res_type: Box<Expr<OperatorPrecedenceExprKind>>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct FuncApp {
    pub func: Expr<AppPrecedenceExprKind>,
    pub arg: Expr<EnclosedPrecedenceExprKind>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeLiteral {
    pub rank: usize,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LevelLiteral {
    pub rank: usize,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpr {
    pub typed_term: Expr<AppPrecedenceExprKind>,
    pub typed_type: Expr<OperatorPrecedenceExprKind>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecType {
    pub name: Ident,
    pub rec_type: Expr<AppPrecedenceExprKind>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EqualityTerm {
    pub a: Expr<AppPrecedenceExprKind>,
    pub b: Expr<AppPrecedenceExprKind>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EqualityType {
    pub a: Expr<AppPrecedenceExprKind>,
    pub b: Expr<AppPrecedenceExprKind>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IntType {
    Ubig,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntLiteral {
    pub bits: BigUint,
    pub int_type: IntType,
    pub span: Span,
}

pub trait Syntax {
    fn span(&self) -> Span;

    fn sub_syntax(&self) -> Vec<&Syntax>;

    fn term(&self) -> Option<Term> {
        None
    }

    fn ctx(&self) -> Option<Ctx> {
        None
    }
}

impl<K: Syntax> Syntax for Expr<K> {
    fn span(&self) -> Span {
        self.kind.span()
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        println!("\n## in Expr::sub_syntax");
        self.kind.sub_syntax()
    }

    fn term(&self) -> Option<Term> {
        self.term.clone()
    }

    fn ctx(&self) -> Option<Ctx> {
        self.ctx.clone()
    }
}

impl Syntax for BlockPrecedenceExprKind {
    fn span(&self) -> Span {
        match *self {
            BlockPrecedenceExprKind::LetExpr(ref s) => s.span(),
            BlockPrecedenceExprKind::OperatorPrecedence(ref s) => s.span(),
        }
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        println!("\n## in BlockPrecedence::sub_syntax");
        match *self {
            BlockPrecedenceExprKind::LetExpr(ref s) => s.sub_syntax(),
            BlockPrecedenceExprKind::OperatorPrecedence(ref s) => s.sub_syntax(),
        }
    }
}

impl Syntax for OperatorPrecedenceExprKind {
    fn span(&self) -> Span {
        match *self {
            OperatorPrecedenceExprKind::SingularFuncTerm(ref s) => s.span(),
            OperatorPrecedenceExprKind::SingularFuncType(ref s) => s.span(),
            OperatorPrecedenceExprKind::EqualityTerm(ref s) => s.span(),
            OperatorPrecedenceExprKind::EqualityType(ref s) => s.span(),
            OperatorPrecedenceExprKind::Typed(ref s) => s.span(),
            OperatorPrecedenceExprKind::RecType(ref s) => s.span(),
            OperatorPrecedenceExprKind::AppPrecedence(ref s) => s.span(),
        }
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        match *self {
            OperatorPrecedenceExprKind::SingularFuncTerm(ref s) => s.sub_syntax(),
            OperatorPrecedenceExprKind::SingularFuncType(ref s) => s.sub_syntax(),
            OperatorPrecedenceExprKind::EqualityTerm(ref s) => s.sub_syntax(),
            OperatorPrecedenceExprKind::EqualityType(ref s) => s.sub_syntax(),
            OperatorPrecedenceExprKind::Typed(ref s) => s.sub_syntax(),
            OperatorPrecedenceExprKind::RecType(ref s) => s.sub_syntax(),
            OperatorPrecedenceExprKind::AppPrecedence(ref s) => s.sub_syntax(),
        }
    }
}

impl Syntax for AppPrecedenceExprKind {
    fn span(&self) -> Span {
        match *self {
            AppPrecedenceExprKind::FuncApp(ref s) => s.span(),
            AppPrecedenceExprKind::EnclosedPrecedence(ref s) => s.span(),
        }
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        match *self {
            AppPrecedenceExprKind::FuncApp(ref s) => s.sub_syntax(),
            AppPrecedenceExprKind::EnclosedPrecedence(ref s) => s.sub_syntax(),
        }
    }
}

impl Syntax for EnclosedPrecedenceExprKind {
    fn span(&self) -> Span {
        match *self {
            EnclosedPrecedenceExprKind::EnumFuncType(ref s) => s.span(),
            EnclosedPrecedenceExprKind::EnumFuncTerm(ref s) => s.span(),
            EnclosedPrecedenceExprKind::Parens(ref s) => s.span(),
            EnclosedPrecedenceExprKind::StructTerm(ref s) => s.span(),
            EnclosedPrecedenceExprKind::StructType(ref s) => s.span(),
            EnclosedPrecedenceExprKind::EnumType(ref s) => s.span(),
            EnclosedPrecedenceExprKind::EnumTerm(ref s) => s.span(),
            EnclosedPrecedenceExprKind::Variable(ref s) => s.span(),
            EnclosedPrecedenceExprKind::TypeLiteral(ref s) => s.span(),
            EnclosedPrecedenceExprKind::LevelLiteral(ref s) => s.span(),
            EnclosedPrecedenceExprKind::IntLiteral(ref s) => s.span(),
        }
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        match *self {
            EnclosedPrecedenceExprKind::EnumFuncType(ref s) => s.sub_syntax(),
            EnclosedPrecedenceExprKind::EnumFuncTerm(ref s) => s.sub_syntax(),
            EnclosedPrecedenceExprKind::Parens(ref s) => s.sub_syntax(),
            EnclosedPrecedenceExprKind::StructTerm(ref s) => s.sub_syntax(),
            EnclosedPrecedenceExprKind::StructType(ref s) => s.sub_syntax(),
            EnclosedPrecedenceExprKind::EnumType(ref s) => s.sub_syntax(),
            EnclosedPrecedenceExprKind::EnumTerm(ref s) => s.sub_syntax(),
            EnclosedPrecedenceExprKind::Variable(ref s) => s.sub_syntax(),
            EnclosedPrecedenceExprKind::TypeLiteral(ref s) => s.sub_syntax(),
            EnclosedPrecedenceExprKind::LevelLiteral(ref s) => s.sub_syntax(),
            EnclosedPrecedenceExprKind::IntLiteral(ref s) => s.sub_syntax(),
        }
    }
}

impl Syntax for ParensExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        vec![&self.sub_expr]
    }
}

impl Syntax for FuncTerm {
    fn span(&self) -> Span {
        match *self {
            FuncTerm::Singular(ref s) => s.span(),
            FuncTerm::Enum(ref s) => s.span(),
        }
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        match *self {
            FuncTerm::Singular(ref s) => s.sub_syntax(),
            FuncTerm::Enum(ref s) => s.sub_syntax(),
        }
    }
}

impl Syntax for LetExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        println!("\n## in LetExpr::sub_syntax");
        vec![&self.pattern, &self.let_to, &self.let_in]
    }
}

impl Syntax for StructTerm {
    fn span(&self) -> Span {
        self.span
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        let mut v = Vec::new();
        for elem in &self.head_elems {
            v.push(&elem.expr as &Syntax);
        }
        if let Some(ref elem) = self.tail {
            v.push(elem as &Syntax);
        }
        v
    }
}

impl Syntax for StructType {
    fn span(&self) -> Span {
        self.span
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        let mut v = Vec::new();
        for elem in &self.head_elems {
            v.push(&elem.expr as &Syntax);
        }
        if let Some(ref elem) = self.tail {
            v.push(elem as &Syntax);
        }
        v
    }
}

impl Syntax for EnumType {
    fn span(&self) -> Span {
        self.span
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        let mut v = Vec::new();
        for elem in &self.head_elems {
            v.push(&elem.expr as &Syntax);
        }
        if let Some(ref elem) = self.tail {
            v.push(elem as &Syntax);
        }
        v
    }
}

impl Syntax for EnumTerm {
    fn span(&self) -> Span {
        self.span
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        match self.kind {
            EnumTermKind::Left(ref left) => vec![left],
            EnumTermKind::Right(ref right) => vec![right],
        }
    }
}

impl Syntax for FuncType {
    fn span(&self) -> Span {
        match *self {
            FuncType::Singular(ref s) => s.span(),
            FuncType::Enum(ref s) => s.span(),
        }
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        match *self {
            FuncType::Singular(ref s) => s.sub_syntax(),
            FuncType::Enum(ref s) => s.sub_syntax(),
        }
    }
}

impl Syntax for FuncApp {
    fn span(&self) -> Span {
        Span::new(self.func.span().start, self.arg.span().end)
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        vec![&self.func, &self.arg]
    }
}

impl Syntax for EqualityTerm {
    fn span(&self) -> Span {
        Span::new(self.a.span().start, self.b.span().end)
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        vec![&self.a, &self.b]
    }
}

impl Syntax for EqualityType {
    fn span(&self) -> Span {
        Span::new(self.a.span().start, self.b.span().end)
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        vec![&self.a, &self.b]
    }
}

impl Syntax for TypeLiteral {
    fn span(&self) -> Span {
        self.span
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        vec![]
    }
}

impl Syntax for LevelLiteral {
    fn span(&self) -> Span {
        self.span
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        vec![]
    }
}

impl Syntax for TypedExpr {
    fn span(&self) -> Span {
        Span::new(self.typed_term.span().start, self.typed_type.span().end)
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        vec![&self.typed_term, &self.typed_type]
    }
}

impl Syntax for RecType {
    fn span(&self) -> Span {
        self.span
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        vec![&self.rec_type]
    }
}

impl Syntax for SingularFuncTerm {
    fn span(&self) -> Span {
        Span::new(self.pattern.span().start, self.res.span().end)
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        vec![&*self.pattern, &*self.res]
    }
}

impl Syntax for EnumFuncTerm {
    fn span(&self) -> Span {
        self.span
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        let mut v = Vec::new();
        for branch in &self.left_branches {
            v.push(&branch.func as &Syntax);
        }
        if let Some(ref branch) = self.right_branch {
            v.push(&**branch as &Syntax);
        }
        v
    }
}

impl Syntax for SingularFuncType {
    fn span(&self) -> Span {
        Span::new(self.pattern.span().start, self.res_type.span().end)
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        vec![&*self.pattern, &*self.res_type]
    }
}

impl Syntax for EnumFuncType {
    fn span(&self) -> Span {
        self.span
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        let mut v = Vec::new();
        for branch in &self.left_branches {
            v.push(&branch.func as &Syntax);
        }
        if let Some(ref branch) = self.right_branch {
            v.push(&**branch as &Syntax);
        }
        v
    }
}

impl Syntax for Pattern {
    fn span(&self) -> Span {
        match self.kind {
            PatternKind::Parens(ref p) => p.span(),
            PatternKind::Bind(ref p) => p.span(),
            PatternKind::Typed(ref p) => p.span(),
            PatternKind::Struct(ref p) => p.span(),
            PatternKind::Equality(ref p) => p.span(),
        }
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        match self.kind {
            PatternKind::Parens(ref p) => p.sub_syntax(),
            PatternKind::Bind(ref p) => p.sub_syntax(),
            PatternKind::Typed(ref p) => p.sub_syntax(),
            PatternKind::Struct(ref p) => p.sub_syntax(),
            PatternKind::Equality(ref p) => p.sub_syntax(),
        }
    }
}

impl Syntax for ParensPattern {
    fn span(&self) -> Span {
        self.span
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        vec![&self.sub_pattern]
    }
}

impl Syntax for Ident {
    fn span(&self) -> Span {
        self.span
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        vec![]
    }
}

impl Syntax for TypedPattern {
    fn span(&self) -> Span {
        Span::new(self.sub_pattern.span().start, self.pattern_type.span().end)
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        vec![&self.sub_pattern, &self.pattern_type]
    }
}

impl Syntax for StructPattern {
    fn span(&self) -> Span {
        self.span
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        let mut v = Vec::new();
        for elem in &self.head_elems {
            v.push(&elem.sub_pattern as &Syntax);
        }
        if let Some(ref elem) = self.tail {
            v.push(&*elem as &Syntax);
        }
        v
    }
}

impl Syntax for EqualityPattern {
    fn span(&self) -> Span {
        Span::new(self.a.span().start, self.b.span().end)
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        vec![&self.a, &self.b]
    }
}

impl Syntax for CompositeTermElem {
    fn span(&self) -> Span {
        self.span
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        let mut v = match self.ident {
            Some(ref i) => vec![i as &Syntax],
            None => vec![],
        };
        v.push(&self.expr as &Syntax);
        v
    }
}

impl Syntax for CompositeTypeElem {
    fn span(&self) -> Span {
        self.span
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        let mut v = match self.ident {
            Some(ref i) => vec![i as &Syntax],
            None => vec![],
        };
        v.push(&self.expr as &Syntax);
        v
    }
}

impl Syntax for CompositePatternElem {
    fn span(&self) -> Span {
        self.span
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        let mut v = match self.name {
            Some(ref i) => vec![i as &Syntax],
            None => vec![],
        };
        v.push(&self.sub_pattern as &Syntax);
        v
    }
}

impl Syntax for IntLiteral {
    fn span(&self) -> Span {
        self.span
    }

    fn sub_syntax(&self) -> Vec<&Syntax> {
        vec![]
    }
}

/*
impl<K: Syntax> Expr<K> {
    pub fn sub_exprs_at_pos(&self, pos: usize) -> Vec<&Expr> {
        // note: abusing `Result` so we can use the ? operator
        //       we just want to return early if we find an answer
        fn inner<L: Syntax>(expr: &Expr<L>, pos: usize) -> Result<(), Vec<&Expr>> {
            if expr.span().start > pos || expr.span().end <= pos {
                return Ok(());
            }

            let find_better = || -> Result<(), Vec<&Expr>> {
                match expr.kind {
                    ExprKind::Parens(ref parens) => {
                        inner(&parens.sub_expr, pos)?;
                    },
                    ExprKind::LetExpr(ref let_expr) => {
                        inner(&let_expr.let_to, pos)?;
                        inner(&let_expr.let_in, pos)?;
                    },
                    ExprKind::StructTerm(ref struct_term) => {
                        for elem in &struct_term.head_elems {
                            inner(&elem.expr, pos)?;
                        }
                        if let Some(ref tail) = struct_term.tail {
                            inner(tail, pos)?;
                        }
                    },
                    ExprKind::StructType(ref struct_type) => {
                        for elem in &struct_type.head_elems {
                            inner(&elem.expr, pos)?;
                        }
                        if let Some(ref tail) = struct_type.tail {
                            inner(tail, pos)?;
                        }
                    },
                    ExprKind::EnumType(ref enum_type) => {
                        for elem in &enum_type.head_elems {
                            inner(&elem.expr, pos)?;
                        }
                        if let Some(ref tail) = enum_type.tail {
                            inner(tail, pos)?;
                        }
                    },
                    ExprKind::EnumTerm(ref enum_term) => {
                        match enum_term.kind {
                            EnumTermKind::Left(ref elem) => {
                                inner(&elem.expr, pos)?;
                            },
                            EnumTermKind::Right(ref expr) => {
                                inner(expr, pos)?;
                            },
                        }
                    },
                    ExprKind::FuncTerm(ref func_term) => {
                        fn try_func_term(func_term: &FuncTerm, pos: usize) -> Result<(), Vec<&Expr>> {
                            match *func_term {
                                FuncTerm::Singular(ref singular_func_term) => {
                                    inner(&singular_func_term.res, pos)?;
                                },
                                FuncTerm::Enum(ref enum_func_term) => {
                                    for branch in &enum_func_term.left_branches {
                                        try_func_term(&branch.func, pos)?;
                                    }
                                    if let Some(ref right) = enum_func_term.right_branch {
                                        try_func_term(right, pos)?;
                                    }
                                },
                            };
                            Ok(())
                        }

                        try_func_term(func_term, pos)?;
                    },
                    ExprKind::FuncType(ref func_type) => {
                        fn try_func_type(func_type: &FuncType, pos: usize) -> Result<(), Vec<&Expr>> {
                            match *func_type {
                                FuncType::Singular(ref singular_func_type) => {
                                    inner(&singular_func_type.res_type, pos)?;
                                },
                                FuncType::Enum(ref enum_func_type) => {
                                    for branch in &enum_func_type.left_branches {
                                        try_func_type(&branch.func, pos)?;
                                    }
                                    if let Some(ref right) = enum_func_type.right_branch {
                                        try_func_type(right, pos)?;
                                    }
                                },
                            };
                            Ok(())
                        }

                        try_func_type(func_type, pos)?;
                    },
                    ExprKind::FuncApp(ref func_app) => {
                        inner(&func_app.func, pos)?;
                        inner(&func_app.arg, pos)?;
                    },
                    ExprKind::Variable(..) |
                    ExprKind::TypeLiteral(..) |
                    ExprKind::LevelLiteral(..) |
                    ExprKind::Numeric(..) => (),

                    ExprKind::Typed(ref typed) => {
                        inner(&typed.typed_term, pos)?;
                        inner(&typed.typed_type, pos)?;
                    },
                    ExprKind::EqualityTerm(ref equality_term) => {
                        inner(&equality_term.a, pos)?;
                        inner(&equality_term.b, pos)?;
                    },
                    ExprKind::EqualityType(ref equality_type) => {
                        inner(&equality_type.a, pos)?;
                        inner(&equality_type.b, pos)?;
                    },
                };
                Ok(())
            };
            match find_better() {
                Err(mut v) => {
                    v.push(expr);
                    Err(v)
                },
                Ok(()) => Err(vec![expr]),
            }
        }

        match inner(self, pos) {
            Err(v) => v,
            Ok(()) => vec![],
        }
    }
}
*/

