use std::str::FromStr;

use core::{Term, TermKind};
use parse::Ident;
use typecheck::{TypeCheckError, TypeMismatch};

pub fn typecheck_numeric(
        ident: &Ident,
        expected_type: &Term) -> Result<Term, TypeCheckError>
{
    if ident.text.ends_with("lev") {
        let n_str = &ident.text[0 .. ident.text.len() - 3];
        if let Ok(n) = u64::from_str(n_str) {
            let mut ret = Term::new(TermKind::LevelZero);
            for _ in 0..n {
                ret = Term::new(TermKind::LevelSucc {
                    pred: ret,
                });
            }
            return match **expected_type {
                TermKind::Level => Ok(ret),
                _ => Err(TypeCheckError::TypeMismatch(TypeMismatch {
                    span: ident.span,
                    expected: expected_type.clone(),
                })),
            }
        }
    }
    Err(TypeCheckError::InvalidNumeric {
        numeric: ident.text.clone(),
        span: ident.span,
    })
}

