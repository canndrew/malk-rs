use core::Name;
use parse::Ident;

pub fn build_ident(ident: &Ident) -> Name {
    Name::new(ident.text.clone())
}

pub fn build_opt_ident(ident: &Option<Ident>) -> Option<Name> {
    match *ident {
        Some(ref i) => Some(build_ident(i)),
        None => None,
    }
}

