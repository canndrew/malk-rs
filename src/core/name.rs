use std::rc::Rc;
use std::fmt;

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct Name(Rc<String>);

impl Name {
    pub fn new(s: Rc<String>) -> Name {
        Name(s.into())
    }
}

impl<S> From<S> for Name
    where String: From<S>
{
    fn from(s: S) -> Name {
        Name(Rc::new(String::from(s)))
    }
}

impl AsRef<str> for Name {
    fn as_ref(&self) -> &str {
        let Name(ref rc_string) = *self;
        rc_string
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Name(ref rc_string) = *self;
        write!(f, "{}", rc_string)
    }
}

