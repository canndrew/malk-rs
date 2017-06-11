use core::Name;

#[derive(Clone)]
pub enum RopeUnit {
    Borrowed(&'static str),
    Owned(String),
    SubRope(Rope),
    Name(Name),
}

#[derive(Clone)]
pub struct Rope {
    units: Vec<RopeUnit>,
}

impl Rope {
    pub fn new() -> Rope {
        Rope {
            units: Vec::new(),
        }
    }

    pub fn from_units(units: Vec<RopeUnit>) -> Rope {
        Rope {
            units: units,
        }
    }

    pub fn push<U>(&mut self, unit: U)
        where U: Into<RopeUnit>
    {
        self.units.push(unit.into());
    }

    pub fn len(&self) -> usize {
        let mut ret = 0;
        for unit in &self.units {
            ret += match *unit {
                RopeUnit::Borrowed(s) => s.len(),
                RopeUnit::Owned(ref s) => s.len(),
                RopeUnit::SubRope(ref r) => r.len(),
                RopeUnit::Name(ref n) => n.as_ref().len(),
            };
        }
        ret
    }

    pub fn as_string(&self) -> String {
        let mut ret = String::with_capacity(self.len());
        self.append_to_string(&mut ret);
        ret
    }

    pub fn append_to_string(&self, acc: &mut String) {
        for unit in &self.units {
            match *unit {
                RopeUnit::Borrowed(s) => acc.push_str(s),
                RopeUnit::Owned(ref s) => acc.push_str(s),
                RopeUnit::SubRope(ref r) => r.append_to_string(acc),
                RopeUnit::Name(ref n) => acc.push_str(n.as_ref()),
            };
        }
    }
}

impl Into<RopeUnit> for &'static str {
    fn into(self) -> RopeUnit {
        RopeUnit::Borrowed(self)
    }
}

impl Into<RopeUnit> for String {
    fn into(self) -> RopeUnit {
        RopeUnit::Owned(self)
    }
}

impl Into<RopeUnit> for Rope {
    fn into(self) -> RopeUnit {
        RopeUnit::SubRope(self)
    }
}

impl<'a> Into<RopeUnit> for &'a Name {
    fn into(self) -> RopeUnit {
        RopeUnit::Name(self.clone())
    }
}

macro_rules! rope (
    ( $( $x:expr ),* ) => (
        {
            let capacity = 0 $( + (|_| 1)(&$x) )*;
            let mut vec = Vec::with_capacity(capacity);
            $(
                vec.push(($x).into());
            )*
            Rope::from_units(vec)
        }
    )
);



