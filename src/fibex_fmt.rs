use crate::fibex::{CompuScale, IntervalType, VvT, XsDouble};
use std::ops::Bound;

impl std::fmt::Display for XsDouble {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            XsDouble::F64(a) => f.write_fmt(format_args!("{}", a)),
            XsDouble::I64(a) => f.write_fmt(format_args!("{}", a)),
        }
    }
}

impl std::fmt::Display for VvT {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VvT::V(a) => write!(f, "{}", a),
            VvT::VT(a) => write!(f, "\"{}\"", a),
        }
    }
}

impl IntervalType {
    fn fmt_as_lower(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntervalType(Bound::Unbounded) => f.write_str(".."),
            IntervalType(Bound::Included(a)) => f.write_fmt(format_args!("[{}..", a)),
            IntervalType(Bound::Excluded(a)) => f.write_fmt(format_args!("({}..", a)),
        }
    }
    fn fmt_as_upper(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntervalType(Bound::Unbounded) => f.write_str(".."),
            IntervalType(Bound::Included(a)) => f.write_fmt(format_args!("..{}]", a)),
            IntervalType(Bound::Excluded(a)) => f.write_fmt(format_args!("..{})", a)),
        }
    }
}

impl std::fmt::Display for CompuScale {
    /// we want to implement/print it like:
    /// lower_limit = upper_limit: [val] -> VvT
    /// lower_limit prov but no upper limit = [val.. -> VvT
    /// upper_limit prov but no lower limit = ..val] -> VvT
    /// both prov but diff value: [val1..val2] -> VvT
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let have_lower = self.lower_limit.is_some();
        let have_upper = self.upper_limit.is_some();
        if have_lower && have_upper {
            let lower = self.lower_limit.as_ref().unwrap();
            let upper = self.upper_limit.as_ref().unwrap();
            match lower {
                IntervalType(Bound::Unbounded) => upper.fmt_as_upper(f)?,
                IntervalType(Bound::Included(a)) => match upper {
                    IntervalType(Bound::Unbounded) => lower.fmt_as_lower(f)?,
                    IntervalType(Bound::Included(b)) => {
                        if a.eq(b) {
                            f.write_fmt(format_args!("[{}]", a))
                        } else {
                            f.write_fmt(format_args!("[{}..{}]", a, b))
                        }?;
                    }
                    IntervalType(Bound::Excluded(b)) => {
                        if a.eq(b) {
                            f.write_fmt(format_args!("[{})", a))
                        } else {
                            f.write_fmt(format_args!("[{}..{})", a, b))
                        }?;
                    }
                },
                IntervalType(Bound::Excluded(a)) => match upper {
                    IntervalType(Bound::Unbounded) => lower.fmt_as_lower(f)?,
                    IntervalType(Bound::Excluded(b)) => {
                        if a.eq(b) {
                            f.write_fmt(format_args!("({})", a))
                        } else {
                            f.write_fmt(format_args!("({}..{})", a, b))
                        }?;
                    }
                    IntervalType(Bound::Included(b)) => {
                        if a.eq(b) {
                            f.write_fmt(format_args!("({}]", a))
                        } else {
                            f.write_fmt(format_args!("({}..{}]", a, b))
                        }?;
                    }
                },
            }
        } else if have_lower {
            self.lower_limit.as_ref().unwrap().fmt_as_lower(f)?;
        } else if have_upper {
            self.upper_limit.as_ref().unwrap().fmt_as_upper(f)?;
        } else {
            f.write_str("..")?;
        }
        if let Some(compu_const) = &self.compu_const {
            f.write_fmt(format_args!("-> {}", compu_const))
        } else {
            f.write_str("-> <None>")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fibex::{IntervalType, XsDouble};

    #[test]
    fn fmt_compuscale() {
        let cs = CompuScale {
            mask: None,
            lower_limit: None,
            upper_limit: None,
            compu_const: None,
        };
        assert_eq!(format!("{}", cs).as_str(), "..-> <None>");

        let v1: XsDouble = "42".into();
        let cs = CompuScale {
            mask: None,
            lower_limit: Some(IntervalType(Bound::Included(v1.clone()))),
            upper_limit: None,
            compu_const: None,
        };
        assert_eq!(format!("{}", cs).as_str(), "[42..-> <None>");

        let v2: XsDouble = "43".into();
        let cs = CompuScale {
            mask: None,
            lower_limit: Some(IntervalType(Bound::Included(v1.clone()))),
            upper_limit: Some(IntervalType(Bound::Excluded(v2))),
            compu_const: Some(VvT::VT("foo".into())),
        };
        assert_eq!(format!("{}", cs).as_str(), "[42..43)-> \"foo\"");
        let cs = CompuScale {
            mask: None,
            lower_limit: Some(IntervalType(Bound::Included(v1.clone()))),
            upper_limit: Some(IntervalType(Bound::Included(v1))),
            compu_const: Some(VvT::V("-1.5".into())),
        };
        assert_eq!(format!("{}", cs).as_str(), "[42]-> -1.5");
    }
}
