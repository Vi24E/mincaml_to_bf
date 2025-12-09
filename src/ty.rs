use std::cell::RefCell;
use std::rc::Rc;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Float,
    Fun(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Array(Box<Type>),
    Var(Rc<RefCell<Option<Type>>>),
}

impl Type {
    pub fn gentyp() -> Type {
        Type::Var(Rc::new(RefCell::new(None)))
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "unit"),
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Fun(args, ret) => {
                write!(f, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, " -> ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, " -> {})", ret)
            }
            Type::Tuple(elems) => {
                write!(f, "(")?;
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " * ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, ")")
            }
            Type::Array(t) => write!(f, "{} array", t),
            Type::Var(r) => {
                match &*r.borrow() {
                    Some(t) => write!(f, "{}", t),
                    None => write!(f, "'?"),
                }
            }
        }
    }
}
