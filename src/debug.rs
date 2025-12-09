use crate::closure::{Fundef, Prog, Term};
use std::fmt;

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Unit => write!(f, "Unit"),
            Term::Int(i) => write!(f, "{}", i),
            Term::Float(d) => write!(f, "{:.6}", d), // Format float
            Term::Neg(x) => write!(f, "-{}", x),
            Term::Add(x, y) => write!(f, "{} + {}", x, y),
            Term::Sub(x, y) => write!(f, "{} - {}", x, y),
            Term::FNeg(x) => write!(f, "-.{}", x),
            Term::FAdd(x, y) => write!(f, "{} +. {}", x, y),
            Term::FSub(x, y) => write!(f, "{} -. {}", x, y),
            Term::FMul(x, y) => write!(f, "{} *. {}", x, y),
            Term::FDiv(x, y) => write!(f, "{} /. {}", x, y),
            Term::IfEq(x, y, e1, e2) => write!(f, "If ({} = {}) {{\n{}\n}}\nelse {{\n{}\n}}", x, y, indent(e1), indent(e2)),
            Term::IfLE(x, y, e1, e2) => write!(f, "If ({} <= {}) {{\n{}\n}}\nelse {{\n{}\n}}", x, y, indent(e1), indent(e2)),
            Term::Let((x, t), e1, e2) => {
                match **e1 {
                     Term::Put(_, _, _) => write!(f, "{}{}", e1, e2), // Put doesn't bind
                     _ => write!(f, "{} {} = {};\n{}", t, x, e1, e2),
                }
            }
            Term::Var(x) => write!(f, "{}", x),
            Term::MakeCls((x, t), cls, e) => {
                let actual_fv_str = cls.actual_fv.join(", ");
                write!(f, "{} {} = MakeCls({}, [{}]);\n{}", t, x, cls.entry, actual_fv_str, e)
            }
            Term::AppCls(x, ys) => {
                let args_str = ys.join(", ");
                write!(f, "{}[[Cls]]({})", x, args_str)
            }
            Term::AppDir(l, ys) => {
                let args_str = ys.join(", ");
                write!(f, "{}[[Dir]]({})", l, args_str)
            }
            Term::Tuple(xs) => {
                let elems_str = xs.join(", ");
                write!(f, "({})", elems_str)
            }
            Term::LetTuple(xts, y, e) => {
                let vars_str = xts.iter().map(|(x, t)| format!("({}, {})", x, t)).collect::<Vec<_>>().join(", ");
                write!(f, "({}) = {};\n{}", vars_str, y, e)
            }
            Term::Get(x, y) => write!(f, "{}[{}]", x, y),
            Term::Put(x, y, z) => write!(f, "{}[{}] = {};\n", x, y, z),
            Term::ExtArray(x) => write!(f, "ExtArray({})", x),
        }
    }
}

fn indent(e: &Term) -> String {
    let s = format!("{}", e);
    s.lines().map(|line| format!("  {}", line)).collect::<Vec<_>>().join("\n")
}

impl fmt::Display for Fundef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let args_str = self.args.iter().map(|(x, t)| format!("({}, {})", x, t)).collect::<Vec<_>>().join(", ");
        let body_str = indent(&self.body);
        write!(f, "LetRec{{\nname: ({}, {});\nargs: [{}];\nbody: \n{}\n}}\n", self.name.0, self.name.1, args_str, body_str)
    }
}

impl fmt::Display for Prog {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for fundef in &self.fundefs {
            write!(f, "{}", fundef)?;
        }
        write!(f, "{}", self.body)
    }
}
