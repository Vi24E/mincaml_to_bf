use crate::closure::{self, Closure};
use crate::id;
use crate::ty::Type;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct Prog {
    pub fundefs: Vec<Fundef>,
    pub body: Term,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Unit,
    Int(i32),
    Float(f64),
    Var(id::T),
    Neg(id::T),
    Add(id::T, id::T),
    Sub(id::T, id::T),
    FNeg(id::T),
    FAdd(id::T, id::T),
    FSub(id::T, id::T),
    FMul(id::T, id::T),
    FDiv(id::T, id::T),
    Get(id::T, id::T),
    Put(id::T, id::T, id::T),
    ExtArray(id::L),
    Tuple(Vec<id::T>),
    MakeCls(Closure),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fundef {
    pub name: (id::T, Type),
    pub args: Vec<(id::T, Type)>,
    pub body: Box<Term>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Let((id::T, Type), Atom, Box<Term>),
    LetTuple(Vec<(id::T, Type)>, id::T, Box<Term>),
    IfEq(id::T, id::T, Box<Term>, Box<Term>),
    IfLE(id::T, id::T, Box<Term>, Box<Term>),
    LetRec(Fundef, Box<Term>),
    AppCls(id::T, Vec<id::T>),
    AppDir(id::L, Vec<id::T>),
}

// CPS transformation
// k: Continuation constructor. Takes the variable holding the result.
pub fn g(e: closure::Term, k: Box<dyn FnOnce(id::T) -> Term>) -> Term {
    match e {
        closure::Term::Unit => {
            let x = id::gentmp(&Type::Unit);
            Term::Let((x.clone(), Type::Unit), Atom::Unit, Box::new(k(x)))
        }
        closure::Term::Int(i) => {
            let x = id::gentmp(&Type::Int);
            Term::Let((x.clone(), Type::Int), Atom::Int(i), Box::new(k(x)))
        }
        closure::Term::Float(d) => {
            let x = id::gentmp(&Type::Float);
            Term::Let((x.clone(), Type::Float), Atom::Float(d), Box::new(k(x)))
        }
        closure::Term::Neg(x) => {
            let y = id::gentmp(&Type::Int);
            Term::Let((y.clone(), Type::Int), Atom::Neg(x), Box::new(k(y)))
        }
        closure::Term::Add(x, y) => {
            let z = id::gentmp(&Type::Int);
            Term::Let((z.clone(), Type::Int), Atom::Add(x, y), Box::new(k(z)))
        }
        closure::Term::Sub(x, y) => {
            let z = id::gentmp(&Type::Int);
            Term::Let((z.clone(), Type::Int), Atom::Sub(x, y), Box::new(k(z)))
        }
        closure::Term::FNeg(x) => {
            let y = id::gentmp(&Type::Float);
            Term::Let((y.clone(), Type::Float), Atom::FNeg(x), Box::new(k(y)))
        }
        closure::Term::FAdd(x, y) => {
            let z = id::gentmp(&Type::Float);
            Term::Let((z.clone(), Type::Float), Atom::FAdd(x, y), Box::new(k(z)))
        }
        closure::Term::FSub(x, y) => {
            let z = id::gentmp(&Type::Float);
            Term::Let((z.clone(), Type::Float), Atom::FSub(x, y), Box::new(k(z)))
        }
        closure::Term::FMul(x, y) => {
            let z = id::gentmp(&Type::Float);
            Term::Let((z.clone(), Type::Float), Atom::FMul(x, y), Box::new(k(z)))
        }
        closure::Term::FDiv(x, y) => {
            let z = id::gentmp(&Type::Float);
            Term::Let((z.clone(), Type::Float), Atom::FDiv(x, y), Box::new(k(z)))
        }
        closure::Term::Var(x) => k(x),
        closure::Term::Let((x, t), e1, e2) => {
            if let Some(atom) = try_atomic(&e1) {
                Term::Let((x.clone(), t.clone()), atom, Box::new(g(*e2, k)))
            } else {
                g(
                    *e1,
                    Box::new(move |y| {
                        Term::Let((x.clone(), t.clone()), Atom::Var(y), Box::new(g(*e2, k)))
                    }),
                )
            }
        }
        closure::Term::MakeCls((x, t), cls, e) => Term::Let(
            (x.clone(), t.clone()),
            Atom::MakeCls(cls),
            Box::new(g(*e, k)),
        ),
        closure::Term::AppCls(f, args) => {
            let x = id::gentmp(&Type::Unit);
            let cont_body = k(x.clone());

            let k_name = id::genid("k_cont");
            let t_x = Type::Int; // Placeholder type

            let mut app_args = args.clone();
            app_args.push(k_name.clone());

            let cont_fundef = Fundef {
                name: (
                    k_name.clone(),
                    Type::Fun(vec![t_x.clone()], Box::new(Type::Unit)),
                ),
                args: vec![(x, t_x)],
                body: Box::new(cont_body),
            };

            Term::LetRec(cont_fundef, Box::new(Term::AppCls(f, app_args)))
        }
        closure::Term::AppDir(f, args) => {
            let x = id::gentmp(&Type::Unit);
            let cont_body = k(x.clone());

            let k_name = id::genid("k_cont");
            let t_x = Type::Int; // Placeholder type

            let mut app_args = args.clone();
            app_args.push(k_name.clone());

            let cont_fundef = Fundef {
                name: (
                    k_name.clone(),
                    Type::Fun(vec![t_x.clone()], Box::new(Type::Unit)),
                ),
                args: vec![(x, t_x)],
                body: Box::new(cont_body),
            };

            Term::LetRec(cont_fundef, Box::new(Term::AppDir(f, app_args)))
        }
        closure::Term::IfEq(x, y, e1, e2) => {
            let res = id::gentmp(&Type::Int); // Placeholder type
            let cont_body = k(res.clone());
            let k_name = id::genid("k_if");

            let k_name1 = k_name.clone();
            let e1_cps = g(*e1, Box::new(move |r| Term::AppCls(k_name1, vec![r])));
            let k_name2 = k_name.clone();
            let e2_cps = g(*e2, Box::new(move |r| Term::AppCls(k_name2, vec![r])));

            let cont_fundef = Fundef {
                name: (
                    k_name.clone(),
                    Type::Fun(vec![Type::Int], Box::new(Type::Unit)),
                ),
                args: vec![(res, Type::Int)],
                body: Box::new(cont_body),
            };

            Term::LetRec(
                cont_fundef,
                Box::new(Term::IfEq(x, y, Box::new(e1_cps), Box::new(e2_cps))),
            )
        }
        closure::Term::IfLE(x, y, e1, e2) => {
            let res = id::gentmp(&Type::Int); // Placeholder type
            let cont_body = k(res.clone());
            let k_name = id::genid("k_if");

            let k_name1 = k_name.clone();
            let e1_cps = g(*e1, Box::new(move |r| Term::AppCls(k_name1, vec![r])));
            let k_name2 = k_name.clone();
            let e2_cps = g(*e2, Box::new(move |r| Term::AppCls(k_name2, vec![r])));

            let cont_fundef = Fundef {
                name: (
                    k_name.clone(),
                    Type::Fun(vec![Type::Int], Box::new(Type::Unit)),
                ),
                args: vec![(res, Type::Int)],
                body: Box::new(cont_body),
            };

            Term::LetRec(
                cont_fundef,
                Box::new(Term::IfLE(x, y, Box::new(e1_cps), Box::new(e2_cps))),
            )
        }
        closure::Term::Tuple(xs) => {
            let y = id::gentmp(&Type::Tuple(vec![]));
            Term::Let(
                (y.clone(), Type::Tuple(vec![])),
                Atom::Tuple(xs),
                Box::new(k(y)),
            )
        }
        closure::Term::LetTuple(xts, y, e2) => {
            Term::LetTuple(xts.clone(), y.clone(), Box::new(g(*e2, k)))
        }
        closure::Term::Get(x, y) => {
            let z = id::gentmp(&Type::Int);
            Term::Let(
                (z.clone(), Type::Int),
                Atom::Get(x.clone(), y.clone()),
                Box::new(k(z)),
            )
        }
        closure::Term::Put(x, y, z) => {
            let w = id::gentmp(&Type::Unit);
            Term::Let(
                (w.clone(), Type::Unit),
                Atom::Put(x.clone(), y.clone(), z.clone()),
                Box::new(k(w)),
            )
        }
        closure::Term::ExtArray(x) => {
            let y = id::gentmp(&Type::Array(Box::new(Type::Int)));
            Term::Let(
                (y.clone(), Type::Array(Box::new(Type::Int))),
                Atom::ExtArray(x.clone()),
                Box::new(k(y)),
            )
        }
    }
}

pub fn f(prog: &closure::Prog) -> Prog {
    let mut cps_fundefs = Vec::new();

    for fundef in &prog.fundefs {
        let k_arg = id::genid("k");
        let k_type = Type::Fun(vec![Type::Unit], Box::new(Type::Unit)); // Placeholder

        let mut new_args = fundef.args.clone();
        new_args.push((k_arg.clone(), k_type));

        let body_cps = g(
            fundef.body.clone(),
            Box::new(move |x| Term::AppCls(k_arg.clone(), vec![x])),
        );

        cps_fundefs.push(Fundef {
            name: fundef.name.clone(),
            args: new_args,
            body: Box::new(body_cps),
        });
    }

    let main_cps = g(
        prog.body.clone(),
        Box::new(|x| Term::AppDir("halt".to_string(), vec![x])),
    );

    Prog {
        fundefs: cps_fundefs,
        body: main_cps,
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Atom::Unit => write!(f, "Unit"),
            Atom::Int(i) => write!(f, "{}", i),
            Atom::Float(d) => write!(f, "{}", d),
            Atom::Var(x) => write!(f, "{}", x),
            Atom::Neg(x) => write!(f, "-{}", x),
            Atom::Add(x, y) => write!(f, "{} + {}", x, y),
            Atom::Sub(x, y) => write!(f, "{} - {}", x, y),
            Atom::FNeg(x) => write!(f, "-.{}", x),
            Atom::FAdd(x, y) => write!(f, "{} +. {}", x, y),
            Atom::FSub(x, y) => write!(f, "{} -. {}", x, y),
            Atom::FMul(x, y) => write!(f, "{} *. {}", x, y),
            Atom::FDiv(x, y) => write!(f, "{} /. {}", x, y),
            Atom::Get(x, y) => write!(f, "{}.({})", x, y),
            Atom::Put(x, y, z) => write!(f, "{}.({}) <- {}", x, y, z),
            Atom::ExtArray(x) => write!(f, "ExtArray({})", x),
            Atom::Tuple(xs) => write!(f, "({:?})", xs),
            Atom::MakeCls(cls) => write!(f, "MakeCls({:?})", cls),
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Let((x, t), atom, e) => write!(f, "let {}: {} = {} in\n{}", x, t, atom, e),
            Term::LetTuple(xts, y, e) => {
                let vars = xts
                    .iter()
                    .map(|(x, _)| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "let ({}) = {} in\n{}", vars, y, e)
            }
            Term::IfEq(x, y, e1, e2) => write!(
                f,
                "if {} = {} then\n{}\nelse\n{}",
                x,
                y,
                indent(e1),
                indent(e2)
            ),
            Term::IfLE(x, y, e1, e2) => write!(
                f,
                "if {} <= {} then\n{}\nelse\n{}",
                x,
                y,
                indent(e1),
                indent(e2)
            ),
            Term::LetRec(fundef, e) => {
                let args = fundef
                    .args
                    .iter()
                    .map(|(x, _)| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ");
                write!(
                    f,
                    "let rec {} {} = \n{}\nin\n{}",
                    fundef.name.0,
                    args,
                    indent(&fundef.body),
                    e
                )
            }
            Term::AppCls(func, args) => {
                let args_str = args
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "AppCls {}({})", func, args_str)
            }
            Term::AppDir(func, args) => {
                let args_str = args
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "AppDir {}({})", func, args_str)
            }
        }
    }
}

fn indent(e: &Term) -> String {
    format!("{}", e)
        .lines()
        .map(|l| format!("  {}", l))
        .collect::<Vec<_>>()
        .join("\n")
}

impl fmt::Display for Prog {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for fundef in &self.fundefs {
            let args = fundef
                .args
                .iter()
                .map(|(x, _)| x.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            write!(
                f,
                "let rec {} {} = \n{}\n\n",
                fundef.name.0,
                args,
                indent(&fundef.body)
            )?;
        }
        write!(f, "{}", self.body)
    }
}

fn try_atomic(e: &closure::Term) -> Option<Atom> {
    match e {
        closure::Term::Unit => Some(Atom::Unit),
        closure::Term::Int(i) => Some(Atom::Int(*i)),
        closure::Term::Float(d) => Some(Atom::Float(*d)),
        closure::Term::Var(x) => Some(Atom::Var(x.clone())),
        closure::Term::Neg(x) => Some(Atom::Neg(x.clone())),
        closure::Term::Add(x, y) => Some(Atom::Add(x.clone(), y.clone())),
        closure::Term::Sub(x, y) => Some(Atom::Sub(x.clone(), y.clone())),
        closure::Term::FNeg(x) => Some(Atom::FNeg(x.clone())),
        closure::Term::FAdd(x, y) => Some(Atom::FAdd(x.clone(), y.clone())),
        closure::Term::FSub(x, y) => Some(Atom::FSub(x.clone(), y.clone())),
        closure::Term::FMul(x, y) => Some(Atom::FMul(x.clone(), y.clone())),
        closure::Term::FDiv(x, y) => Some(Atom::FDiv(x.clone(), y.clone())),
        closure::Term::Get(x, y) => Some(Atom::Get(x.clone(), y.clone())),
        closure::Term::Put(x, y, z) => Some(Atom::Put(x.clone(), y.clone(), z.clone())),
        closure::Term::ExtArray(x) => Some(Atom::ExtArray(x.clone())),
        closure::Term::Tuple(xs) => Some(Atom::Tuple(xs.clone())),
        _ => None,
    }
}
