use crate::closure::{self, Prog as ClosureProg};
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
    LoadLabel(id::L),
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
    // AppCont(label, args, cont_label, cont_args)
    // equivalant to: label args (cont_label cont_args)
    AppCont(id::L, Vec<id::T>, id::L, Vec<id::T>),
    AppClsCont(id::T, Vec<id::T>, id::L, Vec<id::T>),
}

// Helper to create a continuation function definition
// Returns: (Fundef, CapturedVars, ContinuationName)
fn make_continuation_closure(
    k_body_term: Term,
    x: id::T,
    t_x: Type,
    k_name: String,
) -> (Fundef, Vec<id::T>, id::T) {
    // 1. Calculate free variables of the continuation body
    let mut zs = fv(&k_body_term);
    zs.remove(&x); // Remove argument

    let zs_vec: Vec<id::T> = zs.into_iter().collect();

    // 2. Create continuation Fundef
    // Lambda Lifting: Arguments are [captured_vars..., argument]
    let mut args: Vec<(id::T, Type)> = Vec::new();
    for z in &zs_vec {
        args.push((z.clone(), Type::Int)); // Placeholder type for captured vars
    }
    args.push((x, t_x)); // The actual argument of the continuation

    let fundef = Fundef {
        name: (k_name.clone(), Type::Fun(vec![], Box::new(Type::Unit))), // Type is placeholder
        args: args,
        body: Box::new(k_body_term),
    };

    // 3. Return Fundef, Captured Vars, and Name
    (fundef, zs_vec, k_name)
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
        closure::Term::MakeCls((x, t), cls, e) => {
            let entry_var = id::gentmp(&Type::Int);
            let mut tuple_elems = vec![entry_var.clone()];
            tuple_elems.extend(cls.actual_fv.clone());

            Term::Let(
                (entry_var.clone(), Type::Int),
                Atom::LoadLabel(cls.entry),
                Box::new(Term::Let(
                    (x.clone(), t.clone()),
                    Atom::Tuple(tuple_elems),
                    Box::new(g(*e, k)),
                )),
            )
        }
        closure::Term::AppCls(f, args) => {
            let x = id::gentmp(&Type::Unit);
            let cont_body = k(x.clone());
            let k_name = id::genid("k_cont");

            let (cont_fundef, k_env, k_name) =
                make_continuation_closure(cont_body, x, Type::Int, k_name);

            Term::LetRec(
                cont_fundef,
                Box::new(Term::AppClsCont(f, args, k_name, k_env)),
            )
        }
        closure::Term::AppDir(f, args) => {
            let x = id::gentmp(&Type::Unit);
            let cont_body = k(x.clone());
            let k_name = id::genid("k_cont");

            let (cont_fundef, k_env, k_name) =
                make_continuation_closure(cont_body, x, Type::Int, k_name);

            Term::LetRec(cont_fundef, Box::new(Term::AppCont(f, args, k_name, k_env)))
        }
        closure::Term::IfEq(x, y, e1, e2) => {
            let res = id::gentmp(&Type::Int);
            let cont_body = k(res.clone());
            let k_name = id::genid("k_if");

            let (cont_fundef, k_env, k_name) =
                make_continuation_closure(cont_body, res, Type::Int, k_name);

            let k_name1 = k_name.clone();
            let k_env1 = k_env.clone();
            let e1_cps = g(
                *e1,
                Box::new(move |r| {
                    let mut args = k_env1.clone();
                    args.push(r);
                    Term::AppDir(k_name1.clone(), args)
                }),
            );

            let k_name2 = k_name.clone();
            let k_env2 = k_env.clone();
            let e2_cps = g(
                *e2,
                Box::new(move |r| {
                    let mut args = k_env2.clone();
                    args.push(r);
                    Term::AppDir(k_name2.clone(), args)
                }),
            );

            Term::LetRec(
                cont_fundef,
                Box::new(Term::IfEq(x, y, Box::new(e1_cps), Box::new(e2_cps))),
            )
        }
        closure::Term::IfLE(x, y, e1, e2) => {
            let res = id::gentmp(&Type::Int);
            let cont_body = k(res.clone());
            let k_name = id::genid("k_if");

            let (cont_fundef, k_env, k_name) =
                make_continuation_closure(cont_body, res, Type::Int, k_name);

            let k_name1 = k_name.clone();
            let k_env1 = k_env.clone();
            let e1_cps = g(
                *e1,
                Box::new(move |r| {
                    let mut args = k_env1.clone();
                    args.push(r);
                    Term::AppDir(k_name1.clone(), args)
                }),
            );

            let k_name2 = k_name.clone();
            let k_env2 = k_env.clone();
            let e2_cps = g(
                *e2,
                Box::new(move |r| {
                    let mut args = k_env2.clone();
                    args.push(r);
                    Term::AppDir(k_name2.clone(), args)
                }),
            );

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

pub fn f(prog: &ClosureProg) -> Prog {
    let mut cps_fundefs = Vec::new();

    for fundef in &prog.fundefs {
        let k_arg = id::genid("k");
        let k_type = Type::Fun(vec![Type::Unit], Box::new(Type::Unit)); // Placeholder

        let mut new_args = Vec::new();
        // Lambda Lifting: Prepend formal free variables to arguments.
        // fundef.formal_fv contains variables captured by this function.
        // We promote them to explicit arguments.
        for (fv_name, fv_type) in &fundef.formal_fv {
            new_args.push((fv_name.clone(), fv_type.clone()));
        }
        // Then original arguments
        new_args.extend(fundef.args.clone());
        // Then continuation
        new_args.push((k_arg.clone(), k_type));

        let body_cps = g(
            fundef.body.clone(),
            // When k is called in body, it's AppCls(k_arg, [x]) -> k_arg is Tuple.
            Box::new(move |x| Term::AppCls(k_arg.clone(), vec![x])),
        );

        cps_fundefs.push(Fundef {
            name: (fundef.name.0.clone(), fundef.name.1.clone()),
            args: new_args,
            body: Box::new(body_cps),
        });
    }

    // 2. Create `min_caml_start` function
    // fun min_caml_start() = body... -> halt
    let start_name = "min_caml_start".to_string();

    // Original CPS generation: g(body, |x| AppDir("halt", [x]))
    // This bakes "halt" into the tail of the body.
    let main_cps_body = g(
        prog.body.clone(),
        Box::new(|x| Term::AppDir("halt".to_string(), vec![x])),
    );

    // min_caml_start takes NO arguments (no continuation passed, no result passed).
    // It is a self-contained routine.
    let start_fundef = Fundef {
        name: (start_name.clone(), Type::Fun(vec![], Box::new(Type::Unit))),
        args: vec![],
        body: Box::new(main_cps_body),
    };
    cps_fundefs.push(start_fundef);

    // 3. Entry Point: Call min_caml_start()
    let entry_term = Term::AppDir(start_name.clone(), vec![]);

    Prog {
        fundefs: cps_fundefs,
        body: entry_term,
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
            Atom::LoadLabel(l) => write!(f, "LoadLabel({})", l),
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
            Term::AppCont(func, args, k, k_args) => {
                write!(
                    f,
                    "AppCont {}({:?}, cont {}, env {:?})",
                    func, args, k, k_args
                )
            }
            Term::AppClsCont(func, args, k, k_args) => {
                write!(
                    f,
                    "AppClsCont {}({:?}, cont {}, env {:?})",
                    func, args, k, k_args
                )
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
use std::collections::HashSet;

pub fn fv(term: &Term) -> HashSet<id::T> {
    match term {
        Term::Let((x, _), atom, e) => {
            let mut s = fv(e);
            s.remove(x);
            s.extend(fv_atom(atom));
            s
        }
        Term::LetTuple(xts, y, e) => {
            let mut s = fv(e);
            for (x, _) in xts {
                s.remove(x);
            }
            s.insert(y.clone());
            s
        }
        Term::IfEq(x, y, e1, e2) | Term::IfLE(x, y, e1, e2) => {
            let mut s = fv(e1);
            s.extend(fv(e2));
            s.insert(x.clone());
            s.insert(y.clone());
            s
        }
        Term::LetRec(fundef, e) => {
            let mut s = fv(e);
            let mut body_fv = fv(&fundef.body);
            for (arg, _) in &fundef.args {
                body_fv.remove(arg);
            }
            s.extend(body_fv);
            s.remove(&fundef.name.0);
            s
        }
        Term::AppCls(x, args) => {
            let mut s = HashSet::new();
            s.insert(x.clone());
            for arg in args {
                s.insert(arg.clone());
            }
            s
        }
        Term::AppDir(_, args) => {
            let mut s = HashSet::new();
            for arg in args {
                s.insert(arg.clone());
            }
            s
        }
        Term::AppCont(_, args, _, k_args) | Term::AppClsCont(_, args, _, k_args) => {
            let mut s = HashSet::new();
            // AppCont/AppClsCont(f, args, k_label, k_args)
            // f is Label/Var. If Var (AppClsCont), need to insert.
            if let Term::AppClsCont(f, _, _, _) = term {
                s.insert(f.clone());
            }
            for arg in args {
                s.insert(arg.clone());
            }
            // k_label is Label (Const).
            for arg in k_args {
                s.insert(arg.clone());
            }
            s
        }
    }
}

fn fv_atom(atom: &Atom) -> HashSet<id::T> {
    let mut s = HashSet::new();
    match atom {
        Atom::Unit | Atom::Int(_) | Atom::Float(_) | Atom::ExtArray(_) | Atom::LoadLabel(_) => {}
        Atom::Var(x) | Atom::Neg(x) | Atom::FNeg(x) => {
            s.insert(x.clone());
        }
        Atom::Add(x, y)
        | Atom::Sub(x, y)
        | Atom::FAdd(x, y)
        | Atom::FSub(x, y)
        | Atom::FMul(x, y)
        | Atom::FDiv(x, y)
        | Atom::Get(x, y) => {
            s.insert(x.clone());
            s.insert(y.clone());
        }
        Atom::Put(x, y, z) => {
            s.insert(x.clone());
            s.insert(y.clone());
            s.insert(z.clone());
        }
        Atom::Tuple(xs) => {
            for x in xs {
                s.insert(x.clone());
            }
        }
    }
    s
}
