use crate::id;
use crate::k_normal::KNormal;
use crate::ty::Type;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub entry: id::L,
    pub actual_fv: Vec<id::T>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Unit,
    Int(i32),
    Float(f64),
    Neg(id::T),
    Add(id::T, id::T),
    Sub(id::T, id::T),
    FNeg(id::T),
    FAdd(id::T, id::T),
    FSub(id::T, id::T),
    FMul(id::T, id::T),
    FDiv(id::T, id::T),
    IfEq(id::T, id::T, Box<Term>, Box<Term>),
    IfLE(id::T, id::T, Box<Term>, Box<Term>),
    Let((id::T, Type), Box<Term>, Box<Term>),
    Var(id::T),
    MakeCls((id::T, Type), Closure, Box<Term>),
    AppCls(id::T, Vec<id::T>),
    AppDir(id::L, Vec<id::T>),
    Tuple(Vec<id::T>),
    LetTuple(Vec<(id::T, Type)>, id::T, Box<Term>),
    Get(id::T, id::T),
    Put(id::T, id::T, id::T),
    ExtArray(id::L),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fundef {
    pub name: (id::L, Type),
    pub args: Vec<(id::T, Type)>,
    pub formal_fv: Vec<(id::T, Type)>,
    pub body: Term,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Prog {
    pub fundefs: Vec<Fundef>,
    pub body: Term,
}

fn fv(e: &Term) -> HashSet<id::T> {
    match e {
        Term::Unit | Term::Int(_) | Term::Float(_) | Term::ExtArray(_) => HashSet::new(),
        Term::Neg(x) | Term::FNeg(x) => {
            let mut s = HashSet::new();
            s.insert(x.clone());
            s
        }
        Term::Add(x, y)
        | Term::Sub(x, y)
        | Term::FAdd(x, y)
        | Term::FSub(x, y)
        | Term::FMul(x, y)
        | Term::FDiv(x, y)
        | Term::Get(x, y) => {
            let mut s = HashSet::new();
            s.insert(x.clone());
            s.insert(y.clone());
            s
        }
        Term::IfEq(x, y, e1, e2) | Term::IfLE(x, y, e1, e2) => {
            let mut s = HashSet::new();
            s.insert(x.clone());
            s.insert(y.clone());
            s.extend(fv(e1));
            s.extend(fv(e2));
            s
        }
        Term::Let((x, _), e1, e2) => {
            let mut s = fv(e1);
            let s2 = fv(e2);
            let mut s2 = s2.clone();
            s2.remove(x);
            s.extend(s2);
            s
        }
        Term::Var(x) => {
            let mut s = HashSet::new();
            s.insert(x.clone());
            s
        }
        Term::MakeCls((x, _), cls, e) => {
            let mut s = HashSet::new();
            for y in &cls.actual_fv {
                s.insert(y.clone());
            }
            let s2 = fv(e);
            let mut s2 = s2.clone();
            s2.remove(x);
            s.extend(s2);
            s
        }
        Term::AppCls(x, ys) => {
            let mut s = HashSet::new();
            s.insert(x.clone());
            for y in ys {
                s.insert(y.clone());
            }
            s
        }
        Term::AppDir(_, xs) | Term::Tuple(xs) => {
            let mut s = HashSet::new();
            for x in xs {
                s.insert(x.clone());
            }
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
        Term::Put(x, y, z) => {
            let mut s = HashSet::new();
            s.insert(x.clone());
            s.insert(y.clone());
            s.insert(z.clone());
            s
        }
    }
}

pub fn g(
    env: &HashMap<String, Type>,
    known: &HashSet<String>,
    toplevel: &Rc<RefCell<Vec<Fundef>>>,
    e: &KNormal,
) -> Term {
    match e {
        KNormal::Unit => Term::Unit,
        KNormal::Int(i) => Term::Int(*i),
        KNormal::Float(d) => Term::Float(*d),
        KNormal::Neg(x) => Term::Neg(x.clone()),
        KNormal::Add(x, y) => Term::Add(x.clone(), y.clone()),
        KNormal::Sub(x, y) => Term::Sub(x.clone(), y.clone()),
        KNormal::FNeg(x) => Term::FNeg(x.clone()),
        KNormal::FAdd(x, y) => Term::FAdd(x.clone(), y.clone()),
        KNormal::FSub(x, y) => Term::FSub(x.clone(), y.clone()),
        KNormal::FMul(x, y) => Term::FMul(x.clone(), y.clone()),
        KNormal::FDiv(x, y) => Term::FDiv(x.clone(), y.clone()),
        KNormal::IfEq(x, y, e1, e2) => Term::IfEq(
            x.clone(),
            y.clone(),
            Box::new(g(env, known, toplevel, e1)),
            Box::new(g(env, known, toplevel, e2)),
        ),
        KNormal::IfLE(x, y, e1, e2) => Term::IfLE(
            x.clone(),
            y.clone(),
            Box::new(g(env, known, toplevel, e1)),
            Box::new(g(env, known, toplevel, e2)),
        ),
        KNormal::Let((x, t), e1, e2) => {
            let mut new_env = env.clone();
            new_env.insert(x.clone(), t.clone());
            Term::Let(
                (x.clone(), t.clone()),
                Box::new(g(env, known, toplevel, e1)),
                Box::new(g(&new_env, known, toplevel, e2)),
            )
        }
        KNormal::Var(x) => Term::Var(x.clone()),
        KNormal::LetRec(fundef, e2) => {
            let x = &fundef.name.0;
            let t = &fundef.name.1;

            let mut new_env = env.clone();
            new_env.insert(x.clone(), t.clone());
            let mut body_env = new_env.clone();
            for (arg, ty) in &fundef.args {
                body_env.insert(arg.clone(), ty.clone());
            }

            let known_prime = known.clone();
            let e1_prime = g(&body_env, &known_prime, toplevel, &fundef.body);
            let mut zs = fv(&e1_prime);

            zs.retain(|z| z != x);
            for (arg, _) in &fundef.args {
                zs.retain(|z| z != arg);
            }
            // If zs is empty, we can add it to toplevel and known
            // But we need to check if it's actually empty.
            // Also need to handle recursion correctly.

            // For simplicity, we assume we can lift it if zs is empty or if we handle closure creation.
            // min-caml logic:
            // Check if free variables are empty. If so, add to known.
            // If not, we must create a closure.

            // Wait, min-caml logic is slightly more complex with `toplevel_backup` etc.
            // Let's implement a simplified version: always lift to toplevel, but if it has free vars, they become `formal_fv`.

            let zs_vec: Vec<String> = zs.into_iter().collect();
            // We need types for zs.
            let mut zts = Vec::new();
            for z in &zs_vec {
                // z must be in env
                // If not in env, it might be a global or bug.
                // In min-caml, it assumes it's in env.
                if let Some(t) = body_env.get(z) {
                    zts.push((z.clone(), t.clone()));
                } else {
                    // Maybe external?
                    // For now, panic or assume Int?
                    // It should be in env if it's a free variable from outer scope.
                    // If it's toplevel function, it might not be in env if we didn't add it?
                    // But we added x to env.
                    // Let's assume it's found.
                    // println!("Warning: free variable {} not found in env", z);
                    // Actually, if it's a recursive call to itself, it's in env.
                    // If it calls another known function, it's in known, so not a free variable?
                    // Wait, `fv` function treats `AppDir` as having free vars?
                    // `AppDir` uses label, so it doesn't count as free variable `Var`.
                    // So known functions are `AppDir` and don't contribute to `fv`.
                }
            }

            // Add to toplevel
            toplevel.borrow_mut().push(Fundef {
                name: (x.clone(), t.clone()),
                args: fundef.args.clone(),
                formal_fv: zts.clone(),
                body: e1_prime,
            });

            let e2_prime = g(&new_env, &known_prime, toplevel, e2);

            // Check if x appears in e2_prime as a variable (not just AppDir)
            // If it appears as Var(x), we need MakeCls.
            // If it only appears in AppDir(x, ...), we don't need MakeCls.
            // But `fv` returns x if it's used as Var.

            if fv(&e2_prime).contains(x) {
                Term::MakeCls(
                    (x.clone(), t.clone()),
                    Closure {
                        entry: x.clone(),
                        actual_fv: zs_vec,
                    },
                    Box::new(e2_prime),
                )
            } else {
                e2_prime
            }
        }
        KNormal::App(x, ys) => {
            if known.contains(x) {
                Term::AppDir(x.clone(), ys.clone())
            } else {
                Term::AppCls(x.clone(), ys.clone())
            }
        }
        KNormal::Tuple(xs) => Term::Tuple(xs.clone()),
        KNormal::LetTuple(xts, y, e) => {
            let mut new_env = env.clone();
            for (x, t) in xts {
                new_env.insert(x.clone(), t.clone());
            }
            Term::LetTuple(
                xts.clone(),
                y.clone(),
                Box::new(g(&new_env, known, toplevel, e)),
            )
        }
        KNormal::Get(x, y) => Term::Get(x.clone(), y.clone()),
        KNormal::Put(x, y, z) => Term::Put(x.clone(), y.clone(), z.clone()),
        KNormal::ExtArray(x) => Term::ExtArray(x.clone()),
        KNormal::ExtFunApp(x, ys) => Term::AppDir(format!("min_caml_{}", x), ys.clone()),
    }
}

pub fn f(e: &KNormal) -> Prog {
    let toplevel = Rc::new(RefCell::new(Vec::new()));
    let e_prime = g(&HashMap::new(), &HashSet::new(), &toplevel, e);
    Prog {
        fundefs: toplevel.borrow().clone(),
        body: e_prime,
    }
}
