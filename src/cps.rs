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

// Helper to create a closure-optimized continuation
fn make_continuation_closure(
    k_body_term: Term,
    x: id::T,
    t_x: Type,
    k_name: String,
) -> (Fundef, Atom, id::T) {
    // 1. Calculate free variables of the continuation body
    let mut zs = fv(&k_body_term);
    zs.remove(&x); // Remove argument

    // 2. Create MakeCls
    let zs_vec: Vec<id::T> = zs.into_iter().collect();
    let closure = Closure {
        entry: k_name.clone(),
        actual_fv: zs_vec.clone(),
    };

    // 3. Create continuation Fundef
    // k_cont(arg, self_env)
    let self_env = id::genid("self_env");
    let mut body = k_body_term;

    // Inject free variable loading: Let z = Get(self_env, i+1) (0 is code ptr)
    // Note: In blocked.rs, MakeCls creates [code, fv1, fv2...] on stack.
    // k points to this block.
    // k[0] is code. k[1] is fv1.
    // Inject free variable loading: Let z = Get(self_env, i+1) (0 is code ptr)
    // Note: In blocked.rs, MakeCls creates [code, fv1, fv2...] on stack.
    // k points to this block.
    // k[0] is code. k[1] is fv1.
    // We iterate zs_vec (actual_fv). The order in zs_vec must match pushing order.
    // MakeCls uses zs_vec. So order matches.
    // We want to generate:
    // Let z1 = Get(self, 1) in Let z2 = Get(self, 2) in Body.
    // So z1 must be Outermost.
    // Loop must iterate First -> Outermost? No.
    // `body = Let(new, ..., body)`.
    // If we want `Let z1 ... Body`. `body` is processed last.
    // So z1 is processed last.
    // Iterate zs_vec in REVERSE.
    for (i, z) in zs_vec.iter().enumerate().rev() {
        let idx_var = id::gentmp(&Type::Int);
        let idx_val = (i + 1) as i32;

        let get_op = Atom::Get(self_env.clone(), idx_var.clone());
        let let_z = Term::Let((z.clone(), Type::Int), get_op, Box::new(body));

        body = Term::Let((idx_var, Type::Int), Atom::Int(idx_val), Box::new(let_z));
    }

    let fun_type = Type::Fun(vec![t_x.clone()], Box::new(Type::Unit)); // Simplified type

    let fundef = Fundef {
        name: (k_name, fun_type),
        args: vec![(x, t_x), (self_env, Type::Int)], // args + self_env
        body: Box::new(body),
    };

    // 4. Return Fundef, MakeCls Atom, and closure variable name (to be let-bound by caller)
    let k_closure_var = id::genid("k_cls");
    (fundef, Atom::MakeCls(closure), k_closure_var)
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

            let (cont_fundef, make_cls_atom, k_cls_var) =
                make_continuation_closure(cont_body, x, Type::Int, k_name);

            let mut app_args = args.clone();
            app_args.push(k_cls_var.clone()); // Pass closure as argument

            let app_term = Term::Let(
                (k_cls_var, Type::Fun(vec![Type::Int], Box::new(Type::Unit))), // Placeholder type
                make_cls_atom,
                Box::new(Term::AppCls(f, app_args)),
            );

            Term::LetRec(cont_fundef, Box::new(app_term))
        }
        closure::Term::AppDir(f, args) => {
            let x = id::gentmp(&Type::Unit);
            let cont_body = k(x.clone());
            let k_name = id::genid("k_cont");

            let (cont_fundef, make_cls_atom, k_cls_var) =
                make_continuation_closure(cont_body, x, Type::Int, k_name);

            let mut app_args = args.clone();
            app_args.push(k_cls_var.clone()); // Pass closure as argument

            let app_term = Term::Let(
                (k_cls_var, Type::Fun(vec![Type::Int], Box::new(Type::Unit))),
                make_cls_atom,
                Box::new(Term::AppDir(f, app_args)),
            );

            Term::LetRec(cont_fundef, Box::new(app_term))
        }
        closure::Term::IfEq(x, y, e1, e2) => {
            let res = id::gentmp(&Type::Int);
            let cont_body = k(res.clone());
            let k_name = id::genid("k_if");

            // Make shared continuation closure
            let (cont_fundef, make_cls_atom, k_cls_var) =
                make_continuation_closure(cont_body, res, Type::Int, k_name);

            // We need to pass k_cls_var to e1 and e2.
            // But g() constructor takes a generic k.
            // And e1/e2 g() calls need to call k_cls_var... as AppCls?
            // Yes. k is now a closure.
            // So continuation of e1 is: AppCls(k_cls_var, [result])

            let k_cls_var1 = k_cls_var.clone();
            let e1_cps = g(*e1, Box::new(move |r| Term::AppCls(k_cls_var1, vec![r])));

            let k_cls_var2 = k_cls_var.clone();
            let e2_cps = g(*e2, Box::new(move |r| Term::AppCls(k_cls_var2, vec![r])));

            let if_term = Term::Let(
                (k_cls_var, Type::Int),
                make_cls_atom,
                Box::new(Term::IfEq(x, y, Box::new(e1_cps), Box::new(e2_cps))),
            );

            Term::LetRec(cont_fundef, Box::new(if_term))
        }
        closure::Term::IfLE(x, y, e1, e2) => {
            let res = id::gentmp(&Type::Int);
            let cont_body = k(res.clone());
            let k_name = id::genid("k_if");

            let (cont_fundef, make_cls_atom, k_cls_var) =
                make_continuation_closure(cont_body, res, Type::Int, k_name);

            let k_cls_var1 = k_cls_var.clone();
            let e1_cps = g(*e1, Box::new(move |r| Term::AppCls(k_cls_var1, vec![r])));

            let k_cls_var2 = k_cls_var.clone();
            let e2_cps = g(*e2, Box::new(move |r| Term::AppCls(k_cls_var2, vec![r])));

            let if_term = Term::Let(
                (k_cls_var, Type::Int),
                make_cls_atom,
                Box::new(Term::IfLE(x, y, Box::new(e1_cps), Box::new(e2_cps))),
            );

            Term::LetRec(cont_fundef, Box::new(if_term))
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
        // Add self_env argument to function definitions as well?
        // Wait, function definitions also need to follow the convention `f(arg, self_env, k)`?
        // Original MinCaml arguments are in `fundef.args`.
        // The closure conversion plan says "All continuations must expect [arg, self_env]".
        // Does this apply to normal functions?
        // Normal functions are called via AppCls.
        // AppCls(f, args) -> TailCallDynamic(f[0], args + [f])
        // So `f` receives: `arg1, arg2, ..., k_cls, self_env`.
        // Wait, where is `k` in argument list?
        // `closure.rs` treats `k` as the last argument in CPS.
        // `g` appends `k_name`.
        // `AppCls(f, args)` in `cps.rs` appends `k_cls_var`.
        // So arguments are `[original_args..., k_cls]`.
        // AND calling convention says we define `f(original_args..., k_cls, self_env)`.
        // So we need to append a hidden `self` argument to every function definition.

        let self_env = id::genid("self_env");
        new_args.push((self_env, Type::Int));

        let body_cps = g(
            fundef.body.clone(),
            // When k is called in body, it's AppCls(k_arg, [x])
            // And now AppCls implies passing environment.
            // k_arg IS the closure.
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
    }
}

fn fv_atom(atom: &Atom) -> HashSet<id::T> {
    let mut s = HashSet::new();
    match atom {
        Atom::Unit | Atom::Int(_) | Atom::Float(_) | Atom::ExtArray(_) => {}
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
        Atom::MakeCls(cls) => {
            s.extend(cls.actual_fv.clone());
        }
    }
    s
}
