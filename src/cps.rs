use crate::closure::{self, Prog as ClosureProg};
use crate::id;
use crate::ty::Type;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

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
    CallDir(id::L, Vec<id::T>),
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
    // LetRec removed as we lift to toplevel
    App(id::T, Vec<id::T>), // Tail call / Jump (e.g. k x, halt x)
    AppCont(id::T, Vec<id::T>, id::L, Vec<id::T>), // Function call with continuation
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
pub fn g(
    e: closure::Term,
    k: Box<dyn FnOnce(id::T) -> Term>,
    toplevel_fundefs: &Rc<RefCell<Vec<Fundef>>>,
    type_env: &mut HashMap<id::T, Type>,
    label_fvs: &HashMap<id::L, Vec<id::T>>,
) -> Term {
    match e {
        closure::Term::Unit => {
            let x = id::gentmp(&Type::Unit);
            type_env.insert(x.clone(), Type::Unit);
            Term::Let((x.clone(), Type::Unit), Atom::Unit, Box::new(k(x)))
        }
        closure::Term::Int(i) => {
            let x = id::gentmp(&Type::Int);
            type_env.insert(x.clone(), Type::Int);
            Term::Let((x.clone(), Type::Int), Atom::Int(i), Box::new(k(x)))
        }
        closure::Term::Float(d) => {
            let x = id::gentmp(&Type::Float);
            type_env.insert(x.clone(), Type::Float);
            Term::Let((x.clone(), Type::Float), Atom::Float(d), Box::new(k(x)))
        }
        closure::Term::Neg(x) => {
            let y = id::gentmp(&Type::Int);
            type_env.insert(y.clone(), Type::Int);
            Term::Let((y.clone(), Type::Int), Atom::Neg(x), Box::new(k(y)))
        }
        closure::Term::Add(x, y) => {
            let z = id::gentmp(&Type::Int);
            type_env.insert(z.clone(), Type::Int);
            Term::Let((z.clone(), Type::Int), Atom::Add(x, y), Box::new(k(z)))
        }
        closure::Term::Sub(x, y) => {
            let z = id::gentmp(&Type::Int);
            type_env.insert(z.clone(), Type::Int);
            Term::Let((z.clone(), Type::Int), Atom::Sub(x, y), Box::new(k(z)))
        }
        closure::Term::FNeg(x) => {
            let y = id::gentmp(&Type::Float);
            type_env.insert(y.clone(), Type::Float);
            Term::Let((y.clone(), Type::Float), Atom::FNeg(x), Box::new(k(y)))
        }
        closure::Term::FAdd(x, y) => {
            let z = id::gentmp(&Type::Float);
            type_env.insert(z.clone(), Type::Float);
            Term::Let((z.clone(), Type::Float), Atom::FAdd(x, y), Box::new(k(z)))
        }
        closure::Term::FSub(x, y) => {
            let z = id::gentmp(&Type::Float);
            type_env.insert(z.clone(), Type::Float);
            Term::Let((z.clone(), Type::Float), Atom::FSub(x, y), Box::new(k(z)))
        }
        closure::Term::FMul(x, y) => {
            let z = id::gentmp(&Type::Float);
            type_env.insert(z.clone(), Type::Float);
            Term::Let((z.clone(), Type::Float), Atom::FMul(x, y), Box::new(k(z)))
        }
        closure::Term::FDiv(x, y) => {
            let z = id::gentmp(&Type::Float);
            type_env.insert(z.clone(), Type::Float);
            Term::Let((z.clone(), Type::Float), Atom::FDiv(x, y), Box::new(k(z)))
        }
        closure::Term::Var(x) => k(x),
        closure::Term::Let((x, t), e1, e2) => {
            type_env.insert(x.clone(), t.clone());
            if let Some(atom) = try_atomic(&e1) {
                Term::Let(
                    (x.clone(), t.clone()),
                    atom,
                    Box::new(g(*e2, k, toplevel_fundefs, type_env, label_fvs)),
                )
            } else {
                let toplevel_fundefs1 = toplevel_fundefs.clone();
                let label_fvs_clone = label_fvs.clone();
                let type_env_for_e2_closure = type_env.clone(); // Capture current type_env for the closure
                g(
                    *e1,
                    Box::new(move |y| {
                        let mut type_env_e2 = type_env_for_e2_closure.clone();
                        type_env_e2.insert(x.clone(), t.clone());
                        Term::Let(
                            (x.clone(), t.clone()),
                            Atom::Var(y),
                            Box::new(g(
                                *e2,
                                k,
                                &toplevel_fundefs1,
                                &mut type_env_e2,
                                &label_fvs_clone,
                            )),
                        )
                    }),
                    toplevel_fundefs,
                    type_env,
                    label_fvs,
                )
            }
        }
        closure::Term::MakeCls((x, t), cls, e) => {
            type_env.insert(x.clone(), t.clone());

            let entry_var = id::gentmp(&Type::Int);
            type_env.insert(entry_var.clone(), Type::Int);
            let mut tuple_elems = vec![entry_var.clone()];
            tuple_elems.extend(cls.actual_fv.clone());

            Term::Let(
                (entry_var.clone(), Type::Int),
                Atom::LoadLabel(cls.entry),
                Box::new(Term::Let(
                    (x.clone(), t.clone()),
                    Atom::Tuple(tuple_elems),
                    Box::new(g(*e, k, toplevel_fundefs, type_env, label_fvs)),
                )),
            )
        }
        closure::Term::AppCls(f, args) => {
            // Unpack Closure: f is a Tuple (CodePtr, Env...).

            let tuple_info = if let Some(Type::Tuple(ts)) = type_env.get(&f) {
                Some(ts[1..].to_vec())
            } else {
                None
            };

            if let Some(fv_types) = tuple_info {
                // Standard Closure Call

                // 2. Extract CodePtr
                let code_ptr_var = id::gentmp(&Type::Int);
                type_env.insert(code_ptr_var.clone(), Type::Int);

                // 3. Extract FVs
                let mut fv_vars = Vec::new();
                for t in &fv_types {
                    let fv_temp = id::gentmp(t);
                    type_env.insert(fv_temp.clone(), t.clone());
                    fv_vars.push((fv_temp, t.clone()));
                }

                // 4. Construct Cont
                let x = id::gentmp(&Type::Unit);
                type_env.insert(x.clone(), Type::Unit);
                let cont_body = k(x.clone());
                let k_name = id::genid("k_cont");
                let (cont_fundef, k_env, k_name) =
                    make_continuation_closure(cont_body, x, Type::Int, k_name);
                toplevel_fundefs.borrow_mut().push(cont_fundef);

                // 5. Build Let sequences to unpack
                let mut term = Term::AppCont(
                    code_ptr_var.clone(),
                    {
                        let mut all_args = Vec::new();
                        // Arguments: FVs + OriginalArgs
                        for (v, _) in &fv_vars {
                            all_args.push(v.clone());
                        }
                        all_args.extend(args);
                        all_args
                    },
                    k_name,
                    k_env,
                );

                // Wrap in Lets (Reverse order)
                // let fv_n = f.n
                for (i, (fv, t)) in fv_vars.iter().enumerate().rev() {
                    term = Term::Let(
                        (fv.clone(), t.clone()),
                        Atom::Get(f.clone(), format!("{}", i + 1)), // Access index i+1
                        Box::new(term),
                    );
                }
                // let code = f.0
                term = Term::Let(
                    (code_ptr_var.clone(), Type::Int),
                    Atom::Get(f.clone(), "0".to_string()),
                    Box::new(term),
                );

                term
            } else if let Some(fvs) = label_fvs.get(&f) {
                // Fallback to Direct Call logic (AppDir style)
                let l = f.clone();
                // 1. Load Label
                let f_var = id::gentmp(&Type::Int);
                type_env.insert(f_var.clone(), Type::Int);

                // 2. Construct Cont
                let x = id::gentmp(&Type::Unit);
                type_env.insert(x.clone(), Type::Unit);
                let cont_body = k(x.clone());
                let k_name = id::genid("k_cont");
                let (cont_fundef, k_env, k_name) =
                    make_continuation_closure(cont_body, x, Type::Int, k_name);
                toplevel_fundefs.borrow_mut().push(cont_fundef);

                let mut new_args = fvs.clone();
                new_args.extend(args);

                Term::Let(
                    (f_var.clone(), Type::Int),
                    Atom::LoadLabel(l),
                    Box::new(Term::AppCont(f_var, new_args, k_name, k_env)),
                )
            } else {
                let f_type = type_env.get(&f);
                panic!(
                    "AppCls: Function variable {} not found in type_env (or not Tuple) and not in label_fvs. Type: {:?}",
                    f, f_type
                );
            }
        }
        closure::Term::AppDir(l, args) => {
            // Check for Externals
            let is_external = l.starts_with("min_caml_")
                || l == "print_int"
                || l == "print_newline"
                || l == "truncate"
                || l == "sin"
                || l == "cos"
                || l == "sqrt"
                || l == "abs_float"
                || l == "int_of_float"
                || l == "float_of_int"
                || l == "floor";

            if is_external {
                // External Call: Direct Execution
                // let res = CallDir(l, args) in k(res)
                let res = id::gentmp(&Type::Int); // Assuming Int/Float result. Type inference?
                // We don't track return type here. Assuming Int for now or Unit?
                // k expects a certain type.
                // We should just generate Let((res, Type::Int), CallDir(..), k(res)).
                // But the type of `res` depends on the external.
                // For print_int, it returns Unit.
                // For sin/cos, Float.
                // We should lookup type?
                // `g` receives `type_env`, but that's for variables.
                // We can approximate type based on function name or context?
                // Or just use Type::Int (it's typed as T anyway).
                // Correctness depends mainly on backend handling.
                type_env.insert(res.clone(), Type::Int);

                Term::Let(
                    (res.clone(), Type::Int),
                    Atom::CallDir(l, args),
                    Box::new(k(res)),
                )
            } else {
                // Direct Call to Label `l`.
                // We need to pass its FVs.
                let fvs = label_fvs
                    .get(&l)
                    .unwrap_or_else(|| panic!("AppDir: Label {} not found in label_fvs", l))
                    .clone();

                // 1. Load Label
                let f_var = id::gentmp(&Type::Int);
                type_env.insert(f_var.clone(), Type::Int);

                // 2. Construct Cont
                let x = id::gentmp(&Type::Unit);
                type_env.insert(x.clone(), Type::Unit);
                let cont_body = k(x.clone());
                let k_name = id::genid("k_cont");
                let (cont_fundef, k_env, k_name) =
                    make_continuation_closure(cont_body, x, Type::Int, k_name);
                toplevel_fundefs.borrow_mut().push(cont_fundef);

                let mut new_args = fvs;
                new_args.extend(args);

                Term::Let(
                    (f_var.clone(), Type::Int),
                    Atom::LoadLabel(l),
                    Box::new(Term::AppCont(f_var, new_args, k_name, k_env)),
                )
            }
        }
        closure::Term::IfEq(x, y, e1, e2) => {
            let res = id::gentmp(&Type::Int);
            type_env.insert(res.clone(), Type::Int);
            let cont_body = k(res.clone());
            let k_name = id::genid("k_if");
            let (cont_fundef, k_env, k_name) =
                make_continuation_closure(cont_body, res, Type::Int, k_name);
            toplevel_fundefs.borrow_mut().push(cont_fundef);

            let k_name1 = k_name.clone();
            let k_env1 = k_env.clone();
            let toplevel_fundefs1 = toplevel_fundefs.clone();
            let label_fvs1 = label_fvs.clone();
            let mut type_env1 = type_env.clone();
            let e1_cps = g(
                *e1,
                Box::new(move |r| {
                    let mut args = k_env1.clone();
                    args.push(r);
                    Term::App(k_name1.clone(), args)
                }),
                toplevel_fundefs,
                &mut type_env1,
                &label_fvs1,
            );

            let k_name2 = k_name.clone();
            let k_env2 = k_env.clone();
            let label_fvs2 = label_fvs.clone();
            let mut type_env2 = type_env.clone();
            let e2_cps = g(
                *e2,
                Box::new(move |r| {
                    let mut args = k_env2.clone();
                    args.push(r);
                    Term::App(k_name2.clone(), args)
                }),
                &toplevel_fundefs1,
                &mut type_env2,
                &label_fvs2,
            );

            Term::IfEq(x, y, Box::new(e1_cps), Box::new(e2_cps))
        }
        closure::Term::IfLE(x, y, e1, e2) => {
            let res = id::gentmp(&Type::Int);
            type_env.insert(res.clone(), Type::Int);
            let cont_body = k(res.clone());
            let k_name = id::genid("k_if");
            let (cont_fundef, k_env, k_name) =
                make_continuation_closure(cont_body, res, Type::Int, k_name);
            toplevel_fundefs.borrow_mut().push(cont_fundef);

            let k_name1 = k_name.clone();
            let k_env1 = k_env.clone();
            let toplevel_fundefs1 = toplevel_fundefs.clone();
            let label_fvs1 = label_fvs.clone();
            let mut type_env1 = type_env.clone();
            let e1_cps = g(
                *e1,
                Box::new(move |r| {
                    let mut args = k_env1.clone();
                    args.push(r);
                    Term::App(k_name1.clone(), args)
                }),
                toplevel_fundefs,
                &mut type_env1,
                &label_fvs1,
            );

            let k_name2 = k_name.clone();
            let k_env2 = k_env.clone();
            let label_fvs2 = label_fvs.clone();
            let mut type_env2 = type_env.clone();
            let e2_cps = g(
                *e2,
                Box::new(move |r| {
                    let mut args = k_env2.clone();
                    args.push(r);
                    Term::App(k_name2.clone(), args)
                }),
                &toplevel_fundefs1,
                &mut type_env2,
                &label_fvs2,
            );

            Term::IfLE(x, y, Box::new(e1_cps), Box::new(e2_cps))
        }
        closure::Term::Tuple(xs) => {
            let y = id::gentmp(&Type::Tuple(vec![]));
            let mut elem_types = Vec::new();
            for x_elem in &xs {
                if let Some(t) = type_env.get(x_elem) {
                    elem_types.push(t.clone());
                } else {
                    elem_types.push(Type::Int); // Default or error
                }
            }
            type_env.insert(y.clone(), Type::Tuple(elem_types));

            Term::Let(
                (y.clone(), Type::Tuple(vec![])),
                Atom::Tuple(xs),
                Box::new(k(y)),
            )
        }
        closure::Term::LetTuple(xts, y, e2) => {
            for (x_elem, t_elem) in &xts {
                type_env.insert(x_elem.clone(), t_elem.clone());
            }
            Term::LetTuple(
                xts.clone(),
                y.clone(),
                Box::new(g(*e2, k, toplevel_fundefs, type_env, label_fvs)),
            )
        }
        closure::Term::Get(x, y) => {
            let z = id::gentmp(&Type::Int);
            type_env.insert(z.clone(), Type::Int);
            Term::Let(
                (z.clone(), Type::Int),
                Atom::Get(x.clone(), y.clone()),
                Box::new(k(z)),
            )
        }
        closure::Term::Put(x, y, z) => {
            let w = id::gentmp(&Type::Unit);
            type_env.insert(w.clone(), Type::Unit);
            Term::Let(
                (w.clone(), Type::Unit),
                Atom::Put(x.clone(), y.clone(), z.clone()),
                Box::new(k(w)),
            )
        }
        closure::Term::ExtArray(x) => {
            let y = id::gentmp(&Type::Array(Box::new(Type::Int)));
            type_env.insert(y.clone(), Type::Array(Box::new(Type::Int)));
            Term::Let(
                (y.clone(), Type::Array(Box::new(Type::Int))),
                Atom::ExtArray(x.clone()),
                Box::new(k(y)),
            )
        }
    }
}

pub fn f(prog: &ClosureProg) -> Prog {
    let cps_fundefs = Rc::new(RefCell::new(Vec::new()));

    // Build Label FVs map
    let mut label_fvs = HashMap::new();
    for fundef in &prog.fundefs {
        let fvs: Vec<id::T> = fundef.formal_fv.iter().map(|(x, _)| x.clone()).collect();
        label_fvs.insert(fundef.name.0.clone(), fvs);
    }
    // Halt logic
    label_fvs.insert("halt".to_string(), vec![]);
    // Standard Library Externals
    let externals = vec![
        "min_caml_print_int",
        "min_caml_print_newline",
        "min_caml_create_array",
        "min_caml_create_float_array",
        "min_caml_truncate",
        "min_caml_sin",
        "min_caml_cos",
        "min_caml_sqrt",
        "min_caml_abs_float",
        "min_caml_int_of_float",
        "min_caml_float_of_int",
        "min_caml_floor",
    ];
    for ext in externals {
        label_fvs.insert(ext.to_string(), vec![]);
    }

    for fundef in &prog.fundefs {
        let k_arg = id::genid("k");
        let k_type = Type::Fun(vec![Type::Unit], Box::new(Type::Unit));

        let mut new_args = Vec::new();
        for (fv_name, fv_type) in &fundef.formal_fv {
            new_args.push((fv_name.clone(), fv_type.clone()));
        }
        new_args.extend(fundef.args.clone());
        new_args.push((k_arg.clone(), k_type));

        let mut type_env = HashMap::new();
        // Populate type_env with args
        for (arg, t) in &new_args {
            type_env.insert(arg.clone(), t.clone());
        }

        let k_arg_clone = k_arg.clone();
        let body_cps = g(
            fundef.body.clone(),
            // k(x) -> App(k, [x])
            Box::new(move |x| Term::App(k_arg_clone.clone(), vec![x])),
            &cps_fundefs,
            &mut type_env,
            &label_fvs,
        );

        cps_fundefs.borrow_mut().push(Fundef {
            name: (fundef.name.0.clone(), fundef.name.1.clone()),
            args: new_args,
            body: Box::new(body_cps),
        });
    }

    let start_name = "min_caml_start".to_string();
    let mut type_env_start = HashMap::new();
    let main_cps_body = g(
        prog.body.clone(),
        Box::new(|x| Term::App("halt".to_string(), vec![x])), // Treat "halt" as Var/Label call via App. Or LoadLabel needed?
        // If we use App(x), blocked.rs treats x as Dynamic or Label.
        // If "halt" is handled as a label in blocked, App("halt") is fine.
        &cps_fundefs,
        &mut type_env_start,
        &label_fvs,
    );

    let start_fundef = Fundef {
        name: (start_name.clone(), Type::Fun(vec![], Box::new(Type::Unit))),
        args: vec![], // Closed main
        body: Box::new(main_cps_body),
    };
    cps_fundefs.borrow_mut().push(start_fundef);

    let f_ptr = id::gentmp(&Type::Int);
    let entry_term = Term::Let(
        (f_ptr.clone(), Type::Int),
        Atom::LoadLabel(start_name.clone()),
        Box::new(Term::App(f_ptr, vec![])),
    );

    Prog {
        fundefs: Rc::try_unwrap(cps_fundefs).unwrap().into_inner(),
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
            Atom::CallDir(l, args) => {
                let args_s = args
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "CallDir({}, [{}])", l, args_s)
            }
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
            Term::App(func, args) => {
                let args_str = args
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "App {}({})", func, args_str)
            }
            Term::AppCont(func, args, k, k_args) => {
                write!(
                    f,
                    "AppCont {}({:?}, cont {}, env {:?})",
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
        closure::Term::AppDir(l, args) => Some(Atom::CallDir(l.clone(), args.clone())),
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
        Term::App(f, args) => {
            let mut s = HashSet::new();
            s.insert(f.clone());
            for arg in args {
                s.insert(arg.clone());
            }
            s
        }
        Term::AppCont(f, args, _, k_args) => {
            let mut s = HashSet::new();
            s.insert(f.clone());
            for arg in args {
                s.insert(arg.clone());
            }
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
        Atom::CallDir(_, args) => {
            for arg in args {
                s.insert(arg.clone());
            }
        }
    }
    s
}
