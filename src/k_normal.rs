use crate::id;
use crate::syntax;
use crate::ty::Type;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum KNormal {
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
    IfEq(id::T, id::T, Box<KNormal>, Box<KNormal>),
    IfLE(id::T, id::T, Box<KNormal>, Box<KNormal>),
    Let((id::T, Type), Box<KNormal>, Box<KNormal>),
    Var(id::T),
    LetRec(Fundef, Box<KNormal>),
    App(id::T, Vec<id::T>),
    Tuple(Vec<id::T>),
    LetTuple(Vec<(id::T, Type)>, id::T, Box<KNormal>),
    Get(id::T, id::T),
    Put(id::T, id::T, id::T),
    ExtArray(id::T),
    ExtFunApp(id::T, Vec<id::T>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fundef {
    pub name: (id::T, Type),
    pub args: Vec<(id::T, Type)>,
    pub body: Box<KNormal>,
}

fn insert_let(e: (KNormal, Type), k: impl FnOnce(id::T) -> (KNormal, Type)) -> (KNormal, Type) {
    let (e_term, e_type) = e;
    match e_term {
        KNormal::Var(x) => k(x),
        KNormal::Let((x, t), e1, e2) => {
            let (e2_prime, t2) = insert_let((*e2, e_type), k);
            (KNormal::Let((x, t), e1, Box::new(e2_prime)), t2)
        }
        KNormal::LetRec(fundef, e2) => {
            let (e2_prime, t2) = insert_let((*e2, e_type), k);
            (KNormal::LetRec(fundef, Box::new(e2_prime)), t2)
        }
        KNormal::LetTuple(xts, y, e2) => {
            let (e2_prime, t2) = insert_let((*e2, e_type), k);
            (KNormal::LetTuple(xts, y, Box::new(e2_prime)), t2)
        }
        _ => {
            let x = id::gentmp(&e_type);
            let (e_prime, t_prime) = k(x.clone());
            (
                KNormal::Let((x, e_type), Box::new(e_term), Box::new(e_prime)),
                t_prime,
            )
        }
    }
}

pub fn g(env: &HashMap<String, Type>, e: &syntax::Syntax) -> (KNormal, Type) {
    match e {
        syntax::Syntax::Unit => (KNormal::Unit, Type::Unit),
        syntax::Syntax::Bool(b) => (KNormal::Int(if *b { 1 } else { 0 }), Type::Int),
        syntax::Syntax::Int(i) => (KNormal::Int(*i), Type::Int),
        syntax::Syntax::Float(d) => (KNormal::Float(*d), Type::Float),
        syntax::Syntax::Not(e) => g(
            env,
            &syntax::Syntax::If(
                e.clone(),
                Box::new(syntax::Syntax::Bool(false)),
                Box::new(syntax::Syntax::Bool(true)),
            ),
        ),
        syntax::Syntax::Neg(e) => insert_let(g(env, e), |x| (KNormal::Neg(x), Type::Int)),
        syntax::Syntax::Add(e1, e2) => insert_let(g(env, e1), |x| {
            insert_let(g(env, e2), |y| (KNormal::Add(x, y), Type::Int))
        }),
        syntax::Syntax::Sub(e1, e2) => insert_let(g(env, e1), |x| {
            insert_let(g(env, e2), |y| (KNormal::Sub(x, y), Type::Int))
        }),
        syntax::Syntax::FNeg(e) => insert_let(g(env, e), |x| (KNormal::FNeg(x), Type::Float)),
        syntax::Syntax::FAdd(e1, e2) => insert_let(g(env, e1), |x| {
            insert_let(g(env, e2), |y| (KNormal::FAdd(x, y), Type::Float))
        }),
        syntax::Syntax::FSub(e1, e2) => insert_let(g(env, e1), |x| {
            insert_let(g(env, e2), |y| (KNormal::FSub(x, y), Type::Float))
        }),
        syntax::Syntax::FMul(e1, e2) => insert_let(g(env, e1), |x| {
            insert_let(g(env, e2), |y| (KNormal::FMul(x, y), Type::Float))
        }),
        syntax::Syntax::FDiv(e1, e2) => insert_let(g(env, e1), |x| {
            insert_let(g(env, e2), |y| (KNormal::FDiv(x, y), Type::Float))
        }),
        syntax::Syntax::Eq(e1, e2) | syntax::Syntax::LE(e1, e2) => g(
            env,
            &syntax::Syntax::If(
                Box::new(e.clone()),
                Box::new(syntax::Syntax::Bool(true)),
                Box::new(syntax::Syntax::Bool(false)),
            ),
        ),
        syntax::Syntax::If(e1, e2, e3) => match &**e1 {
            syntax::Syntax::Not(e1_inner) => g(
                env,
                &syntax::Syntax::If(e1_inner.clone(), e3.clone(), e2.clone()),
            ),
            syntax::Syntax::Eq(e1_inner, e2_inner) => insert_let(g(env, e1_inner), |x| {
                insert_let(g(env, e2_inner), |y| {
                    let (e2_prime, t2) = g(env, e2);
                    let (e3_prime, _) = g(env, e3);
                    (
                        KNormal::IfEq(x, y, Box::new(e2_prime), Box::new(e3_prime)),
                        t2,
                    )
                })
            }),
            syntax::Syntax::LE(e1_inner, e2_inner) => insert_let(g(env, e1_inner), |x| {
                insert_let(g(env, e2_inner), |y| {
                    let (e2_prime, t2) = g(env, e2);
                    let (e3_prime, _) = g(env, e3);
                    (
                        KNormal::IfLE(x, y, Box::new(e2_prime), Box::new(e3_prime)),
                        t2,
                    )
                })
            }),
            _ => g(
                env,
                &syntax::Syntax::If(
                    Box::new(syntax::Syntax::Eq(
                        e1.clone(),
                        Box::new(syntax::Syntax::Bool(false)),
                    )),
                    e3.clone(),
                    e2.clone(),
                ),
            ),
        },
        syntax::Syntax::Let((x, t), e1, e2) => {
            let (e1_prime, _) = g(env, e1);
            let mut new_env = env.clone();
            new_env.insert(x.clone(), t.clone());
            let (e2_prime, t2) = g(&new_env, e2);
            (
                KNormal::Let(
                    (x.clone(), t.clone()),
                    Box::new(e1_prime),
                    Box::new(e2_prime),
                ),
                t2,
            )
        }
        syntax::Syntax::Var(x) => {
            if env.contains_key(x) {
                (KNormal::Var(x.clone()), env[x].clone())
            } else {
                // External variable (assume array for now as per min-caml)
                // Or check extenv if we had it.
                // For now, assume it's an external array if not found.
                // But wait, in min-caml `Var(x)` when not in env checks `Typing.extenv`.
                // We don't have global `Typing.extenv`.
                // Let's assume it's an external array of Int for simplicity or panic?
                // Or better, assume it's a function if we encounter App later.
                // Here, if it's just Var(x), it's likely an array or value.
                // Let's assume array of int.
                (
                    KNormal::ExtArray(x.clone()),
                    Type::Array(Box::new(Type::Int)),
                )
            }
        }
        syntax::Syntax::LetRec(fundef, e2) => {
            let mut new_env = env.clone();
            new_env.insert(fundef.name.0.clone(), fundef.name.1.clone());
            let (e2_prime, t2) = g(&new_env, e2);
            let mut body_env = new_env.clone();
            for (arg, ty) in &fundef.args {
                body_env.insert(arg.clone(), ty.clone());
            }
            let (body_prime, _) = g(&body_env, &fundef.body);
            (
                KNormal::LetRec(
                    Fundef {
                        name: (fundef.name.0.clone(), fundef.name.1.clone()),
                        args: fundef.args.clone(),
                        body: Box::new(body_prime),
                    },
                    Box::new(e2_prime),
                ),
                t2,
            )
        }
        syntax::Syntax::App(e, es) => {
            match &**e {
                syntax::Syntax::Var(f) if !env.contains_key(f) => {
                    // External function application
                    // We need to know the return type.
                    // In min-caml, it looks up `Typing.extenv`.
                    // We'll assume Int return type for unknown external functions for now, or Unit.
                    // Let's assume Int.
                    let t_ret = Type::Int;
                    let args: Vec<id::T> = Vec::new();

                    // Helper to bind arguments
                    // We need to recursively bind arguments.
                    // Since we can't easily do recursive closure, we'll iterate.
                    // But `insert_let` takes a closure.
                    // We can use a recursive helper function.

                    fn bind_args(
                        env: &HashMap<String, Type>,
                        es: &[syntax::Syntax],
                        f: String,
                        t_ret: Type,
                        args_acc: Vec<id::T>,
                    ) -> (KNormal, Type) {
                        if es.is_empty() {
                            (KNormal::ExtFunApp(f, args_acc), t_ret)
                        } else {
                            insert_let(g(env, &es[0]), |x| {
                                let mut new_args = args_acc.clone();
                                new_args.push(x);
                                bind_args(env, &es[1..], f, t_ret, new_args)
                            })
                        }
                    }

                    bind_args(env, es, f.clone(), t_ret, Vec::new())
                }
                _ => {
                    let (e_prime, t_func) = g(env, e);
                    match t_func.clone() {
                        Type::Fun(_, t_ret) => insert_let((e_prime, t_func), |f| {
                            fn bind_args(
                                env: &HashMap<String, Type>,
                                es: &[syntax::Syntax],
                                f: id::T,
                                t_ret: Type,
                                args_acc: Vec<id::T>,
                            ) -> (KNormal, Type) {
                                if es.is_empty() {
                                    (KNormal::App(f, args_acc), t_ret)
                                } else {
                                    insert_let(g(env, &es[0]), |x| {
                                        let mut new_args = args_acc.clone();
                                        new_args.push(x);
                                        bind_args(env, &es[1..], f, t_ret, new_args)
                                    })
                                }
                            }
                            bind_args(env, es, f, *t_ret, Vec::new())
                        }),
                        _ => panic!("Expected function type"),
                    }
                }
            }
        }
        syntax::Syntax::Tuple(es) => {
            fn bind_elems(
                env: &HashMap<String, Type>,
                es: &[syntax::Syntax],
                ids_acc: Vec<id::T>,
                types_acc: Vec<Type>,
            ) -> (KNormal, Type) {
                if es.is_empty() {
                    (KNormal::Tuple(ids_acc), Type::Tuple(types_acc))
                } else {
                    let (e_prime, t) = g(env, &es[0]);
                    insert_let((e_prime, t.clone()), |x| {
                        let mut new_ids = ids_acc.clone();
                        new_ids.push(x);
                        let mut new_types = types_acc.clone();
                        new_types.push(t);
                        bind_elems(env, &es[1..], new_ids, new_types)
                    })
                }
            }
            bind_elems(env, es, Vec::new(), Vec::new())
        }
        syntax::Syntax::LetTuple(xts, e1, e2) => insert_let(g(env, e1), |y| {
            let mut new_env = env.clone();
            for (x, t) in xts {
                new_env.insert(x.clone(), t.clone());
            }
            let (e2_prime, t2) = g(&new_env, e2);
            (KNormal::LetTuple(xts.clone(), y, Box::new(e2_prime)), t2)
        }),
        syntax::Syntax::Array(e1, e2) => insert_let(g(env, e1), |x| {
            let (e2_prime, t2) = g(env, e2);
            insert_let((e2_prime, t2.clone()), |y| {
                let label = match t2 {
                    Type::Float => "create_float_array",
                    _ => "create_array",
                };
                (
                    KNormal::ExtFunApp(label.to_string(), vec![x, y]),
                    Type::Array(Box::new(t2)),
                )
            })
        }),
        syntax::Syntax::Get(e1, e2) => {
            let (e1_prime, t1) = g(env, e1);
            match t1 {
                Type::Array(t_elem) => insert_let((e1_prime, Type::Array(t_elem.clone())), |x| {
                    insert_let(g(env, e2), |y| (KNormal::Get(x, y), *t_elem))
                }),
                _ => panic!("Expected array type"),
            }
        }
        syntax::Syntax::Put(e1, e2, e3) => insert_let(g(env, e1), |x| {
            insert_let(g(env, e2), |y| {
                insert_let(g(env, e3), |z| (KNormal::Put(x, y, z), Type::Unit))
            })
        }),
    }
}

pub fn f(e: &syntax::Syntax) -> KNormal {
    flatten(g(&HashMap::new(), e).0)
}

fn flatten(e: KNormal) -> KNormal {
    match e {
        KNormal::Let((x, t), e1, e2) => {
            let e1_prime = flatten(*e1);
            match e1_prime {
                KNormal::Let((y, ty), ey1, ey2) => {
                    // Let(x, Let(y, ey1, ey2), e2) -> Let(y, ey1, Let(x, ey2, e2))
                    let inner = KNormal::Let((x, t), ey2, e2);
                    let inner_flat = flatten(inner);
                    KNormal::Let((y, ty), ey1, Box::new(inner_flat))
                }
                _ => {
                    let e2_prime = flatten(*e2);
                    KNormal::Let((x, t), Box::new(e1_prime), Box::new(e2_prime))
                }
            }
        }
        KNormal::IfEq(x, y, e1, e2) => {
            KNormal::IfEq(x, y, Box::new(flatten(*e1)), Box::new(flatten(*e2)))
        }
        KNormal::IfLE(x, y, e1, e2) => {
            KNormal::IfLE(x, y, Box::new(flatten(*e1)), Box::new(flatten(*e2)))
        }
        KNormal::LetRec(fundef, e2) => {
            let body_prime = flatten(*fundef.body);
            let e2_prime = flatten(*e2);
            KNormal::LetRec(
                Fundef {
                    body: Box::new(body_prime),
                    ..fundef
                },
                Box::new(e2_prime),
            )
        }
        KNormal::LetTuple(xts, y, e2) => KNormal::LetTuple(xts, y, Box::new(flatten(*e2))),
        _ => e,
    }
}
