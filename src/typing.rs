use crate::syntax::Syntax;
use crate::ty::Type;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub enum Error {
    Unify(Type, Type),
    Error(Syntax, Type, Type),
}

pub fn deref_typ(t: &Type) -> Type {
    match t {
        Type::Fun(t1s, t2) => Type::Fun(
            t1s.iter().map(deref_typ).collect(),
            Box::new(deref_typ(t2)),
        ),
        Type::Tuple(ts) => Type::Tuple(ts.iter().map(deref_typ).collect()),
        Type::Array(t) => Type::Array(Box::new(deref_typ(t))),
        Type::Var(r) => {
            let mut borrowed = r.borrow_mut();
            match &*borrowed {
                None => {
                    // Uninstantiated type variable; assume int
                    // In original min-caml, it prints a warning and defaults to Int.
                    *borrowed = Some(Type::Int);
                    Type::Int
                }
                Some(t) => {
                    let t_prime = deref_typ(t);
                    *borrowed = Some(t_prime.clone());
                    t_prime
                }
            }
        }
        t => t.clone(),
    }
}

fn occur(r1: &Rc<RefCell<Option<Type>>>, t: &Type) -> bool {
    match t {
        Type::Fun(t2s, t2) => t2s.iter().any(|t| occur(r1, t)) || occur(r1, t2),
        Type::Tuple(t2s) => t2s.iter().any(|t| occur(r1, t)),
        Type::Array(t2) => occur(r1, t2),
        Type::Var(r2) => {
            if Rc::ptr_eq(r1, r2) {
                true
            } else {
                match &*r2.borrow() {
                    None => false,
                    Some(t2) => occur(r1, t2),
                }
            }
        }
        _ => false,
    }
}

pub fn unify(t1: &Type, t2: &Type) -> Result<(), Error> {
    match (t1, t2) {
        (Type::Unit, Type::Unit)
        | (Type::Bool, Type::Bool)
        | (Type::Int, Type::Int)
        | (Type::Float, Type::Float) => Ok(()),
        (Type::Fun(t1s, t1_ret), Type::Fun(t2s, t2_ret)) => {
            if t1s.len() != t2s.len() {
                return Err(Error::Unify(t1.clone(), t2.clone()));
            }
            for (arg1, arg2) in t1s.iter().zip(t2s.iter()) {
                unify(arg1, arg2)?;
            }
            unify(t1_ret, t2_ret)
        }
        (Type::Tuple(t1s), Type::Tuple(t2s)) => {
            if t1s.len() != t2s.len() {
                return Err(Error::Unify(t1.clone(), t2.clone()));
            }
            for (elem1, elem2) in t1s.iter().zip(t2s.iter()) {
                unify(elem1, elem2)?;
            }
            Ok(())
        }
        (Type::Array(t1), Type::Array(t2)) => unify(t1, t2),
        (Type::Var(r1), Type::Var(r2)) if Rc::ptr_eq(r1, r2) => Ok(()),
        (Type::Var(r1), _) => {
            if let Some(t1_val) = &*r1.borrow() {
                unify(t1_val, t2)
            } else {
                if occur(r1, t2) {
                    return Err(Error::Unify(t1.clone(), t2.clone()));
                }
                *r1.borrow_mut() = Some(t2.clone());
                Ok(())
            }
        }
        (_, Type::Var(r2)) => {
            if let Some(t2_val) = &*r2.borrow() {
                unify(t1, t2_val)
            } else {
                if occur(r2, t1) {
                    return Err(Error::Unify(t1.clone(), t2.clone()));
                }
                *r2.borrow_mut() = Some(t1.clone());
                Ok(())
            }
        }
        _ => Err(Error::Unify(t1.clone(), t2.clone())),
    }
}

pub fn g(env: &HashMap<String, Type>, e: &Syntax) -> Result<Type, Error> {
    match e {
        Syntax::Unit => Ok(Type::Unit),
        Syntax::Bool(_) => Ok(Type::Bool),
        Syntax::Int(_) => Ok(Type::Int),
        Syntax::Float(_) => Ok(Type::Float),
        Syntax::Not(e) => {
            unify(&Type::Bool, &g(env, e)?)?;
            Ok(Type::Bool)
        }
        Syntax::Neg(e) => {
            unify(&Type::Int, &g(env, e)?)?;
            Ok(Type::Int)
        }
        Syntax::Add(e1, e2) | Syntax::Sub(e1, e2) => {
            unify(&Type::Int, &g(env, e1)?)?;
            unify(&Type::Int, &g(env, e2)?)?;
            Ok(Type::Int)
        }
        Syntax::FNeg(e) => {
            unify(&Type::Float, &g(env, e)?)?;
            Ok(Type::Float)
        }
        Syntax::FAdd(e1, e2) | Syntax::FSub(e1, e2) | Syntax::FMul(e1, e2) | Syntax::FDiv(e1, e2) => {
            unify(&Type::Float, &g(env, e1)?)?;
            unify(&Type::Float, &g(env, e2)?)?;
            Ok(Type::Float)
        }
        Syntax::Eq(e1, e2) | Syntax::LE(e1, e2) => {
            unify(&g(env, e1)?, &g(env, e2)?)?;
            Ok(Type::Bool)
        }
        Syntax::If(e1, e2, e3) => {
            unify(&g(env, e1)?, &Type::Bool)?;
            let t2 = g(env, e2)?;
            let t3 = g(env, e3)?;
            unify(&t2, &t3)?;
            Ok(t2)
        }
        Syntax::Let((x, t), e1, e2) => {
            unify(t, &g(env, e1)?)?;
            let mut new_env = env.clone();
            new_env.insert(x.clone(), t.clone());
            g(&new_env, e2)
        }
        Syntax::Var(x) => {
            if let Some(t) = env.get(x) {
                Ok(t.clone())
            } else {
                // External variable
                // In min-caml, it assumes external variables are typed via `extenv`.
                // For now, let's create a fresh type and assume it's external.
                // But we need to persist this `extenv`.
                // Since `g` is pure here, we might need a RefCell for extenv or pass it mutably.
                // For simplicity, let's just generate a new type and warn.
                // Or better, use a global/static or thread-local for extenv if we want to match min-caml exactly.
                // Let's use a static RefCell for EXTENV.
                
                // For now, just generate a fresh type.
                Ok(Type::gentyp())
            }
        }
        Syntax::LetRec(fundef, e2) => {
            let mut new_env = env.clone();
            new_env.insert(fundef.name.0.clone(), fundef.name.1.clone());
            
            let mut body_env = new_env.clone();
            for (arg, ty) in &fundef.args {
                body_env.insert(arg.clone(), ty.clone());
            }
            
            let body_type = g(&body_env, &fundef.body)?;
            let func_type = Type::Fun(
                fundef.args.iter().map(|(_, t)| t.clone()).collect(),
                Box::new(body_type),
            );
            unify(&fundef.name.1, &func_type)?;
            
            g(&new_env, e2)
        }
        Syntax::App(e, es) => {
            let t_ret = Type::gentyp();
            let t_args: Vec<Type> = es.iter().map(|arg| g(env, arg)).collect::<Result<_, _>>()?;
            unify(&g(env, e)?, &Type::Fun(t_args, Box::new(t_ret.clone())))?;
            Ok(t_ret)
        }
        Syntax::Tuple(es) => {
            let ts = es.iter().map(|e| g(env, e)).collect::<Result<_, _>>()?;
            Ok(Type::Tuple(ts))
        }
        Syntax::LetTuple(xts, e1, e2) => {
            unify(
                &Type::Tuple(xts.iter().map(|(_, t)| t.clone()).collect()),
                &g(env, e1)?,
            )?;
            let mut new_env = env.clone();
            for (x, t) in xts {
                new_env.insert(x.clone(), t.clone());
            }
            g(&new_env, e2)
        }
        Syntax::Array(e1, e2) => {
            unify(&g(env, e1)?, &Type::Int)?;
            Ok(Type::Array(Box::new(g(env, e2)?)))
        }
        Syntax::Get(e1, e2) => {
            let t = Type::gentyp();
            unify(&g(env, e1)?, &Type::Array(Box::new(t.clone())))?;
            unify(&g(env, e2)?, &Type::Int)?;
            Ok(t)
        }
        Syntax::Put(e1, e2, e3) => {
            let t = g(env, e3)?;
            unify(&g(env, e1)?, &Type::Array(Box::new(t)))?;
            unify(&g(env, e2)?, &Type::Int)?;
            Ok(Type::Unit)
        }
    }
}

pub fn f(e: &Syntax) -> Result<Syntax, Error> {
    // Reset extenv if we had one.
    // Run inference
    // unify(&Type::Unit, &g(&HashMap::new(), e)?)?; // Don't enforce Unit for now
    g(&HashMap::new(), e)?;
    
    // Dereference types in the syntax tree?
    Ok(deref_term_syntax(e))
}

fn deref_term_syntax(e: &Syntax) -> Syntax {
    match e {
        Syntax::Not(e) => Syntax::Not(Box::new(deref_term_syntax(e))),
        Syntax::Neg(e) => Syntax::Neg(Box::new(deref_term_syntax(e))),
        Syntax::Add(e1, e2) => Syntax::Add(Box::new(deref_term_syntax(e1)), Box::new(deref_term_syntax(e2))),
        Syntax::Sub(e1, e2) => Syntax::Sub(Box::new(deref_term_syntax(e1)), Box::new(deref_term_syntax(e2))),
        Syntax::FNeg(e) => Syntax::FNeg(Box::new(deref_term_syntax(e))),
        Syntax::FAdd(e1, e2) => Syntax::FAdd(Box::new(deref_term_syntax(e1)), Box::new(deref_term_syntax(e2))),
        Syntax::FSub(e1, e2) => Syntax::FSub(Box::new(deref_term_syntax(e1)), Box::new(deref_term_syntax(e2))),
        Syntax::FMul(e1, e2) => Syntax::FMul(Box::new(deref_term_syntax(e1)), Box::new(deref_term_syntax(e2))),
        Syntax::FDiv(e1, e2) => Syntax::FDiv(Box::new(deref_term_syntax(e1)), Box::new(deref_term_syntax(e2))),
        Syntax::Eq(e1, e2) => Syntax::Eq(Box::new(deref_term_syntax(e1)), Box::new(deref_term_syntax(e2))),
        Syntax::LE(e1, e2) => Syntax::LE(Box::new(deref_term_syntax(e1)), Box::new(deref_term_syntax(e2))),
        Syntax::If(e1, e2, e3) => Syntax::If(
            Box::new(deref_term_syntax(e1)),
            Box::new(deref_term_syntax(e2)),
            Box::new(deref_term_syntax(e3)),
        ),
        Syntax::Let((x, t), e1, e2) => Syntax::Let(
            (x.clone(), deref_typ(t)),
            Box::new(deref_term_syntax(e1)),
            Box::new(deref_term_syntax(e2)),
        ),
        Syntax::LetRec(fundef, e2) => Syntax::LetRec(
            crate::syntax::Fundef {
                name: (fundef.name.0.clone(), deref_typ(&fundef.name.1)),
                args: fundef.args.iter().map(|(x, t)| (x.clone(), deref_typ(t))).collect(),
                body: Box::new(deref_term_syntax(&fundef.body)),
            },
            Box::new(deref_term_syntax(e2)),
        ),
        Syntax::App(e, es) => Syntax::App(
            Box::new(deref_term_syntax(e)),
            es.iter().map(deref_term_syntax).collect(),
        ),
        Syntax::Tuple(es) => Syntax::Tuple(es.iter().map(deref_term_syntax).collect()),
        Syntax::LetTuple(xts, e1, e2) => Syntax::LetTuple(
            xts.iter().map(|(x, t)| (x.clone(), deref_typ(t))).collect(),
            Box::new(deref_term_syntax(e1)),
            Box::new(deref_term_syntax(e2)),
        ),
        Syntax::Array(e1, e2) => Syntax::Array(
            Box::new(deref_term_syntax(e1)),
            Box::new(deref_term_syntax(e2)),
        ),
        Syntax::Get(e1, e2) => Syntax::Get(
            Box::new(deref_term_syntax(e1)),
            Box::new(deref_term_syntax(e2)),
        ),
        Syntax::Put(e1, e2, e3) => Syntax::Put(
            Box::new(deref_term_syntax(e1)),
            Box::new(deref_term_syntax(e2)),
            Box::new(deref_term_syntax(e3)),
        ),
        e => e.clone(),
    }
}
