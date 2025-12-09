use crate::id;
use crate::k_normal::{Fundef, KNormal};
use std::collections::HashMap;

fn find(x: &str, env: &HashMap<String, String>) -> String {
    match env.get(x) {
        Some(y) => y.clone(),
        None => x.to_string(),
    }
}

pub fn g(env: &HashMap<String, String>, e: &KNormal) -> KNormal {
    match e {
        KNormal::Unit => KNormal::Unit,
        KNormal::Int(i) => KNormal::Int(*i),
        KNormal::Float(d) => KNormal::Float(*d),
        KNormal::Neg(x) => KNormal::Neg(find(x, env)),
        KNormal::Add(x, y) => KNormal::Add(find(x, env), find(y, env)),
        KNormal::Sub(x, y) => KNormal::Sub(find(x, env), find(y, env)),
        KNormal::FNeg(x) => KNormal::FNeg(find(x, env)),
        KNormal::FAdd(x, y) => KNormal::FAdd(find(x, env), find(y, env)),
        KNormal::FSub(x, y) => KNormal::FSub(find(x, env), find(y, env)),
        KNormal::FMul(x, y) => KNormal::FMul(find(x, env), find(y, env)),
        KNormal::FDiv(x, y) => KNormal::FDiv(find(x, env), find(y, env)),
        KNormal::IfEq(x, y, e1, e2) => KNormal::IfEq(find(x, env), find(y, env), Box::new(g(env, e1)), Box::new(g(env, e2))),
        KNormal::IfLE(x, y, e1, e2) => KNormal::IfLE(find(x, env), find(y, env), Box::new(g(env, e1)), Box::new(g(env, e2))),
        KNormal::Let((x, t), e1, e2) => {
            let x_prime = id::genid(x);
            let mut new_env = env.clone();
            new_env.insert(x.clone(), x_prime.clone());
            KNormal::Let((x_prime, t.clone()), Box::new(g(env, e1)), Box::new(g(&new_env, e2)))
        }
        KNormal::Var(x) => KNormal::Var(find(x, env)),
        KNormal::LetRec(fundef, e2) => {
            let x = &fundef.name.0;
            let x_prime = id::genid(x);
            let mut new_env = env.clone();
            new_env.insert(x.clone(), x_prime.clone());
            
            let mut body_env = new_env.clone();
            let mut args_prime = Vec::new();
            for (arg, ty) in &fundef.args {
                let arg_prime = id::genid(arg);
                body_env.insert(arg.clone(), arg_prime.clone());
                args_prime.push((arg_prime, ty.clone()));
            }
            
            KNormal::LetRec(Fundef {
                name: (x_prime, fundef.name.1.clone()),
                args: args_prime,
                body: Box::new(g(&body_env, &fundef.body)),
            }, Box::new(g(&new_env, e2)))
        }
        KNormal::App(x, ys) => KNormal::App(find(x, env), ys.iter().map(|y| find(y, env)).collect()),
        KNormal::Tuple(xs) => KNormal::Tuple(xs.iter().map(|x| find(x, env)).collect()),
        KNormal::LetTuple(xts, y, e) => {
            let mut new_env = env.clone();
            let mut xts_prime = Vec::new();
            for (x, t) in xts {
                let x_prime = id::genid(x);
                new_env.insert(x.clone(), x_prime.clone());
                xts_prime.push((x_prime, t.clone()));
            }
            KNormal::LetTuple(xts_prime, find(y, env), Box::new(g(&new_env, e)))
        }
        KNormal::Get(x, y) => KNormal::Get(find(x, env), find(y, env)),
        KNormal::Put(x, y, z) => KNormal::Put(find(x, env), find(y, env), find(z, env)),
        KNormal::ExtArray(x) => KNormal::ExtArray(x.clone()), // External arrays are not renamed? Or should they be? min-caml says ExtArray(x) -> ExtArray(x)
        KNormal::ExtFunApp(x, ys) => KNormal::ExtFunApp(x.clone(), ys.iter().map(|y| find(y, env)).collect()),
    }
}

pub fn f(e: &KNormal) -> KNormal {
    g(&HashMap::new(), e)
}
