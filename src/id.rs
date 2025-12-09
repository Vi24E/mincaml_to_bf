use crate::ty::Type;
use std::sync::atomic::{AtomicUsize, Ordering};

pub type T = String;
pub type L = String;

static COUNTER: AtomicUsize = AtomicUsize::new(0);

pub fn genid(s: &str) -> String {
    let count = COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("{}.{}", s, count + 1)
}

pub fn gentmp(typ: &Type) -> String {
    let count = COUNTER.fetch_add(1, Ordering::SeqCst);
    let s = match typ {
        Type::Unit => "u",
        Type::Bool => "b",
        Type::Int => "i",
        Type::Float => "d",
        Type::Fun(_, _) => "f",
        Type::Tuple(_) => "t",
        Type::Array(_) => "a",
        Type::Var(_) => "v", // Should be handled better if needed
    };
    format!("T{}{}", s, count + 1)
}

pub fn id_of_typ(typ: &Type) -> String {
    match typ {
        Type::Unit => "u".to_string(),
        Type::Bool => "b".to_string(),
        Type::Int => "i".to_string(),
        Type::Float => "d".to_string(),
        Type::Fun(_, _) => "f".to_string(),
        Type::Tuple(_) => "t".to_string(),
        Type::Array(_) => "a".to_string(),
        Type::Var(_) => "v".to_string(),
    }
}
