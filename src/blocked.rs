use crate::cps::{self, Atom as CpsAtom, Prog as CpsProg, Term as CpsTerm};
use crate::id;
use crate::ty::Type;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub term: Term,
    pub id: id::L,
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
    TailCallCls(id::T),
    TailCallBlock(id::L),
    TailCallDynamic(id::T), // Call entry point stored in variable
    LoadLabel(id::L),       // Load label address into variable

    Push(id::T),
    Pop(id::T),

    Tuple(Vec<id::T>),
    LetTuple(Vec<(id::T, Type)>, id::T, Box<Term>),
    Get(id::T, id::T),
    Put(id::T, id::T, id::T),
    ExtArray(id::L),
    Goto(id::L),
    JumpVar(id::T), // Unconditional jump to variable
    CallDir(id::L, Vec<id::T>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Prog {
    pub blocks: Vec<Block>,
    pub entry: id::L,
    pub functions: Vec<(String, Vec<id::T>)>,
}

struct Converter {
    blocks: Vec<(String, Term)>,
    closure_fundefs: HashMap<String, crate::closure::Fundef>,
    tuple_env: HashMap<id::T, Vec<id::T>>,
    label_env: HashMap<id::T, id::L>,

    locals_stack: Vec<id::T>,
    locals: HashSet<id::T>,
}

impl Converter {
    fn new(closure_fundefs: HashMap<String, crate::closure::Fundef>) -> Converter {
        Converter {
            blocks: Vec::new(),
            closure_fundefs,
            tuple_env: HashMap::new(),
            label_env: HashMap::new(),
            locals_stack: Vec::new(),
            locals: HashSet::new(),
        }
    }

    fn add_block(&mut self, label: String, term: Term) {
        self.blocks.push((label, term));
    }

    fn convert_fundef(&mut self, fundef: &cps::Fundef) {
        let func_label = fundef.name.0.clone();
        let current_args = fundef.args.clone();
        let saved_locals = self.locals.clone();

        for (arg, _ty) in &current_args {
            self.locals.insert(arg.clone());
        }

        let body_term = self.convert_term(&fundef.body);
        let mut main_func_body = body_term;

        self.locals = saved_locals;

        let is_continuation = func_label.starts_with("k_");
        let total_args = current_args.len();

        if is_continuation && total_args > 0 {
            let last_idx = total_args - 1;
            let (last_arg, last_ty) = &current_args[last_idx];
            for i in (0..last_idx).rev() {
                let (arg, ty) = &current_args[i];
                main_func_body = Term::Let(
                    (arg.clone(), ty.clone()),
                    Box::new(Term::Pop(arg.clone())),
                    Box::new(main_func_body),
                );
            }

            main_func_body = Term::Let(
                (last_arg.clone(), last_ty.clone()),
                Box::new(Term::Pop(last_arg.clone())),
                Box::new(main_func_body),
            );
        } else {
            for i in (0..total_args).rev() {
                let (arg, ty) = &current_args[i];
                main_func_body = Term::Let(
                    (arg.clone(), ty.clone()),
                    Box::new(Term::Pop(arg.clone())),
                    Box::new(main_func_body),
                );
            }
        }

        self.add_block(func_label.clone(), main_func_body);
    }

    fn push_val(&self, arg: &id::T, res: Term) -> Term {
        if self.locals.contains(arg) {
            Term::Let(
                (id::gentmp(&Type::Unit), Type::Unit),
                Box::new(Term::Push(arg.clone())),
                Box::new(res),
            )
        } else if self.closure_fundefs.contains_key(arg) {
            let label_var = id::gentmp(&Type::Int);
            Term::Let(
                (label_var.clone(), Type::Int),
                Box::new(Term::LoadLabel(arg.clone())),
                Box::new(Term::Let(
                    (id::gentmp(&Type::Unit), Type::Unit),
                    Box::new(Term::Push(label_var)),
                    Box::new(res),
                )),
            )
        } else {
            Term::Let(
                (id::gentmp(&Type::Unit), Type::Unit),
                Box::new(Term::Push(arg.clone())),
                Box::new(res),
            )
        }
    }

    fn convert_term(&mut self, term: &CpsTerm) -> Term {
        match term {
            CpsTerm::Let((x, t), atom, e2) => {
                let val_term = match atom {
                    CpsAtom::Unit => Term::Unit,
                    CpsAtom::Int(i) => Term::Int(*i),
                    CpsAtom::Float(f) => Term::Float(*f),
                    CpsAtom::Var(v) => Term::Var(v.clone()),
                    CpsAtom::Neg(v) => Term::Neg(v.clone()),
                    CpsAtom::Add(v1, v2) => Term::Add(v1.clone(), v2.clone()),
                    CpsAtom::Sub(v1, v2) => Term::Sub(v1.clone(), v2.clone()),
                    CpsAtom::FNeg(v) => Term::FNeg(v.clone()),
                    CpsAtom::FAdd(v1, v2) => Term::FAdd(v1.clone(), v2.clone()),
                    CpsAtom::FSub(v1, v2) => Term::FSub(v1.clone(), v2.clone()),
                    CpsAtom::FMul(v1, v2) => Term::FMul(v1.clone(), v2.clone()),
                    CpsAtom::FDiv(v1, v2) => Term::FDiv(v1.clone(), v2.clone()),
                    CpsAtom::Get(v1, v2) => Term::Get(v1.clone(), v2.clone()),
                    CpsAtom::Put(v1, v2, v3) => Term::Put(v1.clone(), v2.clone(), v3.clone()),
                    CpsAtom::ExtArray(l) => Term::ExtArray(l.clone()),
                    CpsAtom::Tuple(xs) => {
                        self.tuple_env.insert(x.clone(), xs.clone());
                        Term::Tuple(xs.clone())
                    }
                    CpsAtom::LoadLabel(l) => {
                        self.label_env.insert(x.clone(), l.clone());
                        Term::LoadLabel(l.clone())
                    }
                    CpsAtom::CallDir(l, args) => Term::CallDir(l.clone(), args.clone()),
                };

                let added_local = self.locals.insert(x.clone());
                self.locals_stack.push(x.clone());
                let body_term = self.convert_term(e2);
                self.locals_stack.pop();
                if added_local {
                    self.locals.remove(x);
                }

                Term::Let(
                    (x.clone(), t.clone()),
                    Box::new(val_term),
                    Box::new(body_term),
                )
            }
            CpsTerm::LetTuple(xts, y, e) => {
                let mut added_locals = Vec::new();
                for (x, _) in xts {
                    if self.locals.insert(x.clone()) {
                        added_locals.push(x.clone());
                    }
                }
                let mut stack_count = 0;
                for (x, _) in xts {
                    self.locals_stack.push(x.clone());
                    stack_count += 1;
                }
                let next = self.convert_term(e);
                for _ in 0..stack_count {
                    self.locals_stack.pop();
                }
                for x in added_locals {
                    self.locals.remove(&x);
                }
                Term::LetTuple(xts.clone(), y.clone(), Box::new(next))
            }
            CpsTerm::IfEq(x, y, e1, e2) => {
                let saved_stack = self.locals_stack.clone();
                let t1 = self.convert_term(e1);
                self.locals_stack = saved_stack.clone();
                let t2 = self.convert_term(e2);
                self.locals_stack = saved_stack;
                Term::IfEq(x.clone(), y.clone(), Box::new(t1), Box::new(t2))
            }
            CpsTerm::IfLE(x, y, e1, e2) => {
                let saved_stack = self.locals_stack.clone();
                let t1 = self.convert_term(e1);
                self.locals_stack = saved_stack.clone();
                let t2 = self.convert_term(e2);
                self.locals_stack = saved_stack;
                Term::IfLE(x.clone(), y.clone(), Box::new(t1), Box::new(t2))
            }
            CpsTerm::App(f, args) => {
                let mut res = if f == "halt" {
                    Term::TailCallBlock(f.clone())
                } else if self.label_env.contains_key(f) {
                    let l = self.label_env.get(f).unwrap();
                    Term::TailCallBlock(l.clone())
                } else {
                    Term::TailCallDynamic(f.clone())
                };
                for arg in args.iter() {
                    res = self.push_val(arg, res);
                }
                res
            }
            CpsTerm::AppCont(f, args, k_label, k_args) => {
                let is_external = f.starts_with("min_caml_")
                    || f == "print_int"
                    || f == "print_newline"
                    || f == "truncate"
                    || f == "sin"
                    || f == "cos"
                    || f == "sqrt"
                    || f == "abs_float"
                    || f == "int_of_float"
                    || f == "float_of_int"
                    || f == "floor"
                    || f == "halt";

                let mut res = if self.closure_fundefs.contains_key(f) || is_external {
                    Term::TailCallBlock(f.clone())
                } else {
                    Term::TailCallDynamic(f.clone())
                };

                for arg in args {
                    res = self.push_val(arg, res);
                }

                let k_label_val = id::gentmp(&Type::Int);
                res = Term::Let(
                    (k_label_val.clone(), Type::Int),
                    Box::new(Term::LoadLabel(k_label.clone())),
                    Box::new(Term::Let(
                        (id::gentmp(&Type::Unit), Type::Unit),
                        Box::new(Term::Push(k_label_val)),
                        Box::new(res),
                    )),
                );

                for k_arg in k_args {
                    res = self.push_val(k_arg, res);
                }

                res
            }

            CpsTerm::Ret(x) => {
                let tag = id::gentmp(&Type::Int);
                let unit = id::gentmp(&Type::Unit);
                Term::Let(
                    (tag.clone(), Type::Int),
                    Box::new(Term::Pop(tag.clone())),
                    Box::new(Term::Let(
                        (unit, Type::Unit),
                        Box::new(Term::Push(x.clone())),
                        Box::new(Term::JumpVar(tag)),
                    )),
                )
            }
        }
    }
}

pub fn f(prog: &CpsProg, closure_prog: &crate::closure::Prog) -> Prog {
    let mut closure_fundefs = HashMap::new();
    for fundef in &closure_prog.fundefs {
        closure_fundefs.insert(fundef.name.0.clone(), fundef.clone());
    }

    let mut converter = Converter::new(closure_fundefs);

    let entry_label = "main".to_string();
    let main_term = converter.convert_term(&prog.body);
    converter.add_block(entry_label.clone(), main_term);

    for fundef in &prog.fundefs {
        converter.convert_fundef(fundef);
    }

    Prog {
        blocks: converter
            .blocks
            .into_iter()
            .map(|(id, term)| Block { id, term })
            .collect(),
        entry: entry_label,
        functions: prog
            .fundefs
            .iter()
            .map(|f| {
                (
                    f.name.0.clone(),
                    f.args.iter().map(|(x, _)| x.clone()).collect(),
                )
            })
            .collect(),
    }
}

use std::fmt;

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Unit => write!(f, "Unit"),
            Term::Int(i) => write!(f, "{}", i),
            Term::Float(d) => write!(f, "{:.6}", d),
            Term::Neg(x) => write!(f, "-{}", x),
            Term::Add(x, y) => write!(f, "{} + {}", x, y),
            Term::Sub(x, y) => write!(f, "{} - {}", x, y),
            Term::FNeg(x) => write!(f, "-.{}", x),
            Term::FAdd(x, y) => write!(f, "{} +. {}", x, y),
            Term::FSub(x, y) => write!(f, "{} -. {}", x, y),
            Term::FMul(x, y) => write!(f, "{} *. {}", x, y),
            Term::FDiv(x, y) => write!(f, "{} /. {}", x, y),
            Term::IfEq(x, y, e1, e2) => write!(
                f,
                "If ({} = {}) {{\n{}\n}}\nelse {{\n{}\n}}",
                x,
                y,
                indent_term(e1),
                indent_term(e2)
            ),
            Term::IfLE(x, y, e1, e2) => write!(
                f,
                "If ({} <= {}) {{\n{}\n}}\nelse {{\n{}\n}}",
                x,
                y,
                indent_term(e1),
                indent_term(e2)
            ),
            Term::Let((x, t), e1, e2) => write!(f, "{} {} = {};\n{}", t, x, e1, e2),
            Term::Var(x) => write!(f, "{}", x),
            Term::TailCallCls(x) => write!(f, "TailCallCls({})", x),
            Term::TailCallBlock(l) => write!(f, "TailCallBlock({})", l),
            Term::TailCallDynamic(x) => write!(f, "TailCallDynamic({})", x),
            Term::LoadLabel(l) => write!(f, "LoadLabel({})", l),

            Term::Push(x) => write!(f, "Push({})", x),
            Term::Pop(x) => write!(f, "Pop({})", x),

            Term::Tuple(xs) => {
                let elems_str = xs
                    .iter()
                    .map(|id_t| id_t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({})", elems_str)
            }
            Term::LetTuple(xts, y, e) => {
                let vars_str = xts
                    .iter()
                    .map(|(x, t)| format!("({}: {})", x, t))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({}) = {};\n{}", vars_str, y, e)
            }
            Term::Get(x, y) => write!(f, "{}[{}]", x, y),
            Term::Put(x, y, z) => write!(f, "{}[{}] = {};", x, y, z),
            Term::ExtArray(x) => write!(f, "ExtArray({})", x),
            Term::Goto(l) => write!(f, "Goto {}", l),
            Term::JumpVar(x) => write!(f, "JumpVar({})", x),
            Term::CallDir(l, args) => {
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

fn indent_term(e: &Term) -> String {
    let s = format!("{}", e);
    s.lines()
        .map(|line| format!("  {}", line))
        .collect::<Vec<_>>()
        .join("\n")
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:\n{}", self.id, indent_term(&self.term))
    }
}

impl fmt::Display for Prog {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Entry: {}\n\n", self.entry)?;
        for block in &self.blocks {
            write!(f, "{}\n\n", block)?;
        }
        Ok(())
    }
}
