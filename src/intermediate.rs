use crate::blocked::{Prog as BlockedProg, Term as BlockedTerm};
use crate::closure::Prog as ClosureProg;
use crate::id;
use crate::ty::Type;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Prog {
    pub blocks: Vec<Block>,
    pub entry: id::L,
    pub layout: Layout,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub term: Term,
    pub id: id::L,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
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
    Var(id::T),
    GetStack(usize),
    Tuple(Vec<id::T>),
    Get(id::T, id::T),
    Put(id::T, id::T, id::T),
    ExtArray(id::L),
    LoadLabel(id::L),
    Push(id::T),
    Pop,

    CallDir(id::L, Vec<id::T>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Atom(Atom),
    IfEq(id::T, id::T, id::L, id::L),
    IfLE(id::T, id::T, id::L, id::L),
    Let((id::T, Type), Atom, Box<Term>),
    LetTuple(Vec<(id::T, Type)>, Atom, Box<Term>),
    Jump(id::L),
    JumpVar(id::T),
    CallExternal(id::L),
    Ret(id::T),
}

struct Converter {
    blocks: Vec<Block>,
    // current_func_args_len: usize, // Unused
    known_labels: HashMap<id::T, id::L>,
}

impl Converter {
    fn new(_func_arg_counts: HashMap<String, usize>) -> Self {
        Converter {
            blocks: Vec::new(),
            // current_func_args_len: 0,
            known_labels: HashMap::new(),
        }
    }

    fn new_block_id(&self) -> id::L {
        id::genid("block")
    }

    fn add_block(&mut self, id: id::L, term: Term) {
        self.blocks.push(Block { id, term });
    }

    fn convert_term(
        &mut self,
        term: &BlockedTerm,
        dest: Option<(id::T, Type)>,
        next: Option<id::L>,
    ) -> Term {
        match term {
            BlockedTerm::Unit => self.bind_atom(Atom::Unit, dest, next),
            BlockedTerm::Int(i) => self.bind_atom(Atom::Int(*i), dest, next),
            BlockedTerm::Float(d) => self.bind_atom(Atom::Float(*d), dest, next),
            BlockedTerm::Neg(x) => self.bind_atom(Atom::Neg(x.clone()), dest, next),
            BlockedTerm::Add(x, y) => self.bind_atom(Atom::Add(x.clone(), y.clone()), dest, next),
            BlockedTerm::Sub(x, y) => self.bind_atom(Atom::Sub(x.clone(), y.clone()), dest, next),
            BlockedTerm::FNeg(x) => self.bind_atom(Atom::FNeg(x.clone()), dest, next),
            BlockedTerm::FAdd(x, y) => self.bind_atom(Atom::FAdd(x.clone(), y.clone()), dest, next),
            BlockedTerm::FSub(x, y) => self.bind_atom(Atom::FSub(x.clone(), y.clone()), dest, next),
            BlockedTerm::FMul(x, y) => self.bind_atom(Atom::FMul(x.clone(), y.clone()), dest, next),
            BlockedTerm::FDiv(x, y) => self.bind_atom(Atom::FDiv(x.clone(), y.clone()), dest, next),
            BlockedTerm::Var(x) => self.bind_atom(Atom::Var(x.clone()), dest, next),

            BlockedTerm::Tuple(xs) => self.bind_atom(Atom::Tuple(xs.clone()), dest, next),
            BlockedTerm::Get(x, y) => self.bind_atom(Atom::Get(x.clone(), y.clone()), dest, next),
            BlockedTerm::Put(x, y, z) => {
                self.bind_atom(Atom::Put(x.clone(), y.clone(), z.clone()), dest, next)
            }
            BlockedTerm::ExtArray(x) => self.bind_atom(Atom::ExtArray(x.clone()), dest, next),
            BlockedTerm::LoadLabel(l) => self.bind_atom(Atom::LoadLabel(l.clone()), dest, next),
            BlockedTerm::Push(x) => self.bind_atom(Atom::Push(x.clone()), dest, next),
            BlockedTerm::Pop(_x) => {
                if let Some((dest_x, _)) = &dest {
                    let _ = dest_x;
                }
                self.bind_atom(Atom::Pop, dest, next)
            }

            BlockedTerm::CallDir(l, args) => {
                self.bind_atom(Atom::CallDir(l.clone(), args.clone()), dest, next)
            }
            BlockedTerm::TailCallCls(x) => Term::JumpVar(x.clone()),
            BlockedTerm::TailCallBlock(l) => {
                if l == "print_int" || l == "min_caml_print_int" {
                    Term::CallExternal(l.clone())
                } else if l == "halt" {
                    Term::CallExternal(l.clone())
                } else {
                    Term::Jump(l.clone())
                }
            }
            BlockedTerm::TailCallDynamic(x) => {
                if let Some(l) = self.known_labels.get(x) {
                    if l == "print_int" || l == "min_caml_print_int" {
                        Term::CallExternal(l.clone())
                    } else if l == "halt" {
                        Term::CallExternal(l.clone())
                    } else {
                        Term::Jump(l.clone())
                    }
                } else {
                    Term::JumpVar(x.clone())
                }
            }
            BlockedTerm::Goto(l) => Term::Jump(l.clone()),
            BlockedTerm::JumpVar(x) => Term::JumpVar(x.clone()),
            BlockedTerm::IfEq(x, y, e1, e2) => {
                self.convert_if(x, y, e1, e2, dest, next, |x, y, c1, c2| {
                    Term::IfEq(x, y, c1, c2)
                })
            }
            BlockedTerm::IfLE(x, y, e1, e2) => {
                self.convert_if(x, y, e1, e2, dest, next, |x, y, c1, c2| {
                    Term::IfLE(x, y, c1, c2)
                })
            }
            BlockedTerm::Let((x, t), e1, e2) => match &**e1 {
                BlockedTerm::Let((y, ty), y_val, y_body) => {
                    let rest = BlockedTerm::Let((x.clone(), t.clone()), y_body.clone(), e2.clone());
                    let new_term =
                        BlockedTerm::Let((y.clone(), ty.clone()), y_val.clone(), Box::new(rest));
                    self.convert_term(&new_term, dest, next)
                }
                _ => {
                    if let Some(atom) = self.as_atom(e1) {
                        if let Atom::LoadLabel(ref l) = atom {
                            self.known_labels.insert(x.clone(), l.clone());
                        }
                        let term2 = self.convert_term(e2, dest, next);
                        Term::Let((x.clone(), t.clone()), atom, Box::new(term2))
                    } else {
                        panic!("Let e1 must be Atom in blocked IR, got: {:?}", e1);
                    }
                }
            },
            BlockedTerm::LetTuple(xts, y, e) => {
                let term2 = self.convert_term(e, dest, next);
                Term::LetTuple(xts.clone(), Atom::Var(y.clone()), Box::new(term2))
            }
        }
    }

    fn bind_atom(&self, atom: Atom, dest: Option<(id::T, Type)>, next: Option<id::L>) -> Term {
        match (dest, next) {
            (Some((x, t)), Some(next_l)) => Term::Let((x, t), atom, Box::new(Term::Jump(next_l))),
            (Some(_), None) => panic!("bind_atom: next label is None (Ret removed)"),
            (None, Some(next_l)) => {
                let dummy = id::gentmp(&Type::Unit);
                Term::Let((dummy, Type::Unit), atom, Box::new(Term::Jump(next_l)))
            }
            (None, None) => panic!("bind_atom: next label is None (Ret removed)"),
        }
    }

    fn convert_if<F>(
        &mut self,
        x: &id::T,
        y: &id::T,
        e1: &BlockedTerm,
        e2: &BlockedTerm,
        dest: Option<(id::T, Type)>,
        next: Option<id::L>,
        ctor: F,
    ) -> Term
    where
        F: FnOnce(id::T, id::T, id::L, id::L) -> Term,
    {
        let l_then = self.new_block_id();
        let l_else = self.new_block_id();

        let term_then = self.convert_term(e1, dest.clone(), next.clone());
        let term_else = self.convert_term(e2, dest, next);

        self.add_block(l_then.clone(), term_then);
        self.add_block(l_else.clone(), term_else);

        ctor(x.clone(), y.clone(), l_then, l_else)
    }

    fn as_atom(&self, term: &BlockedTerm) -> Option<Atom> {
        match term {
            BlockedTerm::Unit => Some(Atom::Unit),
            BlockedTerm::Int(i) => Some(Atom::Int(*i)),
            BlockedTerm::Float(f) => Some(Atom::Float(*f)),
            BlockedTerm::Neg(x) => Some(Atom::Neg(x.clone())),
            BlockedTerm::Add(x, y) => Some(Atom::Add(x.clone(), y.clone())),
            BlockedTerm::Sub(x, y) => Some(Atom::Sub(x.clone(), y.clone())),
            BlockedTerm::FNeg(x) => Some(Atom::FNeg(x.clone())),
            BlockedTerm::FAdd(x, y) => Some(Atom::FAdd(x.clone(), y.clone())),
            BlockedTerm::FSub(x, y) => Some(Atom::FSub(x.clone(), y.clone())),
            BlockedTerm::FMul(x, y) => Some(Atom::FMul(x.clone(), y.clone())),
            BlockedTerm::FDiv(x, y) => Some(Atom::FDiv(x.clone(), y.clone())),
            BlockedTerm::Var(x) => Some(Atom::Var(x.clone())),
            BlockedTerm::Get(x, y) => Some(Atom::Get(x.clone(), y.clone())),
            BlockedTerm::Put(x, y, z) => Some(Atom::Put(x.clone(), y.clone(), z.clone())),
            BlockedTerm::ExtArray(l) => Some(Atom::ExtArray(l.clone())),
            BlockedTerm::LoadLabel(l) => Some(Atom::LoadLabel(l.clone())),

            BlockedTerm::Push(x) => Some(Atom::Push(x.clone())),
            BlockedTerm::Pop(_) => Some(Atom::Pop),

            BlockedTerm::Tuple(xs) => Some(Atom::Tuple(xs.clone())),
            BlockedTerm::CallDir(l, args) => Some(Atom::CallDir(l.clone(), args.clone())),
            _ => None,
        }
    }
}

pub fn f(prog: &BlockedProg, _closure_prog: &ClosureProg) -> Prog {
    let mut func_arg_counts = HashMap::new();
    for (name, args) in &prog.functions {
        func_arg_counts.insert(name.clone(), args.len());
    }
    func_arg_counts.insert("main".to_string(), 0);
    let mut converter = Converter::new(func_arg_counts.clone());

    for block in &prog.blocks {
        converter
            .known_labels
            .insert(block.id.clone(), block.id.clone());
    }

    let entry_label = prog.entry.clone();

    for block in &prog.blocks {
        let term = converter.convert_term(&block.term, None, None);
        converter.add_block(block.id.clone(), term);
    }

    let layout = compute_layout(
        &converter.blocks,
        &func_arg_counts,
        &prog.functions,
        &entry_label,
    );

    Prog {
        blocks: converter.blocks,
        entry: entry_label,
        layout,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Layout {
    pub block_map: HashMap<id::L, usize>,
    pub var_map: HashMap<id::T, usize>,
    pub block_count: usize,
    pub var_count: usize,
    pub frame_sizes: HashMap<String, usize>,
}

fn compute_layout(
    blocks: &Vec<Block>,
    func_arg_counts: &HashMap<String, usize>,
    functions: &Vec<(String, Vec<id::T>)>,
    entry_label: &id::L,
) -> Layout {
    let mut block_map = HashMap::new();
    let mut var_map = HashMap::new();
    let mut block_count = 0;
    let mut frame_sizes = HashMap::new();

    block_map.insert(entry_label.clone(), 0);
    block_count += 1;

    for block in blocks {
        if !block_map.contains_key(&block.id) {
            block_map.insert(block.id.clone(), block_count);
            block_count += 1;
        }
    }

    let mut current_func_name = "main".to_string();
    let mut current_var_count = 0;

    let map_args = |func_name: &str, map: &mut HashMap<id::T, usize>, count: &mut usize| {
        if let Some((_, args)) = functions.iter().find(|(name, _)| name == func_name) {
            for arg in args {
                if !map.contains_key(arg) {
                    map.insert(arg.clone(), *count);
                    *count += 1;
                }
            }
        }
    };

    map_args(&current_func_name, &mut var_map, &mut current_var_count);
    for block in blocks {
        if func_arg_counts.contains_key(&block.id) {
            frame_sizes.insert(current_func_name.clone(), current_var_count);

            current_func_name = block.id.clone();
            map_args(&current_func_name, &mut var_map, &mut current_var_count);
        }

        collect_vars(&block.term, &mut var_map, &mut current_var_count);
    }
    frame_sizes.insert(current_func_name, current_var_count);
    let var_count = current_var_count + 1;
    Layout {
        block_map,
        var_map,
        block_count,
        var_count,
        frame_sizes,
    }
}

fn collect_vars(term: &Term, map: &mut HashMap<id::T, usize>, count: &mut usize) {
    match term {
        Term::Let((x, _), _, e) => {
            if !map.contains_key(x) {
                map.insert(x.clone(), *count);
                *count += 1;
            }
            collect_vars(e, map, count);
        }

        Term::LetTuple(xts, _, e) => {
            for (x, _) in xts {
                if !map.contains_key(x) {
                    map.insert(x.clone(), *count);
                    *count += 1;
                }
            }
            collect_vars(e, map, count);
        }
        Term::IfEq(_, _, _, _) | Term::IfLE(_, _, _, _) => {}
        _ => {}
    }
}

use std::fmt;

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:\n{}", self.id, indent_term(&self.term))
    }
}

fn indent_term(e: &Term) -> String {
    let s = format!("{}", e);
    s.lines()
        .map(|line| format!("  {}", line))
        .collect::<Vec<_>>()
        .join("\n")
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Atom::Unit => write!(f, "Unit"),
            Atom::Int(i) => write!(f, "{}", i),
            Atom::Float(d) => write!(f, "{:.6}", d),
            Atom::Neg(x) => write!(f, "-{}", x),
            Atom::Add(x, y) => write!(f, "{} + {}", x, y),
            Atom::Sub(x, y) => write!(f, "{} - {}", x, y),
            Atom::FNeg(x) => write!(f, "-.{}", x),
            Atom::FAdd(x, y) => write!(f, "{} +. {}", x, y),
            Atom::FSub(x, y) => write!(f, "{} -. {}", x, y),
            Atom::FMul(x, y) => write!(f, "{} *. {}", x, y),
            Atom::FDiv(x, y) => write!(f, "{} /. {}", x, y),
            Atom::Var(x) => write!(f, "{}", x),
            Atom::GetStack(i) => write!(f, "GetStack({})", i),

            Atom::Tuple(xs) => write!(f, "({:?})", xs),
            Atom::Get(x, y) => write!(f, "{}[{}]", x, y),
            Atom::Put(x, y, z) => write!(f, "{}[{}] = {}", x, y, z),
            Atom::ExtArray(l) => write!(f, "ExtArray({})", l),
            Atom::LoadLabel(l) => write!(f, "LoadLabel({})", l),
            Atom::Push(x) => write!(f, "Push({})", x),
            Atom::Pop => write!(f, "Pop"),

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
            Term::Atom(a) => write!(f, "{}", a),
            Term::IfEq(x, y, c1, c2) => write!(f, "IfEq({}, {}, Goto({}), Goto({}))", x, y, c1, c2),
            Term::IfLE(x, y, c1, c2) => write!(f, "IfLE({}, {}, Goto({}), Goto({}))", x, y, c1, c2),
            Term::Let((x, t), a, e) => write!(f, "Let ({}: {}) = {} in\n{}", x, t, a, e),
            Term::LetTuple(xts, a, e) => {
                let vars = xts
                    .iter()
                    .map(|(x, _)| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "LetTuple ({}) = {} in\n{}", vars, a, e)
            }
            Term::Jump(c) => write!(f, "Jump({})", c),
            Term::JumpVar(x) => write!(f, "JumpVar({})", x),
            Term::CallExternal(l) => write!(f, "CallExternal({})", l),
            Term::Ret(x) => write!(f, "Ret({})", x),
        }
    }
}

impl fmt::Display for Prog {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Entry: {}\n", self.entry)?;
        write!(f, "Block Count: {}\n", self.layout.block_count)?;
        write!(f, "Var Count: {}\n", self.layout.var_count)?;

        write!(f, "Block Map:\n")?;
        let mut sorted_blocks: Vec<_> = self.layout.block_map.iter().collect();
        sorted_blocks.sort_by_key(|k| k.1);
        for (id, idx) in sorted_blocks {
            write!(f, "  {}: {}\n", id, idx)?;
        }

        write!(f, "Var Map:\n")?;
        let mut sorted_vars: Vec<_> = self.layout.var_map.iter().collect();
        sorted_vars.sort_by_key(|k| k.1);
        for (id, idx) in sorted_vars {
            write!(f, "  {}: {}\n", id, idx)?;
        }

        for block in &self.blocks {
            write!(f, "{}\n", block)?;
        }
        Ok(())
    }
}
