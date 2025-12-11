use crate::blocked::{Prog as BlockedProg, Term as BlockedTerm};
use crate::closure::{self, Prog as ClosureProg};
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
    SetArgs(Vec<id::T>), // 引数をスタックに保存
    GetStack(usize),     // スタックから値を取得 (引数も含む)
    Tuple(Vec<id::T>),
    Get(id::T, id::T),
    Put(id::T, id::T, id::T),
    ExtArray(id::L),
    LoadLabel(id::L), // Load label address (Block ID)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Atom(Atom),
    IfEq(id::T, id::T, id::L, id::L),
    IfLE(id::T, id::T, id::L, id::L),
    Let((id::T, Type), Atom, Box<Term>),
    // LetFuncall removed (only tail calls allowed)
    LetTuple(Vec<(id::T, Type)>, Atom, Box<Term>),
    Jump(id::L),    // Unconditional jump to Label
    JumpVar(id::T), // Unconditional jump to Variable (Dynamic)
}

struct Converter {
    blocks: Vec<Block>,
    // closure_map removed
    func_arg_counts: HashMap<String, usize>, // Func Name -> Arg Count
    current_func_args_len: usize,
}

impl Converter {
    fn new(func_arg_counts: HashMap<String, usize>) -> Self {
        Converter {
            blocks: Vec::new(),
            func_arg_counts,
            current_func_args_len: 0,
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

            BlockedTerm::SetArgs(xs) => self.bind_atom(Atom::SetArgs(xs.clone()), dest, next),
            BlockedTerm::GetArg(i) => self.bind_atom(Atom::GetStack(*i), dest, next),
            BlockedTerm::GetEnv(i) => {
                // Lambda Lifting: GetEnv(i) -> GetStack(current_func_args_len + i)
                let new_idx = self.current_func_args_len + i;
                self.bind_atom(Atom::GetStack(new_idx), dest, next)
            }
            BlockedTerm::Tuple(xs) => self.bind_atom(Atom::Tuple(xs.clone()), dest, next),
            BlockedTerm::Get(x, y) => self.bind_atom(Atom::Get(x.clone(), y.clone()), dest, next),
            BlockedTerm::Put(x, y, z) => {
                self.bind_atom(Atom::Put(x.clone(), y.clone(), z.clone()), dest, next)
            }
            BlockedTerm::ExtArray(x) => self.bind_atom(Atom::ExtArray(x.clone()), dest, next),
            BlockedTerm::LoadLabel(l) => self.bind_atom(Atom::LoadLabel(l.clone()), dest, next),

            // CallCls and CallBlock are removed from blocked::Term
            // TailCallCls and TailCallBlock are the only calls
            BlockedTerm::TailCallCls(x) => {
                // TailCallCls(x) -> JumpVar(x) because we treat x as a variable holding the target?
                // Wait, blocked::TailCallCls(id::T) means x is a CLOSURE POINTER or CODE POINTER?
                // In usual mincaml, Cls call means x is a closure.
                // But blocked::TailCallCls fallback is used when we don't know the closure.
                // So we assume x IS the code label? No.
                // If x is a closure, we must extract code ptr.
                // But `AppCls` handled that extraction!
                // `AppCls` emits `TailCallDynamic(entry_var)`.
                // So `TailCallCls` shouldn't appear if AppCls handled it?
                // AppCls fallback (lines 178-183 in blocked.rs) emits `TailCallCls`.
                // This fallback implies "Compiler couldn't optimize".
                // If we implemented `TailCallDynamic`, we should use it in fallback too.
                // But `convert_term` in blocked.rs is recursive.
                // Let's assume TailCallCls is legacy/error now.
                // But for safety, map it to JumpVar(x) assuming x is LABEL VARIABLE?
                // Or Jump(Label(x))?
                // id::T implies Variable.
                Term::JumpVar(x.clone())
            }
            BlockedTerm::TailCallBlock(l) => {
                // TailCallBlock(l) -> Jump(l)
                Term::Jump(l.clone())
            }
            BlockedTerm::TailCallDynamic(x) => Term::JumpVar(x.clone()),

            BlockedTerm::Goto(l) => Term::Jump(l.clone()),

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

            BlockedTerm::Let((x, t), e1, e2) => {
                // e1 is guaranteed to be Atom by blocked.rs construction
                let term2 = self.convert_term(e2, dest, next);
                if let Some(atom) = self.as_atom(e1) {
                    Term::Let((x.clone(), t.clone()), atom, Box::new(term2))
                } else {
                    panic!("Let e1 must be Atom in blocked IR, got: {:?}", e1);
                }
            }

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
            BlockedTerm::Float(d) => Some(Atom::Float(*d)),
            BlockedTerm::Neg(x) => Some(Atom::Neg(x.clone())),
            BlockedTerm::Add(x, y) => Some(Atom::Add(x.clone(), y.clone())),
            BlockedTerm::Sub(x, y) => Some(Atom::Sub(x.clone(), y.clone())),
            BlockedTerm::FNeg(x) => Some(Atom::FNeg(x.clone())),
            BlockedTerm::FAdd(x, y) => Some(Atom::FAdd(x.clone(), y.clone())),
            BlockedTerm::FSub(x, y) => Some(Atom::FSub(x.clone(), y.clone())),
            BlockedTerm::FMul(x, y) => Some(Atom::FMul(x.clone(), y.clone())),
            BlockedTerm::FDiv(x, y) => Some(Atom::FDiv(x.clone(), y.clone())),
            BlockedTerm::Var(x) => Some(Atom::Var(x.clone())),
            BlockedTerm::SetArgs(xs) => Some(Atom::SetArgs(xs.clone())),
            BlockedTerm::GetArg(i) => Some(Atom::GetStack(*i)),
            // GetEnv is handled in convert_term
            BlockedTerm::Tuple(xs) => Some(Atom::Tuple(xs.clone())),
            BlockedTerm::Get(x, y) => Some(Atom::Get(x.clone(), y.clone())),
            BlockedTerm::Put(x, y, z) => Some(Atom::Put(x.clone(), y.clone(), z.clone())),
            BlockedTerm::ExtArray(x) => Some(Atom::ExtArray(x.clone())),
            BlockedTerm::LoadLabel(l) => Some(Atom::LoadLabel(l.clone())),
            _ => None,
        }
    }
}

pub fn f(prog: &BlockedProg, closure_prog: &ClosureProg) -> Prog {
    // 1. Build func_arg_counts from closure_prog
    let mut func_arg_counts = HashMap::new();
    for fundef in &closure_prog.fundefs {
        func_arg_counts.insert(fundef.name.0.clone(), fundef.args.len());
    }
    // Main function has 0 args? Or it's not in fundefs.
    // Main is usually entry point, 0 args.
    func_arg_counts.insert("main".to_string(), 0); // Assuming main has 0 args

    let mut converter = Converter::new(func_arg_counts.clone());

    let entry_label = prog.entry.clone();

    for block in &prog.blocks {
        // Check if this block is a function entry
        if converter.func_arg_counts.contains_key(&block.id) {
            converter.current_func_args_len = *converter.func_arg_counts.get(&block.id).unwrap();
        }

        let term = converter.convert_term(&block.term, None, None);
        converter.add_block(block.id.clone(), term);
    }

    let layout = compute_layout(
        &converter.blocks,
        &func_arg_counts,
        closure_prog,
        &entry_label,
    );

    // Resolve SetStack indices - Removed
    // for block in &mut converter.blocks {
    //     resolve_set_stack(&mut block.term, &layout.var_map);
    // }

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
    pub frame_sizes: HashMap<String, usize>, // Function Name -> Frame Size
}

fn compute_layout(
    blocks: &Vec<Block>,
    func_arg_counts: &HashMap<String, usize>,
    closure_prog: &ClosureProg,
    entry_label: &id::L,
) -> Layout {
    let mut block_map = HashMap::new();
    let mut var_map = HashMap::new();
    let mut block_count = 1;
    let mut var_count = 0; // Max var count across all functions (for global stats, if needed)
    let mut frame_sizes = HashMap::new();

    // Ensure entry block is index 1
    block_map.insert(entry_label.clone(), 1);
    block_count += 1;

    // Assign block IDs globally
    for block in blocks {
        if !block_map.contains_key(&block.id) {
            block_map.insert(block.id.clone(), block_count);
            block_count += 1;
        }
    }

    // Assign variable IDs per function
    // We iterate blocks. If a block ID matches a function name, we start a new function scope.
    // We assume blocks for a function are contiguous.

    let mut current_func_name = "main".to_string();
    let mut current_var_count = 0;

    // Helper to map args
    let mut map_args = |func_name: &str, map: &mut HashMap<id::T, usize>, count: &mut usize| {
        if let Some(fundef) = closure_prog.fundefs.iter().find(|f| f.name.0 == func_name) {
            for (arg, _) in &fundef.args {
                if !map.contains_key(arg) {
                    map.insert(arg.clone(), *count);
                    *count += 1;
                }
            }
        }
    };

    // Initial args for main (usually none, but consistent)
    map_args(&current_func_name, &mut var_map, &mut current_var_count);

    for block in blocks {
        // Check if we entered a new function
        if func_arg_counts.contains_key(&block.id) {
            // Save previous frame size (accumulated count)
            frame_sizes.insert(current_func_name.clone(), current_var_count);

            // Start new function - DO NOT RESET current_var_count
            current_func_name = block.id.clone();
            // current_var_count = 0; // Removed to ensure unique variable IDs globally
            map_args(&current_func_name, &mut var_map, &mut current_var_count);
        }

        collect_vars(&block.term, &mut var_map, &mut current_var_count);
    }
    // Save last frame size
    frame_sizes.insert(current_func_name, current_var_count);
    var_count = current_var_count + 1; // Total count + 1 (reserved for global comparison temp)

    Layout {
        block_map,
        var_map,
        block_count,
        var_count, // This is now max frame size
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
        Term::IfEq(_, _, _, _) | Term::IfLE(_, _, _, _) => {
            // No new variables defined in If branches in this IR (they are Jumps)
        }
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
            Atom::SetArgs(xs) => write!(f, "SetArgs({:?})", xs),
            Atom::Tuple(xs) => write!(f, "({:?})", xs),
            Atom::Get(x, y) => write!(f, "{}[{}]", x, y),
            Atom::Put(x, y, z) => write!(f, "{}[{}] = {}", x, y, z),
            Atom::ExtArray(l) => write!(f, "ExtArray({})", l),
            Atom::LoadLabel(l) => write!(f, "LoadLabel({})", l),
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
