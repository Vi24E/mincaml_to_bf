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
    // MakeCls removed
    // CallCls removed (integrated into CallBlock/TailCallBlock or handled via Dispatcher if needed)
    // For now, we assume all calls are optimized or we panic/use a placeholder if not.
    // But to be safe, let's keep CallCls/TailCallCls for unknown functions if we don't implement dispatcher yet.
    // The user said "TailCallCls also integrated into TailCallBlock".
    // This implies we should use TailCallBlock for everything.
    // If it's a variable, we might need TailCallBlock(Var)? No, Block takes Label.
    // So we probably need TailCallDynamic(id::T)?
    // Or we stick to the user's request and try to remove TailCallCls.
    // If we remove it, we must ensure all calls are to labels.
    // This is true if we have a dispatcher.
    // For this step, I will keep TailCallCls but try not to use it for known closures.
    // CallCls and CallBlock removed (only tail calls allowed)
    TailCallCls(id::T),
    TailCallBlock(id::L),
    TailCallDynamic(id::T), // Call entry point stored in variable
    LoadLabel(id::L),       // Load label address into variable
    SetArgs(Vec<id::T>),    // Legacy: Will be removed or mapped to Push loops
    GetArg(usize),          // Legacy: Will be removed or mapped to Pop
    GetEnv(usize),          // Legacy
    Push(id::T),
    Pop(id::T),
    GetSp(id::T), // dest = SP
    Tuple(Vec<id::T>),
    LetTuple(Vec<(id::T, Type)>, id::T, Box<Term>),
    Get(id::T, id::T),
    Put(id::T, id::T, id::T),
    ExtArray(id::L),
    Goto(id::L),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Prog {
    pub blocks: Vec<Block>,
    pub entry: id::L,
}

struct Converter {
    blocks: Vec<(String, Term)>,
    #[allow(dead_code)]
    papp_counts: HashMap<String, HashSet<usize>>,
}

impl Converter {
    fn new(papp_counts: HashMap<String, HashSet<usize>>) -> Self {
        Converter {
            blocks: Vec::new(),
            papp_counts,
        }
    }

    fn add_block(&mut self, label: String, term: Term) {
        self.blocks.push((label, term));
    }

    fn convert_fundef(&mut self, fundef: &cps::Fundef) {
        let func_label = fundef.name.0.clone();
        let counts = self
            .papp_counts
            .get(&func_label)
            .cloned()
            .unwrap_or_default();

        // 1. Generate Main Body
        let body_term = self.convert_term(&fundef.body);

        let mut main_func_body = body_term;
        // Pop Args Logic (Reverse of Push)
        // Stack: [FV... Args...]. Top is ArgN.
        // Pop ArgN .. Arg1 .. FVs.
        // Pop Args Logic (Reverse of Push)
        // Stack: [FV... Args...]. Top is ArgN.
        // Pop ArgN .. Arg1 .. FVs.
        for (arg, ty) in fundef.args.iter() {
            main_func_body = Term::Let(
                (arg.clone(), ty.clone()),
                Box::new(Term::Pop(arg.clone())),
                Box::new(main_func_body),
            );
        }

        self.add_block(func_label.clone(), main_func_body);

        // 2. Generate Aliases (Trampolines)
        for count in counts {
            let alias_label = format!("{}_{}", func_label, count);
            self.add_block(alias_label, Term::TailCallBlock(func_label.clone()));
        }
    }

    fn convert_term(&mut self, term: &CpsTerm) -> Term {
        match term {
            CpsTerm::Let((x, t), atom, e) => {
                if let CpsAtom::PApp(l, fvs) = atom {
                    // PApp logic: Push FVs, Label = l_N.
                    let count = fvs.len();
                    let target_label = format!("{}_{}", l, count);

                    let mut res = self.convert_term(e);

                    let label_tmp = id::gentmp(&Type::Int);
                    res = Term::Let(
                        (x.clone(), t.clone()),
                        Box::new(Term::Var(label_tmp.clone())),
                        Box::new(res),
                    );
                    res = Term::Let(
                        (label_tmp, Type::Int),
                        Box::new(Term::LoadLabel(target_label)),
                        Box::new(res),
                    );

                    // Push FVs (Iterate Reverse -> Execution Order: Push FV1, Push FV2...)
                    // Push FVs (Iterate Reverse -> Execution Order: Push FV1, Push FV2...)
                    for fv in fvs.iter().rev() {
                        let dummy = id::gentmp(&Type::Unit);
                        res = Term::Let(
                            (dummy, Type::Unit),
                            Box::new(Term::Push(fv.clone())),
                            Box::new(res),
                        );
                    }
                    res
                } else if let Some(a) = self.try_atomic(atom, x, t) {
                    let next = self.convert_term(e);
                    Term::Let((x.clone(), t.clone()), Box::new(a), Box::new(next))
                } else {
                    let next_term = self.convert_term(e);
                    self.bind_atom(atom.clone(), x.clone(), next_term)
                }
            }
            CpsTerm::LetTuple(xts, y, e) => {
                let next = self.convert_term(e);
                Term::LetTuple(xts.clone(), y.clone(), Box::new(next))
            }
            CpsTerm::IfEq(x, y, e1, e2) => {
                let t1 = self.convert_term(e1);
                let t2 = self.convert_term(e2);
                Term::IfEq(x.clone(), y.clone(), Box::new(t1), Box::new(t2))
            }
            CpsTerm::IfLE(x, y, e1, e2) => {
                let t1 = self.convert_term(e1);
                let t2 = self.convert_term(e2);
                Term::IfLE(x.clone(), y.clone(), Box::new(t1), Box::new(t2))
            }
            CpsTerm::LetRec(fundef, e) => {
                self.convert_fundef(fundef);
                self.convert_term(e)
            }
            CpsTerm::AppCls(f, args) => {
                // AppCls Logic: Push Args -> TailCallDynamic(f)
                // Note: f is the label variable (loaded by PApp or passed as argument).
                // Just use TailCallDynamic.

                let target = f.clone();
                let jump_term = Term::TailCallDynamic(target);

                let mut res = jump_term;
                // Push Args (Reverse Iter -> Forward Push)
                // Stack Result: [Previous Stack] [Arg1] [Arg2] ...
                // Top is Last Arg.

                for arg in args.iter().rev() {
                    let dummy = id::gentmp(&Type::Unit);
                    res = Term::Let(
                        (dummy, Type::Unit),
                        Box::new(Term::Push(arg.clone())),
                        Box::new(res),
                    );
                }
                res
            }
            CpsTerm::AppDir(l, args) => {
                let len = args.len();
                if len == 1 && l == "halt" {
                    let arg0 = &args[0];
                    return Term::Let(
                        (id::gentmp(&Type::Unit), Type::Unit),
                        Box::new(Term::Push(arg0.clone())),
                        Box::new(Term::TailCallBlock("halt".to_string())),
                    );
                }

                let jump_term = Term::TailCallBlock(l.clone());
                let mut res = jump_term;
                for arg in args.iter().rev() {
                    let dummy = id::gentmp(&Type::Unit);
                    res = Term::Let(
                        (dummy, Type::Unit),
                        Box::new(Term::Push(arg.clone())),
                        Box::new(res),
                    );
                }
                res
            }
        }
    }

    fn try_atomic(&self, atom: &CpsAtom, _x: &id::T, _t: &Type) -> Option<Term> {
        match atom {
            CpsAtom::Unit => Some(Term::Unit),
            CpsAtom::Int(i) => Some(Term::Int(*i)),
            CpsAtom::Float(d) => Some(Term::Float(*d)),
            _ => None,
        }
    }

    fn bind_atom(&mut self, atom: CpsAtom, dest: id::T, next: Term) -> Term {
        let val = match atom {
            CpsAtom::Unit => Term::Unit,
            CpsAtom::Int(i) => Term::Int(i),
            CpsAtom::Float(d) => Term::Float(d),
            CpsAtom::Var(x) => Term::Var(x),
            CpsAtom::Neg(x) => Term::Neg(x),
            CpsAtom::Add(x, y) => Term::Add(x, y),
            CpsAtom::Sub(x, y) => Term::Sub(x, y),
            CpsAtom::FNeg(x) => Term::FNeg(x),
            CpsAtom::FAdd(x, y) => Term::FAdd(x, y),
            CpsAtom::FSub(x, y) => Term::FSub(x, y),
            CpsAtom::FMul(x, y) => Term::FMul(x, y),
            CpsAtom::FDiv(x, y) => Term::FDiv(x, y),
            CpsAtom::Get(x, y) => Term::Get(x, y),
            CpsAtom::Put(x, y, z) => Term::Put(x, y, z),
            CpsAtom::ExtArray(x) => Term::ExtArray(x),
            CpsAtom::Tuple(xs) => Term::Tuple(xs),
            CpsAtom::PApp(_, _) => panic!("PApp in bind_atom"),
        };
        Term::Let((dest, Type::Int), Box::new(val), Box::new(next))
    }
}

// Imports removed

// Helper to scan for PApp usages and collect arg counts per function
fn scan_papp(term: &CpsTerm, map: &mut HashMap<String, HashSet<usize>>) {
    match term {
        CpsTerm::Let((_, _), atom, e) => {
            if let CpsAtom::PApp(l, xs) = atom {
                // l is label, xs is args.
                // We use xs.len() as the count.
                // But l might be a variable?
                // cps.rs PApp(id::L, ...) uses Label.
                // So l is String (Label).
                map.entry(l.clone()).or_default().insert(xs.len());
            }
            scan_papp(e, map);
        }
        CpsTerm::LetTuple(_, _, e) => scan_papp(e, map),
        CpsTerm::IfEq(_, _, e1, e2) | CpsTerm::IfLE(_, _, e1, e2) => {
            scan_papp(e1, map);
            scan_papp(e2, map);
        }
        CpsTerm::LetRec(fundef, e) => {
            scan_papp(&fundef.body, map);
            scan_papp(e, map);
        }
        _ => {}
    }
}

fn scan_arg_counts(term: &CpsTerm, map: &mut HashMap<String, usize>) {
    match term {
        CpsTerm::Let(_, _, e) | CpsTerm::LetTuple(_, _, e) => scan_arg_counts(e, map),
        CpsTerm::IfEq(_, _, e1, e2) | CpsTerm::IfLE(_, _, e1, e2) => {
            scan_arg_counts(e1, map);
            scan_arg_counts(e2, map);
        }
        CpsTerm::LetRec(fundef, e) => {
            map.insert(fundef.name.0.clone(), fundef.args.len());
            scan_arg_counts(&fundef.body, map);
            scan_arg_counts(e, map);
        }
        _ => {}
    }
}

pub fn f(prog: &CpsProg) -> Prog {
    let mut papp_counts = HashMap::new();
    scan_papp(&prog.body, &mut papp_counts);
    for fundef in &prog.fundefs {
        scan_papp(&fundef.body, &mut papp_counts);
    }

    let mut func_arg_counts = HashMap::new();
    for fundef in &prog.fundefs {
        func_arg_counts.insert(fundef.name.0.clone(), fundef.args.len());
        scan_arg_counts(&fundef.body, &mut func_arg_counts);
    }
    scan_arg_counts(&prog.body, &mut func_arg_counts);

    let mut converter = Converter::new(papp_counts);

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
            Term::SetArgs(xs) => {
                let args_str = xs.join(", ");
                write!(f, "SetArgs({})", args_str)
            }
            Term::GetArg(i) => write!(f, "GetArg({})", i),
            Term::GetEnv(i) => write!(f, "GetEnv({})", i),
            Term::Push(x) => write!(f, "Push({})", x),
            Term::Pop(x) => write!(f, "Pop({})", x),
            Term::GetSp(x) => write!(f, "GetSp({})", x),
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
