use crate::cps::{self, Atom as CpsAtom, Prog as CpsProg, Term as CpsTerm};
use crate::id;
use crate::ty::Type;
use std::collections::HashMap;

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

impl Term {
    fn get_type(&self) -> Type {
        match self {
            Term::Unit => Type::Unit,
            Term::Int(_) | Term::Add(_, _) | Term::Sub(_, _) => Type::Int,
            Term::Float(_)
            | Term::Neg(_)
            | Term::FNeg(_)
            | Term::FAdd(_, _)
            | Term::FSub(_, _)
            | Term::FMul(_, _)
            | Term::FDiv(_, _) => Type::Float,
            Term::IfEq(_, _, e1, _) | Term::IfLE(_, _, e1, _) => e1.get_type(),
            Term::Let(_, _, e2) => e2.get_type(),
            Term::Var(_) => Type::Int,
            Term::TailCallCls(_) | Term::TailCallBlock(_) | Term::TailCallDynamic(_) => Type::Unit,
            Term::LoadLabel(_) => Type::Int,
            Term::SetArgs(_) => Type::Unit,
            Term::GetArg(_) | Term::GetEnv(_) => Type::Int,
            Term::Push(_) => Type::Unit,
            Term::Pop(_) => Type::Unit, // Pop returns Unit? No, it binds to a variable. But here `get_type` is for the term itself?
            // Actually `Let((x, t), Pop(..), ..)` uses `Pop` as the atom.
            // So `Pop` should return the type of value popped. Assume Int for now.
            // But `blocked::Term` includes things used in `Let` (Atom-like) and things that are expressions.
            // `Pop(id::T)` is an *instruction* `Pop to x`.
            // Wait, `Operation::Pop(dest)` in `virtual`.
            // Here `Term::Pop(id::T)` -> `Pop(x)`.
            // It modifies `x`. So it returns Unit.
            Term::GetSp(_) => Type::Int,
            Term::Tuple(_) => Type::Tuple(vec![]),
            Term::LetTuple(_, _, e) => e.get_type(),
            Term::Get(_, _) => Type::Int,
            Term::Put(_, _, _) => Type::Unit,
            Term::ExtArray(_) => Type::Array(Box::new(Type::Int)),
            Term::Goto(_) => Type::Unit,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Prog {
    pub blocks: Vec<Block>,
    pub entry: id::L,
}

struct Converter {
    blocks: Vec<Block>,
    closure_map: HashMap<id::T, (id::L, Vec<id::T>)>, // Var -> (Label, FVs)
    // New fields for Get optimization
    constants: HashMap<id::T, i32>,
    current_self: Option<(id::T, id::L, Vec<id::T>)>, // (SelfVar, FuncLabel, SortedFVs)
}

impl Converter {
    fn new(closure_map: HashMap<id::T, (id::L, Vec<id::T>)>) -> Self {
        Converter {
            blocks: Vec::new(),
            closure_map,
            constants: HashMap::new(),
            current_self: None,
        }
    }

    fn new_block_id(&self) -> id::L {
        id::genid("block")
    }

    fn add_block(&mut self, id: id::L, term: Term) {
        self.blocks.push(Block { id, term });
    }

    fn convert_term(&mut self, term: &CpsTerm) -> Term {
        match term {
            CpsTerm::Let((x, t), atom, e) => {
                if let CpsAtom::MakeCls(cls) = atom {
                    // MakeCls(cls) ->
                    // 1. Push(fv)... (Push FVs to Stack)
                    // 2. x = LoadLabel(entry) (Bind x to the Code Label)

                    // We DO NOT create a Tuple/Closure Object on Stack.
                    // We DO NOT GetSp.
                    // This creates a "Stack-Based Closure" where FVs are on Stack,
                    // and 'x' is just the Label to jump to.
                    // This works for One-Shot Continuations (Linear Stack consumption).

                    let mut push_ops = Vec::new();

                    // Push FVs
                    for fv in &cls.actual_fv {
                        push_ops.push(Term::Push(fv.clone()));
                    }

                    let body = self.convert_term(e);

                    // Sequence: push_ops -> Let x = LoadLabel -> body

                    let mut res = body;

                    // Prepend LoadLabel
                    res = Term::Let(
                        (x.clone(), t.clone()),
                        Box::new(Term::LoadLabel(cls.entry.clone())),
                        Box::new(res),
                    );

                    // Prepend Pushes
                    for op in push_ops.into_iter().rev() {
                        let dummy = id::gentmp(&Type::Unit);
                        res = Term::Let((dummy, Type::Unit), Box::new(op), Box::new(res));
                    }

                    return res;
                }

                // Track constants for Get optimization
                if let CpsAtom::Int(val) = atom {
                    self.constants.insert(x.clone(), *val);
                }

                let val = self.convert_atom(atom, x, t);
                let body = self.convert_term(e);
                Term::Let((x.clone(), t.clone()), Box::new(val), Box::new(body))
            }
            CpsTerm::LetTuple(xts, y, e) => {
                let body = self.convert_term(e);
                Term::LetTuple(xts.clone(), y.clone(), Box::new(body))
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
                // Flatten LetRec.
                let func_label = fundef.name.0.clone();
                // eprintln!("DEBUG: LetRec func={} args={:?}", func_label, fundef.args);
                // 1. Calculate FV - Use closure_map if available (authoritative source)
                // Search for any closure that uses this function as entry point
                let func_label_str = fundef.name.0.clone();
                let closure_fvs = self
                    .closure_map
                    .values()
                    .find(|(entry, _)| *entry == func_label_str);

                let mut sorted_fvs = if let Some((_, fvs)) = closure_fvs {
                    fvs.clone()
                } else {
                    // Fallback (e.g. not a closure, or optimization passed)
                    let mut body_fv = cps::fv(&fundef.body);
                    for (arg, _) in &fundef.args {
                        body_fv.remove(arg);
                    }
                    body_fv.remove(&fundef.name.0); // Remove recursive self
                    let mut sorted: Vec<_> = body_fv.into_iter().collect();
                    sorted.sort();
                    sorted
                };

                // Stack Protocol:
                // Caller: [Push FVs (Sorted)] -> [Push Args] -> Top
                // Callee: [Pop Args (Forward Iter)] -> [Pop FVs (Forward Iter)]
                // (Because wrapping creates inside-out execution order)

                // Set Current Self for body conversion
                // Assume Self is the LAST argument (standard mincaml/cps behavior)
                // We SKIP popping Self (as it wasn't pushed).

                let mut real_args = fundef.args.clone();
                let mut self_arg = None;

                if !real_args.is_empty() {
                    // Remove last arg (Self)
                    let last = real_args.pop().unwrap();
                    self_arg = Some(last.0);
                }

                // Update converter state
                let old_self = self.current_self.clone();
                if let Some(s) = &self_arg {
                    self.current_self = Some((s.clone(), func_label.clone(), sorted_fvs.clone()));
                } else {
                    self.current_self = None;
                }

                let func_body = self.convert_term(&fundef.body);

                // Restore state
                self.current_self = old_self;

                let mut wrapped_body = func_body;

                // 2. Prepend Pop for FVs (Forward)
                // Stack has: [FV1, FV2, ... FVN, Arg1, ... ArgM] (Top)
                // Wrapping Order: Loop A, B -> Let B ... Let A ...
                // Exec Order: Pop B (Top), Pop A.
                // We want to Pop Top first.
                // So Outermost Let should be ArgM.
                // So ArgM must be wrapped LAST.
                // So Args Loop must be LAST.

                // In FVs [FV1...FVN]. Top is FVN.
                // We want Let FVN... Let FV1.
                // So we Loop FV1...FVN.
                // 1. Wrap FV1. 2. Wrap FVN.
                // Result Let FVN ... Let FV1.
                // Exec: Pop FVN, Pop FV1. Matches.

                for fv in sorted_fvs.iter() {
                    wrapped_body = Term::Let(
                        (fv.clone(), Type::Int),
                        Box::new(Term::Pop(fv.clone())),
                        Box::new(wrapped_body),
                    );
                }

                // 3. Prepend Pop for Args (Forward)
                // Stack Top is ArgM.
                // We want Let ArgM ... Let Arg1.
                // Loop 1..M.
                // Wrap 1. Wrap M.
                // Result Let M ... Let 1.
                // Exec Pop M ... Pop 1. Matches.

                for (arg, ty) in real_args.iter() {
                    wrapped_body = Term::Let(
                        (arg.clone(), ty.clone()),
                        Box::new(Term::Pop(arg.clone())),
                        Box::new(wrapped_body),
                    );
                }

                self.add_block(func_label.clone(), wrapped_body);

                // 4. Generate MakeCls code (Push FVs, Bind Label)

                // 2. Generate MakeCls code (Push FVs, Bind Label)
                let mut push_ops = Vec::new();

                // Push FVs
                for fv in sorted_fvs {
                    push_ops.push(Term::Push(fv.clone()));
                }

                // 3. Bind Func Name to Label
                let rest = self.convert_term(e);

                let mut res = rest;

                // Bind Label
                res = Term::Let(
                    fundef.name.clone(),
                    Box::new(Term::LoadLabel(fundef.name.0.clone())),
                    Box::new(res),
                );

                // Prepend Pushes
                for op in push_ops.into_iter().rev() {
                    match op {
                        Term::Push(var) => {
                            let dummy = id::gentmp(&Type::Unit);
                            res = Term::Let(
                                (dummy, Type::Unit),
                                Box::new(Term::Push(var)),
                                Box::new(res),
                            );
                        }
                        _ => panic!("Unexpected op"),
                    }
                }

                res
            }
            CpsTerm::AppCls(f, args) => {
                // AppCls(f, args)
                // f is the Label.
                // args are arguments.
                // 1. Push args...
                // 2. JumpVar(f) (TailCallDynamic)

                // NO Push Self. FVs are already on stack (from MakeCls).
                // NO Get(f, 0). f IS the label.

                let mut push_ops = Vec::new();
                for arg in args {
                    push_ops.push(Term::Push(arg.clone()));
                }

                let call = Term::TailCallDynamic(f.clone());

                let mut res = call;
                for op in push_ops.into_iter().rev() {
                    let dummy = id::gentmp(&Type::Unit);
                    res = Term::Let((dummy, Type::Unit), Box::new(op), Box::new(res));
                }

                res
            }
            CpsTerm::AppDir(l, args) => {
                // AppDir(l, args)
                // Just Push args and Jump.
                // NO Dummy Self. FVs (if any) are assumed on Stack (if recursive) or Empty (global).

                let mut push_ops = Vec::new();
                for arg in args {
                    push_ops.push(Term::Push(arg.clone()));
                }

                let call = Term::TailCallBlock(l.clone());

                let mut res = call;
                for op in push_ops.into_iter().rev() {
                    let dummy = id::gentmp(&Type::Unit);
                    res = Term::Let((dummy, Type::Unit), Box::new(op), Box::new(res));
                }

                res
            }
        }
    }

    fn convert_atom(&self, atom: &CpsAtom, _dest_x: &id::T, _dest_t: &Type) -> Term {
        match atom {
            CpsAtom::Unit => Term::Unit,
            CpsAtom::Int(i) => Term::Int(*i),
            CpsAtom::Float(d) => Term::Float(*d),
            CpsAtom::Var(x) => Term::Var(x.clone()),
            CpsAtom::Neg(x) => Term::Neg(x.clone()),
            CpsAtom::Add(x, y) => Term::Add(x.clone(), y.clone()),
            CpsAtom::Sub(x, y) => Term::Sub(x.clone(), y.clone()),
            CpsAtom::FNeg(x) => Term::FNeg(x.clone()),
            CpsAtom::FAdd(x, y) => Term::FAdd(x.clone(), y.clone()),
            CpsAtom::FSub(x, y) => Term::FSub(x.clone(), y.clone()),
            CpsAtom::FMul(x, y) => Term::FMul(x.clone(), y.clone()),
            CpsAtom::FDiv(x, y) => Term::FDiv(x.clone(), y.clone()),
            CpsAtom::Get(x, y) => {
                // Optimization: If x is Self, resolve to FV Variable
                eprintln!(
                    "DEBUG: convert_atom Get({}, {}) self={:?}",
                    x, y, self.current_self
                );
                if let Some((self_var, func_label, fvs)) = &self.current_self {
                    if x == self_var {
                        // Resolve y value
                        if let Some(idx) = self.constants.get(y) {
                            // MinCaml closure layout: [CodePtr, FV1, FV2...]
                            // index 0 is CodePtr.
                            // fvs list corresponds to 1, 2, ...
                            // So array index i maps to fvs[i-1].
                            let i = *idx as usize;
                            if i == 0 {
                                // Accessing Code Pointer -> Return Label
                                return Term::LoadLabel(func_label.clone());
                            } else if i > 0 && i <= fvs.len() {
                                return Term::Var(fvs[i - 1].clone());
                            } else {
                                // Out of bounds.
                            }
                        }
                    }
                }
                Term::Get(x.clone(), y.clone())
            }
            CpsAtom::Put(x, y, z) => Term::Put(x.clone(), y.clone(), z.clone()),
            CpsAtom::ExtArray(x) => Term::ExtArray(x.clone()),
            CpsAtom::Tuple(xs) => Term::Tuple(xs.clone()),
            CpsAtom::MakeCls(_) => panic!("MakeCls should be handled in convert_term"),
        }
    }
}

fn scan_make_cls(term: &CpsTerm, map: &mut HashMap<id::T, (id::L, Vec<id::T>)>) {
    match term {
        CpsTerm::Let((x, _), atom, e) => {
            if let CpsAtom::MakeCls(cls) = atom {
                map.insert(x.clone(), (cls.entry.clone(), cls.actual_fv.clone()));
            }
            scan_make_cls(e, map);
        }
        CpsTerm::LetTuple(_, _, e) => scan_make_cls(e, map),
        CpsTerm::IfEq(_, _, e1, e2) | CpsTerm::IfLE(_, _, e1, e2) => {
            scan_make_cls(e1, map);
            scan_make_cls(e2, map);
        }
        CpsTerm::LetRec(fundef, e) => {
            scan_make_cls(&fundef.body, map);
            scan_make_cls(e, map);
        }
        _ => {}
    }
}

pub fn f(prog: &CpsProg) -> Prog {
    // 1. Scan for MakeCls
    let mut closure_map = HashMap::new();
    scan_make_cls(&prog.body, &mut closure_map);
    for fundef in &prog.fundefs {
        scan_make_cls(&fundef.body, &mut closure_map);
    }

    let mut converter = Converter::new(closure_map);

    // Convert main body
    let entry_label = "main".to_string();
    let main_term = converter.convert_term(&prog.body);
    converter.add_block(entry_label.clone(), main_term);

    // Convert functions
    for fundef in &prog.fundefs {
        let func_label = fundef.name.0.clone();
        let mut func_term = converter.convert_term(&fundef.body);

        // Prepend loading of free variables (if any, but CPS fundefs from closure don't have formal_fv in CPS struct?)
        // Wait, cps::Fundef doesn't have formal_fv.
        // But closure::Fundef had it.
        // cps::f preserved args but didn't preserve formal_fv in cps::Fundef?
        // Let's check cps::Fundef.
        /*
        pub struct Fundef {
            pub name: (id::T, Type),
            pub args: Vec<(id::T, Type)>,
            pub body: Box<Term>,
        }
        */
        // It lost formal_fv!
        // This is a problem. We need formal_fv to emit GetEnv.
        // I should update cps::Fundef to include formal_fv.
        // Or I can look it up from closure::Prog if I pass it to blocked::f?
        // But blocked::f takes CpsProg.
        // So CpsProg should carry this info.

        // I will assume for now that I need to update cps::Fundef.
        // But for this step, I will just emit GetArg.
        // If formal_fv is missing, we can't emit GetEnv.

        // Prepend loading of arguments
        // 1. Strip implicit Self argument (last arg)
        let mut real_args = fundef.args.clone();
        if !real_args.is_empty() {
            real_args.pop(); // Remove Self
        }

        // 2. Pop remaining arguments in REVERSE order (Stack is LIFO)
        for (arg, ty) in real_args.into_iter().rev() {
            func_term = Term::Let(
                (arg.clone(), ty.clone()),
                Box::new(Term::Pop(arg.clone())),
                Box::new(func_term),
            );
        }

        converter.add_block(func_label, func_term);
    }

    Prog {
        blocks: converter.blocks,
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
