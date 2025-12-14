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
    func_arg_counts: HashMap<id::L, usize>,           // Function Label -> Arg Count
}

impl Converter {
    fn new(
        closure_map: HashMap<id::T, (id::L, Vec<id::T>)>,
        func_arg_counts: HashMap<id::L, usize>,
    ) -> Self {
        Converter {
            blocks: Vec::new(),
            closure_map,
            constants: HashMap::new(),
            current_self: None,
            func_arg_counts,
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
                    // 1. Let label = LoadLabel(entry)
                    // 2. Let tuple = Tuple([label, fvs...])
                    // 3. Let x = tuple (Bind x to the Tuple Pointer)

                    let label_var = id::gentmp(&Type::Int);
                    let tuple_var = id::gentmp(&Type::Int); // Fallback type

                    eprintln!(
                        "DEBUG: MakeCls binding {} to tuple {} entry: {}",
                        x, tuple_var, cls.entry
                    );

                    let mut tuple_elems = Vec::new();
                    tuple_elems.push(label_var.clone());
                    for fv in &cls.actual_fv {
                        tuple_elems.push(fv.clone());
                    }

                    let body = self.convert_term(e);
                    let mut res = body;

                    // Let x = tuple_var
                    res = Term::Let(
                        (x.clone(), t.clone()),
                        Box::new(Term::Var(tuple_var.clone())),
                        Box::new(res),
                    );

                    // Let tuple_var = Tuple(...)
                    res = Term::Let(
                        (tuple_var.clone(), Type::Int),
                        Box::new(Term::Tuple(tuple_elems)),
                        Box::new(res),
                    );

                    // Let label_var = LoadLabel(...)
                    res = Term::Let(
                        (label_var.clone(), Type::Int),
                        Box::new(Term::LoadLabel(cls.entry.clone())),
                        Box::new(res),
                    );

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
                if func_label.contains("fib") {
                    eprintln!("DEBUG: LetRec {} args: {:?}", func_label, fundef.args);
                    eprintln!("DEBUG: LetRec {} body: {:?}", func_label, fundef.body);
                }
                eprintln!("DEBUG: LetRec {} args: {:?}", func_label, fundef.args);
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
                    // Fallback: If not found in closure_map, it implies no MakeCls was emitted.
                    // This creates a "Known Function" (Label) optimized by mincaml.
                    // It accesses FVs from the environment (registers/memory) directly.
                    // We must NOT generate Pop FVs, as the caller (AppDir/JumpVar without MakeCls) didn't push them.
                    Vec::new()
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
                    self_arg = Some(last);
                }

                // Update converter state
                let old_self = self.current_self.clone();
                if let Some((s, _)) = &self_arg {
                    self.current_self = Some((s.clone(), func_label.clone(), sorted_fvs.clone()));
                } else {
                    self.current_self = None;
                }

                let func_body = self.convert_term(&fundef.body);

                // Restore state
                self.current_self = old_self;

                let mut wrapped_body = func_body;

                // 2. Prepend Pop for Self (Execute FIRST, so Wrap INNERMOST)
                // Stack Top (after Args popped) is Self.
                if let Some((s, ty)) = self_arg {
                    wrapped_body = Term::Let(
                        (s.clone(), ty),
                        Box::new(Term::Pop(s)),
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
                // closure 'f' is a Heap Tuple: [Entry, FVs...]
                // 1. Let label = Get(f, 0)
                // 2. Push args
                // 3. Push f (Self)
                // 4. TailCall(label)

                let label_var = id::gentmp(&Type::Int);
                let zero_var = id::gentmp(&Type::Int);

                let mut push_ops = Vec::new();
                for arg in args {
                    push_ops.push(Term::Push(arg.clone()));
                }
                // Explicitly Push Self (Tuple Pointer)
                push_ops.push(Term::Push(f.clone()));

                let call = Term::TailCallDynamic(label_var.clone());
                let mut res = call;

                // Wrap Pushes (Args + Self)
                for op in push_ops.into_iter().rev() {
                    let dummy = id::gentmp(&Type::Unit);
                    res = Term::Let((dummy, Type::Unit), Box::new(op), Box::new(res));
                }

                // Let label = Get(f, zero)
                res = Term::Let(
                    (label_var.clone(), Type::Int),
                    Box::new(Term::Get(f.clone(), zero_var.clone())),
                    Box::new(res),
                );

                // Let zero = 0
                res = Term::Let(
                    (zero_var.clone(), Type::Int),
                    Box::new(Term::Int(0)),
                    Box::new(res),
                );

                res
            }
            CpsTerm::AppDir(l, args) => {
                eprintln!("DEBUG: AppDir {} args: {:?}", l, args);
                // AppDir(l, args)
                // Just Push args and Jump.
                // NO Dummy Self. FVs (if any) are assumed on Stack (if recursive) or Empty (global).

                let mut push_ops = Vec::new();
                // if l.starts_with("fib") {
                eprintln!("DEBUG: AppDir {} args: {:?}", l, args);
                // }
                for arg in args {
                    push_ops.push(Term::Push(arg.clone()));
                }

                // Check if we need to push implicit self/env dummy args
                if let Some(expected_count) = self.func_arg_counts.get(l) {
                    if args.len() < *expected_count {
                        // let missing = *expected_count - args.len();
                        // eprintln!(
                        //     "DEBUG: AppDir {} missing {} args. Pushing dummies.",
                        //     l, missing
                        // );
                    }
                }

                // Revised logic:
                let mut extra_pushes = 0;
                if let Some(expected_count) = self.func_arg_counts.get(l) {
                    if args.len() < *expected_count {
                        extra_pushes = *expected_count - args.len();
                    }
                }

                let call = Term::TailCallBlock(l.clone());

                let mut res = call;

                // Wrap pushes (Reverse order of execution -> Last push is Inner wrapper)
                // But we constructed push_ops in Argument Order (Arg1, Arg2...).
                // Execution: Push Arg1, Push Arg2...
                // Stack: [Arg1, Arg2...] Top is ArgN.
                // Output code: Let ... Push Arg1 ... Let ... Push Arg2...
                // So First wrapper = Push ArgN.
                // So reverse the list.

                // Add Dummies at the END of arguments (Implicit Self is last).
                // So Push dummies LAST.
                // So Wrap dummies FIRST (Innermost).

                let closure_info = self.closure_map.get(l);

                for i in 0..extra_pushes {
                    // i=0 is pushed LAST (Self).
                    let mut used_tuple = false;

                    let dummy_val = if i == 0 {
                        if let Some((_, fvs)) = closure_info {
                            if !fvs.is_empty() {
                                // Generate Tuple for FVs
                                let tuple_var = id::gentmp(&Type::Int); // Type doesn't matter much for generation, strictly
                                let fv_vars: Vec<id::T> = fvs.iter().map(|x| x.clone()).collect();

                                // Let tuple = Term::Tuple(fvs) in ...
                                res = Term::Let(
                                    (tuple_var.clone(), Type::Int), // Type fallback
                                    Box::new(Term::Tuple(fv_vars)),
                                    Box::new(res),
                                );
                                used_tuple = true;
                                tuple_var
                            } else {
                                id::gentmp(&Type::Int)
                            }
                        } else {
                            id::gentmp(&Type::Int)
                        }
                    } else {
                        id::gentmp(&Type::Int)
                    };

                    let val_term = if used_tuple {
                        Term::Var(dummy_val.clone())
                    } else {
                        // Fallback dummy
                        if i == 0 {
                            // If i=0 and no FVs, it matches previous logic?
                            // Previous logic generated 'Let zero = 0'.
                        }
                        Term::Int(0)
                    };

                    // IF used_tuple:
                    //   Let tuple = Tuple... (Wrapped above)
                    //   Let unit = Push(tuple) ...

                    // IF not used_tuple:
                    //   Let zero = 0
                    //   Let unit = Push(zero)

                    if !used_tuple {
                        // Generate the Int=0 definition
                        res = Term::Let(
                            (dummy_val.clone(), Type::Int),
                            Box::new(Term::Int(0)),
                            Box::new(res),
                        );
                    }

                    let dummy_unit = id::gentmp(&Type::Unit);
                    res = Term::Let(
                        (dummy_unit, Type::Unit),
                        Box::new(Term::Push(dummy_val)),
                        Box::new(res),
                    );
                }

                for op in push_ops.into_iter().rev() {
                    let dummy = id::gentmp(&Type::Unit);
                    res = Term::Let((dummy, Type::Unit), Box::new(op), Box::new(res));
                }

                res
            }
        }
    }

    fn bind_atom(&mut self, atom: CpsAtom, dest: &id::T, next: BlockedTerm) -> BlockedTerm {
        // eprintln!("DEBUG: bind_atom atom={:?} dest={}", atom, dest);
        if let CpsAtom::Sub(_, _) = atom {
            eprintln!("DEBUG: bind_atom converting Sub to dest={}", dest);
        }
        match atom {
            CpsAtom::Unit => Term::Unit,
            CpsAtom::Int(i) => Term::Int(i),
            CpsAtom::Float(d) => Term::Float(d),
            CpsAtom::Var(x) => Term::Var(x.clone()),
            CpsAtom::Neg(x) => Term::Neg(x.clone()),
            CpsAtom::Add(x, y) => Term::Add(x.clone(), y.clone()),
            CpsAtom::Sub(x, y) => Term::Sub(x.clone(), y.clone()),
            CpsAtom::FNeg(x) => Term::FNeg(x.clone()),
            CpsAtom::FAdd(x, y) => Term::FAdd(x.clone(), y.clone()),
            CpsAtom::FSub(x, y) => Term::FSub(x.clone(), y.clone()),
            CpsAtom::FMul(x, y) => Term::FMul(x.clone(), y.clone()),
            CpsAtom::FDiv(x, y) => Term::FDiv(x.clone(), y.clone()),
            CpsAtom::Get(x, y) => Term::Get(x.clone(), y.clone()),
            CpsAtom::Put(x, y, z) => Term::Put(x.clone(), y.clone(), z.clone()),
            CpsAtom::ExtArray(x) => Term::ExtArray(x.clone()),
            CpsAtom::Tuple(xs) => Term::Tuple(xs.clone()),
            CpsAtom::MakeCls(_) => panic!("MakeCls should be handled in convert_term"),
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
            CpsAtom::Get(x, y) => Term::Get(x.clone(), y.clone()),
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
    // 1. Scan for MakeCls
    let mut closure_map = HashMap::new();
    scan_make_cls(&prog.body, &mut closure_map);
    for fundef in &prog.fundefs {
        scan_make_cls(&fundef.body, &mut closure_map);
    }
    // eprintln!("DEBUG: Closure Map: {:?}", closure_map);

    // 0. Scan fundefs for arg counts (Global and Local)
    let mut func_arg_counts = HashMap::new();
    for fundef in &prog.fundefs {
        func_arg_counts.insert(fundef.name.0.clone(), fundef.args.len());
        scan_arg_counts(&fundef.body, &mut func_arg_counts); // Scan body of globals too
    }
    scan_arg_counts(&prog.body, &mut func_arg_counts); // Scan main body

    let mut converter = Converter::new(closure_map, func_arg_counts);

    // Convert main body
    let entry_label = "main".to_string();
    let main_term = converter.convert_term(&prog.body);
    converter.add_block(entry_label.clone(), main_term);

    // Convert functions
    for fundef in &prog.fundefs {
        let func_label = fundef.name.0.clone();
        eprintln!(
            "DEBUG: Global Fundef {} args: {:?}",
            func_label, fundef.args
        );
        let mut func_term = converter.convert_term(&fundef.body);

        // Prepend loading of free variables (if any, but CPS fundefs from closure don't have formal_fv in CPS struct?)
        // I will assume for now that I need to update cps::Fundef.
        // But for this step, I will just emit GetArg.
        // If formal_fv is missing, we can't emit GetEnv.

        // Prepend loading of arguments
        // 1. Strip implicit Self argument (last arg) - CPS Fundefs DO NOT have Self in args
        // Actually they DO have Self in args if closure conversion added it.
        // And we MUST pop it because it's on the stack (pushed by App/AppDir).
        let mut real_args = fundef.args.clone();
        let mut dropped_arg = None;
        if !real_args.is_empty() {
            dropped_arg = real_args.pop(); // Remove Self
        }

        // 2. Pop remaining arguments in FORWARD order (Wraps to Inside-Out: Let ArgN ... Let Arg1)
        for (arg, ty) in real_args.into_iter() {
            func_term = Term::Let(
                (arg.clone(), ty.clone()),
                Box::new(Term::Pop(arg.clone())),
                Box::new(func_term),
            );
        }

        // 3. Pop Dropped Arg (Self) - Execute FIRST (Innermost wrapper)
        if let Some((arg, ty)) = dropped_arg {
            // Use a dummy name to indicate discard, but use actual type
            let dummy = id::gentmp(&ty);
            func_term = Term::Let(
                (dummy, ty),
                Box::new(Term::Pop(arg)), // Store original name for debug/trace?
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
