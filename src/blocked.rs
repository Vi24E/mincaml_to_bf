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
    // Tuple/Label Tracking for Devirtualization
    tuple_env: HashMap<id::T, Vec<id::T>>,
    label_env: HashMap<id::T, id::L>,

    // Stack Frame Tracking for Cleanup (TailCall optimization)
    locals_stack: Vec<id::T>,

    // Locals (Scalars)
    locals: HashSet<id::T>,
}

impl Converter {
    fn new(closure_fundefs: HashMap<String, crate::closure::Fundef>) -> Converter {
        // eprintln!(
        //     "DEBUG: Converter initialized with closure_fundefs: {:?}",
        //     closure_fundefs.keys().collect::<Vec<_>>()
        // );
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
        // eprintln!("DEBUG: blocked::add_block: {}", label);
        self.blocks.push((label, term));
    }

    fn convert_fundef(&mut self, fundef: &cps::Fundef) {
        let func_label = fundef.name.0.clone();

        // eprintln!(
        //     "DEBUG: convert_fundef: {} args: {:?}",
        //     func_label, fundef.args
        // );

        let current_args = fundef.args.clone();

        // Scope Management:
        // Save current locals.
        let saved_locals = self.locals.clone();

        for (arg, _ty) in &current_args {
            self.locals.insert(arg.clone());
        }

        let body_term = self.convert_term(&fundef.body);
        let mut main_func_body = body_term;

        // Restore locals
        self.locals = saved_locals;

        // Argument Popping Order: Reverse Order
        // Caller Pushes:
        // 1. Continuation Frames (Deepest)
        // 2. Scalar Arguments (Top)
        //
        // Function defines: [FV1, FV2, ..., Arg, Cont]
        // We Pop All Arguments (including Cont).
        // Stack Top: Cont -> ArgN -> ... -> Arg0.
        // Forward Loop: Let(ArgN, Pop, ... Let(Arg0, Pop)).
        // Exec: Pop ArgN (Cont). ... Pop Arg0.

        // 1. Pop CodePtr (MOVED TO END - Outer Wrap)
        // 2. Pop FVs (REMOVED - Included in Explicit Args)
        // args already includes FVs. Pop Explicit Args loop handles them.
        // But we DO need to Pop CodePtr because it is NOT in args. (Assuming CodePtr Arg0 convention).

        // 3. Pop Args
        // If Closure: Split into Explicit (Top) and Cont (Bottom).
        // If Not Closure: All Args are Explicit (Top-to-Bottom).

        let total_args = current_args.len();
        if self.closure_fundefs.contains_key(&fundef.name.0) {
            // Is Closure -> Expects [Cont] [Args] [FVs] [Code].
            // (Args includes FVs).
            // Pop Code (Outer). Pop Explicit (Middle). Pop Cont (Inner).

            if total_args > 0 {
                let last_idx = total_args - 1;
                let (cont_arg, cont_ty) = &current_args[last_idx];

                // Pop Continuaton (Bottom / Inner)
                main_func_body = Term::Let(
                    (cont_arg.clone(), cont_ty.clone()),
                    Box::new(Term::Pop(cont_arg.clone())),
                    Box::new(main_func_body),
                );

                // Pop Explicit Args (Top / Middle)
                // AppClsCont Pushes FVs (Index 0) on TOP of Args (Index 1).
                // So we must Pop Index 0 First.
                // rev() Loop 0..Last:
                // Yield N..0. Let(N)... Let(0). Exec 0...N.
                // So Pop 0 First.
                for i in (0..last_idx).rev() {
                    let (arg, ty) = &current_args[i];
                    main_func_body = Term::Let(
                        (arg.clone(), ty.clone()),
                        Box::new(Term::Pop(arg.clone())),
                        Box::new(main_func_body),
                    );
                }
            }
        } else {
            // Not Closure (Join Block) -> Expects [Args] only.
            // AppCont Pushes [Arg1] [Arg0]. Top Arg0.
            // We want Pop Arg0 First.
            // rev() Loop: Exec 0...N.
            for i in (0..total_args).rev() {
                let (arg, ty) = &current_args[i];
                main_func_body = Term::Let(
                    (arg.clone(), ty.clone()),
                    Box::new(Term::Pop(arg.clone())),
                    Box::new(main_func_body),
                );
            }
        }

        // 1. Pop CodePtr (MOVED HERE - Outer Wrap -> Exec First)
        if let Some(_fundef_in_map) = self.closure_fundefs.get(&fundef.name.0) {
            // eprintln!(
            //     "DEBUG: convert_fundef checking closure: {}",
            //     fundef_in_map.name.0
            // );
            // Verify if this function is actually called as a closure?
            // If so, it expects [Cont] [Args] [FVs] [CodePtr] (Top)

            // Pop Code Pointer (Arg0 in Flattened)
            let code_ptr_var = id::gentmp(&Type::Int); // Dummy var for code ptr
            main_func_body = Term::Let(
                (code_ptr_var.clone(), Type::Int),
                Box::new(Term::Pop(code_ptr_var)),
                Box::new(main_func_body),
            );
        } else {
            // eprintln!(
            //     "DEBUG: convert_fundef NOT treating {} as closure (Not found in closure_fundefs)",
            //     fundef.name.0
            // );
        }

        // 4. Pop Continuation (Bottom)
        // Implicitly handled by caller Pushing Cont Last (Bottom).
        // If function expects Cont as Argument (e.g. k_cont),
        // Wait. `current_args` contains explicit args.
        // If `k_cont` is in explicit args, it's handled in Step 3.
        // We just need to ensure Push Order matches Pop Order.
        // Stack: [K] [Args] [FVs] [Code] (Top).
        // Pop Code. Pop FVs. Pop Args. Pop K. Matches.

        self.add_block(func_label.clone(), main_func_body);
    }

    // Helper: Push Value (Label or Scalar)
    fn push_val(&self, arg: &id::T, res: Term) -> Term {
        // eprintln!(
        //     "DEBUG: push_val {} (In locals: {})",
        //     arg,
        //     self.locals.contains(arg)
        // );
        if self.locals.contains(arg) {
            Term::Let(
                (id::gentmp(&Type::Unit), Type::Unit),
                Box::new(Term::Push(arg.clone())),
                Box::new(res),
            )
        } else if self.closure_fundefs.contains_key(arg) {
            // Known Global Function Label: Load Address then Push
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
            // Fallback for globals/constants
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
                // Stack Layout: [Args] (Top) -> K_Label -> [K_Args] (Bottom)
                // Exec: Push K_Args -> Push K_Label -> Push Args -> Jump

                // 1. Jump f
                let mut res = Term::TailCallDynamic(f.clone());

                // 2. Wrap Push Args (Forward loop -> Top=Arg1)
                for arg in args {
                    res = self.push_val(arg, res);
                }

                // 3. Wrap Push K_Label
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

                // 4. Wrap Push K_Args (Forward loop -> Bottom=Arg1)
                for k_arg in k_args {
                    res = self.push_val(k_arg, res);
                }

                res
            }
        }
    }
}

// scan_papp removed
// scan_arg_counts removed (unused)
// scan_fv_counts removed (unused)

pub fn f(prog: &CpsProg, closure_prog: &crate::closure::Prog) -> Prog {
    // 2. Collect Closure Fundefs for AppCls usage
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

    // eprintln!("DEBUG: blocked::blocks count: {}", converter.blocks.len());
    // for (id, _) in &converter.blocks {
    //     eprintln!("DEBUG: blocked::block: {}", id);
    // }

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
