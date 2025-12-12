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
}

impl Converter {
    fn new(closure_map: HashMap<id::T, (id::L, Vec<id::T>)>) -> Self {
        Converter {
            blocks: Vec::new(),
            closure_map,
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
                    // 1. GetSp(x) (Current Stack Pointer is the address of the closure)
                    // 2. Push(LoadLabel(entry))
                    // 3. Push(fv)...

                    let entry_var = id::gentmp(&Type::Int);

                    // Construct Pushes
                    let mut push_ops = Vec::new();

                    // Push Entry Label
                    let load_label = Term::Let(
                        (entry_var.clone(), Type::Int),
                        Box::new(Term::LoadLabel(cls.entry.clone())),
                        Box::new(Term::Push(entry_var.clone())),
                    );
                    push_ops.push(load_label);

                    // Push FVs
                    for fv in &cls.actual_fv {
                        push_ops.push(Term::Push(fv.clone()));
                    }

                    let body = self.convert_term(e);

                    // Sequence: GetSp(x) -> push_ops -> body
                    // We structure this as nested Lets?
                    // blocked::Term is not a list of instructions, it's a tree of Lets.
                    // But `Push` returns Unit.
                    // Let _ = Push() in ...

                    let mut res = body;
                    for op in push_ops.into_iter().rev() {
                        let dummy = id::gentmp(&Type::Unit);
                        res = Term::Let((dummy, Type::Unit), Box::new(op), Box::new(res));
                    }

                    // Prepend GetSp(x)
                    res = Term::Let(
                        (x.clone(), t.clone()),
                        Box::new(Term::GetSp(x.clone())), // Get current SP as closure address
                        Box::new(res),
                    );

                    return res;
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
                let func_body = self.convert_term(&fundef.body);
                eprintln!(
                    "DEBUG: LetRec func: {}, args: {:?}",
                    func_label, fundef.args
                );

                // Arguments are popped in reverse order of pushing.
                // Caller Pushes: Arg1, Arg2, ... ArgN, Self.
                // Stack Top: Self.
                // So we Pop: Self, ArgN, ... Arg1.

                // Arguments are popped in reverse order of pushing (Stack LIFO).
                // Caller Pushes: Arg1, Arg2, ... Self (Top).
                // We must Pop: Self, ... Arg1.
                // To generate "Let Self=Pop in Let ... in Let Arg1=Pop", we iterate Forward [Arg1, ..., Self].
                let mut wrapped_body = func_body;
                for (arg, ty) in fundef.args.iter() {
                    wrapped_body = Term::Let(
                        (arg.clone(), ty.clone()),
                        Box::new(Term::Pop(arg.clone())),
                        Box::new(wrapped_body),
                    );
                }

                self.add_block(func_label.clone(), wrapped_body);

                // Create Closure for this function
                // 1. Calculate FV
                // fv(LetRec) logic in cps.rs excludes function name and args.
                // We need fvs of the BODY.
                let mut body_fv = cps::fv(&fundef.body);
                for (arg, _) in &fundef.args {
                    body_fv.remove(arg);
                }
                body_fv.remove(&fundef.name.0); // Remove recursive self reference from FVs (it's passed as argument)

                let mut sorted_fvs: Vec<_> = body_fv.into_iter().collect();
                sorted_fvs.sort(); // Deterministic order

                // 2. Generate MakeCls code (LoadLabel, Push FVs, GetSp)
                // Same logic as CpsTerm::MakeCls
                let mut push_ops = Vec::new();

                // Push Entry Label
                let temp_entry = id::gentmp(&Type::Int);
                // LoadLabel returns Int.
                push_ops.push(Term::Let(
                    (temp_entry.clone(), Type::Int),
                    Box::new(Term::LoadLabel(func_label)),
                    Box::new(Term::Push(temp_entry)),
                ));

                // Push FVs
                for fv in sorted_fvs {
                    push_ops.push(Term::Push(fv.clone()));
                }

                // 3. Bind Closure Ptr (SP) to func name
                // Sequence: PushOps -> GetSp(func_name) -> Rest(e)
                // We build bottom-up.

                let rest = self.convert_term(e);

                // GetSp
                let mut res = Term::Let(
                    fundef.name.clone(),
                    Box::new(Term::GetSp(fundef.name.0.clone())),
                    Box::new(rest),
                );

                // Wrap Pushes
                for op in push_ops.into_iter().rev() {
                    // op is Term::Push(..) or Term::Let(..Push..)
                    // If it's Push, it returns Unit.
                    // If it's Let, it returns Unit (Push result).
                    match op {
                        Term::Push(var) => {
                            let dummy = id::gentmp(&Type::Unit);
                            res = Term::Let(
                                (dummy, Type::Unit),
                                Box::new(Term::Push(var)),
                                Box::new(res),
                            );
                        }
                        Term::Let((x, t), v, body) => {
                            // op is Let(temp, LoadLabel, Push(temp)).
                            // We want `Let temp = LoadLabel in Let _ = Push(temp) in res`.
                            // So, the `body` of this `Let` (which is `Push(temp)`) needs to be wrapped with `res`.
                            let dummy = id::gentmp(&Type::Unit);
                            let new_body = Term::Let((dummy, Type::Unit), body, Box::new(res));
                            res = Term::Let((x, t), v, Box::new(new_body));
                        }
                        _ => panic!("Unexpected op structure in LetRec MakeCls"),
                    }
                }

                res
            }
            CpsTerm::AppCls(f, args) => {
                // AppCls(f, args)
                // args includes k_cls.
                // 1. Push args...
                // 2. Push f (Self)
                // 3. Load code = f[0]
                // 4. TailCallDynamic(code)

                let mut all_args = args.clone();
                all_args.push(f.clone()); // Self

                let mut push_ops = Vec::new();
                for arg in all_args {
                    push_ops.push(Term::Push(arg));
                }

                // Load Code
                let code_var = id::gentmp(&Type::Int);
                // Get(f, 0)
                let load_code = Term::Get(f.clone(), id::gentmp(&Type::Int)); // Wait, Get second arg is var.
                // We need a var for index 0.
                let zero_var = id::gentmp(&Type::Int);
                let get_op = Term::Let(
                    (zero_var.clone(), Type::Int),
                    Box::new(Term::Int(0)),
                    Box::new(Term::Let(
                        (code_var.clone(), Type::Int),
                        Box::new(Term::Get(f.clone(), zero_var)),
                        Box::new(Term::TailCallDynamic(code_var.clone())),
                    )),
                );

                // Chain Pushes
                let mut res = get_op;
                for op in push_ops.into_iter().rev() {
                    let dummy = id::gentmp(&Type::Unit);
                    res = Term::Let((dummy, Type::Unit), Box::new(op), Box::new(res));
                }

                res
            }
            CpsTerm::AppDir(l, args) => {
                // AppDir(l, args)
                // args includes k_cls? Yes usually.
                // Do we pass Self?
                // Top-level functions don't use Self?
                // But `f` in `cps.rs` added `self_env` to ALL functions.
                // So `AppDir` targets must also accept `self_env`.
                // But what is `self_env` for a direct call?
                // Use `0` (Unit/Null)?

                // Wait, `cps.rs` `AppDir` handling: `app_args.push(k_cls)`.
                // `cps.rs` did NOT add `self` to `AppDir` args.
                // BUT `f` (defs) logic added `self` to args of ALL functions.
                // This is a mismatch.
                // If I change a function signature, I must update all call sites.
                // `AppDir` calls a label.
                // That label corresponds to a function converted by `f`.
                // So it EXPECTS `self`.
                // So I MUST push a dummy `self` for `AppDir`.

                let mut all_args = args.clone();
                // Push dummy self (0)
                let dummy_self = id::gentmp(&Type::Int);
                all_args.push(dummy_self.clone());

                // We need to define `dummy_self = 0` before pushing.

                let mut push_ops = Vec::new();
                for arg in all_args {
                    push_ops.push(Term::Push(arg));
                }

                let call = Term::TailCallBlock(l.clone());

                let mut res = call;
                for op in push_ops.into_iter().rev() {
                    // If op is Push(dummy_self), ensure dummy_self is defined?
                    // No, `Push(arg)` assumes arg is bound.
                    // We need to bind `dummy_self` globally for this block?
                    // Or wrap.
                    let dummy = id::gentmp(&Type::Unit);
                    res = Term::Let((dummy, Type::Unit), Box::new(op), Box::new(res));
                }

                // Wrap with `Let dummy_self = 0`
                Term::Let(
                    (dummy_self, Type::Int),
                    Box::new(Term::Int(0)),
                    Box::new(res),
                )
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
        for (i, (arg, ty)) in fundef.args.iter().enumerate() {
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
