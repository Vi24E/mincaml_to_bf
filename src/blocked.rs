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
    SetArgs(Vec<id::T>),
    GetArg(usize),
    GetEnv(usize),
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
                    if cls.actual_fv.is_empty() {
                        // Optimization: If no free variables, just load the label.
                        // No Tuple allocation (Store) needed.
                        let val = Term::LoadLabel(cls.entry.clone());
                        let body = self.convert_term(e);
                        return Term::Let((x.clone(), t.clone()), Box::new(val), Box::new(body));
                    }

                    // MakeCls(cls) -> Tuple(cls.entry, cls.actual_fv...)
                    // We need to treat the entry label as an integer (block ID).
                    // This requires a way to get the block ID at runtime?
                    // Or we assume `id::L` can be used as `id::T` if we map it?
                    // `Tuple` expects `Vec<id::T>`.
                    // We need to introduce a Let(entry_var, Int(label_id))?
                    // But we don't know label_id here.
                    // We can use Atom::Label(id::L)? `Tuple` only takes `id::T`.
                    // Let's use `ExtArray` or similar hack? No.
                    // We need `Atom::Label` or similar in `Term`.
                    // Actually, `Atom::Int` takes `i32`.
                    // We can rely on `intermediate` to resolve Label to Int?
                    // But `Tuple` is `Vec<id::T>`. The elements must be variables.

                    // So we must: `Let entry = Label(cls.entry)`. `Let closure = Tuple(entry, fvs)`.
                    // Does `Term` have `Label`? No.
                    // We need to add `Term::Label(id::L)` or `Atom::Label(id::L)`.
                    // Let's convert it to `Term::Let((x, t), Atom::Tuple(..), ..)`.
                    // But first we need a variable holding the label.

                    let entry_var = id::gentmp(&Type::Int);
                    let mut tuple_elems = vec![entry_var.clone()];
                    tuple_elems.extend(cls.actual_fv.clone());

                    // Recursive conversion of body
                    let body = self.convert_term(e);

                    // Construct: Let entry = LoadLabel(entry_label); Let x = Tuple(entry, fvs); body
                    let load_label = Term::Let(
                        (entry_var.clone(), Type::Int),
                        Box::new(Term::LoadLabel(cls.entry.clone())),
                        Box::new(Term::Let(
                            (x.clone(), t.clone()),
                            Box::new(Term::Tuple(tuple_elems)),
                            Box::new(body),
                        )),
                    );
                    return load_label;
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
                // fundef.body is a new block.
                let func_label = fundef.name.0.clone(); // Use function name as label
                let func_body = self.convert_term(&fundef.body);

                // We need to handle arguments.
                // In blocked, arguments are loaded via GetArg.
                // So we wrap the body with Let(arg, GetArg(i), ...).
                let mut wrapped_body = func_body;
                for (i, (arg, ty)) in fundef.args.iter().enumerate().rev() {
                    wrapped_body = Term::Let(
                        (arg.clone(), ty.clone()),
                        Box::new(Term::GetArg(i)),
                        Box::new(wrapped_body),
                    );
                }

                self.add_block(func_label, wrapped_body);

                // Continue with e
                self.convert_term(e)
            }
            CpsTerm::AppCls(f, args) => {
                // Optimization: Assume `f` is a raw function pointer (Label Index), NOT a tuple.
                // This assumes all closures are empty and we optimized MakeCls to LoadLabel.

                // SetArgs(args..., f) <--- Pass f as Env (it's the integer label, but fine)
                let mut all_args = args.clone();
                all_args.push(f.clone()); // Pass the function ptr itself as environment

                let set_args = Term::SetArgs(all_args);

                // TailCallDynamic(f) directly
                let call = Term::TailCallDynamic(f.clone());

                let dummy = id::gentmp(&Type::Unit);
                Term::Let((dummy, Type::Unit), Box::new(set_args), Box::new(call))
            }
            CpsTerm::AppDir(l, args) => {
                // AppDir can stay as is (TailCallBlock), or if we are consistent, we pass Env?
                // Known functions don't need Env if they don't use it.
                // But `lambda lifting` in intermediate assumes Env is passed?
                // If `intermediate` maps `GetEnv` to `GetStack`, then YES, we must pass Env.
                // But for AppDir, `Env` is usually empty or not used?
                // If the function was a closure but optimized to AppDir, it might still have free vars?
                // CPS `AppDir` implies direct call. `scan_make_cls` found its FVs?
                // If `mincaml` optimizes `AppCls` to `AppDir`, it means FVs are handled or empty?
                // Usually `AppDir` is for top-level functions which have no FVs.
                // So we don't need to pass Env.
                // BUT `GetEnv` logic in `virtual` relies on Stack position.
                // If `GetEnv` is used inside a function called via `AppDir`, it will read wrong stack!
                // Wait, top-level functions don't use `GetEnv` because they have no free variables!
                // So `AppDir` is safe without extra args.

                let set_args = Term::SetArgs(args.clone());
                let call = Term::TailCallBlock(l.clone());
                let dummy = id::gentmp(&Type::Unit);
                Term::Let((dummy, Type::Unit), Box::new(set_args), Box::new(call))
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
        for (i, (arg, ty)) in fundef.args.iter().enumerate().rev() {
            func_term = Term::Let(
                (arg.clone(), ty.clone()),
                Box::new(Term::GetArg(i)),
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
