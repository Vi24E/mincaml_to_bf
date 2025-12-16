use crate::id;

#[derive(Debug)]
pub enum Operation {
    SetImm(u32, i32),
    Neg(u32, u32),
    Add(u32, u32, u32),
    Sub(u32, u32, u32),
    SubZ(u32, u32, u32),
    JumpIfZero(u32, u32, u32),

    Jump(u32),
    JumpVar(u32),
    MoveData(u32, u32, usize),
    CallExternal(String),

    Push(u32),
    Pop(u32),
    Halt,
}

#[derive(Debug)]
pub struct Block {
    pub ops: Vec<Operation>,
}

use crate::intermediate::{self, Atom, Term};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum ConstVal {
    Int(i32),
    LoadLabel(id::L),
    Tuple(Vec<id::T>),
}

#[derive(Debug)]
pub struct Prog {
    pub blocks: Vec<Block>,
    pub block_count: usize,
    pub var_count: usize,
    pub reg_start: usize,
    pub var_start: usize,
    pub stack_start: usize,
    pub sp_addr: usize,
}

impl Prog {
    pub fn new(
        blocks: Vec<Block>,
        block_count: usize,
        var_count: usize,
        reg_start: usize,
        var_start: usize,
        stack_start: usize,
        sp_addr: usize,
    ) -> Self {
        Prog {
            blocks,
            block_count,
            var_count,
            reg_start,
            var_start,
            stack_start,
            sp_addr,
        }
    }
}

pub fn f(prog: &intermediate::Prog) -> Prog {
    use std::io::Write;
    std::io::stderr().flush().unwrap();

    let constants = analyze_constants(prog);

    let block_count = prog.layout.block_count;
    let var_count = prog.layout.var_count;
    let reg_size = 128;
    let reg_start = (block_count + 1) * 2;
    let sp_addr = reg_start;
    let var_start = reg_start + reg_size;
    let stack_start = var_start + var_count * 32;

    let mut blocks = Vec::new();
    let block_map = &prog.layout.block_map;
    let var_map = &prog.layout.var_map;

    let mut sorted_blocks: Vec<(&id::L, &intermediate::Block)> =
        prog.blocks.iter().map(|b| (&b.id, b)).collect();
    sorted_blocks.sort_by_key(|(id, _)| *block_map.get(*id).unwrap());

    let _entry_idx = 0;
    for (_i, block) in sorted_blocks {
        let mut ops = Vec::new();

        let cmp_temp_addr = var_start + (var_count - 1) * 32;
        convert_term(
            &block.term,
            &mut ops,
            var_map,
            block_map,
            reg_start,
            var_start,
            stack_start,
            cmp_temp_addr,
            sp_addr,
            &constants,
        );
        let mut keys: Vec<_> = var_map.keys().collect();
        keys.sort();
        blocks.push(Block { ops: ops });
    }

    Prog::new(
        blocks,
        block_count,
        var_count,
        reg_start,
        var_start,
        stack_start,
        sp_addr,
    )
}

fn analyze_constants(prog: &intermediate::Prog) -> HashMap<id::T, ConstVal> {
    let mut constants = HashMap::new();
    for block in &prog.blocks {
        analyze_term(&block.term, &mut constants);
    }
    constants
}

fn analyze_term(term: &Term, constants: &mut HashMap<id::T, ConstVal>) {
    match term {
        Term::Let((x, _), atom, e) => {
            match atom {
                Atom::Int(i) => {
                    constants.insert(x.clone(), ConstVal::Int(*i));
                }
                Atom::LoadLabel(l) => {
                    constants.insert(x.clone(), ConstVal::LoadLabel(l.clone()));
                }
                Atom::Tuple(xs) => {
                    constants.insert(x.clone(), ConstVal::Tuple(xs.clone()));
                }
                _ => {}
            }
            analyze_term(e, constants);
        }
        Term::LetTuple(_, _, e) => analyze_term(e, constants),
        Term::IfEq(_, _, _, _)
        | Term::IfLE(_, _, _, _)
        | Term::Jump(_)
        | Term::JumpVar(_)
        | Term::CallExternal(_) => {}
        Term::Atom(_) => {}
        Term::Ret(_) => {}
    }
}

fn convert_term(
    term: &Term,
    ops: &mut Vec<Operation>,
    var_map: &HashMap<id::T, usize>,
    block_map: &HashMap<id::L, usize>,
    reg_start: usize,
    var_start: usize,
    stack_start: usize,
    cmp_temp_addr: usize,
    sp_addr: usize,
    constants: &HashMap<id::T, ConstVal>,
) {
    match term {
        Term::Let((x, _), atom, e) => {
            let dest_addr = (var_start + var_map.get(x).unwrap() * 32) as u32;
            convert_atom(
                atom,
                dest_addr,
                ops,
                var_map,
                block_map,
                reg_start,
                var_start,
                stack_start,
                sp_addr,
                constants,
            );
            convert_term(
                e,
                ops,
                var_map,
                block_map,
                reg_start,
                var_start,
                stack_start,
                cmp_temp_addr,
                sp_addr,
                constants,
            );
        }
        Term::Jump(l) => {
            if l == "halt" {
                ops.push(Operation::Halt);
            } else if let Some(target_idx) = block_map.get(l) {
                ops.push(Operation::Jump(*target_idx as u32));
            } else if let Some(_offset) = var_map.get(l) {
                // Legacy: Jumping to a variable used to output a byte, but this is removed.
                // Assuming this path is reachable for dynamic jumps, we should treat it as JumpVar or error?
                // Given the original code: it was `OutputByte(addr)`.
                // The user requested removing OutputByte.
                // "Jump(l)" where l is a variable -> JumpVar(l) logic seems better if supported,
                // but `convert_term` has a separate `JumpVar` variant for that.
                // If `l` is in `var_map` here, it means `Term::Jump` was called with a variable name.
                // In properly blocked IR, this should be `Term::JumpVar`.
                // We will panic for now as this seems like malformed IR or legacy debug behavior.
                panic!(
                    "Jump to variable '{}' via Term::Jump is legacy behavior. Use Term::JumpVar.",
                    l
                );
            } else {
                ops.push(Operation::CallExternal(l.clone()));
            }
        }
        Term::JumpVar(x) => {
            let addr = (var_start
                + var_map
                    .get(x)
                    .expect(&format!("JumpVar: Variable {} not found in var_map", x))
                    * 32) as u32;
            ops.push(Operation::JumpVar(addr));
        }
        Term::CallExternal(l) => {
            ops.push(Operation::CallExternal(l.clone()));
        }
        Term::IfEq(x, y, l1, l2) => {
            let addr_x = (var_start + var_map.get(x).unwrap() * 32) as u32;
            let addr_y = (var_start + var_map.get(y).unwrap() * 32) as u32;
            let idx_l1 = *block_map.get(l1).unwrap() as u32;
            let idx_l2 = *block_map.get(l2).unwrap() as u32;
            let tmp_addr = cmp_temp_addr as u32;
            ops.push(Operation::Sub(tmp_addr, addr_x, addr_y));
            ops.push(Operation::JumpIfZero(tmp_addr, idx_l1, idx_l2));
        }
        Term::IfLE(x, y, l1, l2) => {
            let addr_x = (var_start + var_map.get(x).unwrap() * 32) as u32;
            let addr_y = (var_start + var_map.get(y).unwrap() * 32) as u32;
            let idx_l1 = *block_map.get(l1).unwrap() as u32;
            let idx_l2 = *block_map.get(l2).unwrap() as u32;
            let tmp_addr = cmp_temp_addr as u32;

            ops.push(Operation::SubZ(tmp_addr, addr_x, addr_y));
            ops.push(Operation::JumpIfZero(tmp_addr, idx_l1, idx_l2));
        }
        Term::LetTuple(xts, atom, e) => {
            let tuple_ptr_addr = match atom {
                Atom::Var(v) => (var_start + var_map.get(v).unwrap() * 32) as u32,
                _ => panic!("LetTuple expected Var on RHS, got {:?}", atom),
            };

            for (i, (x, _)) in xts.iter().enumerate() {
                let _dest_addr = (var_start + var_map.get(x).unwrap() * 32) as u32;
                let tmp_addr = (reg_start + 64) as u32;
                ops.push(Operation::SetImm(tmp_addr, (i * 32) as i32));
                ops.push(Operation::Add(tmp_addr, tuple_ptr_addr, tmp_addr));

                panic!("LetTuple Load not supported");
            }

            convert_term(
                e,
                ops,
                var_map,
                block_map,
                reg_start,
                var_start,
                stack_start,
                cmp_temp_addr,
                sp_addr,
                constants,
            );
        }
        Term::Atom(_) => panic!("Atom at tail position should not happen in blocked IR"),
        Term::Ret(_) => {}
    }
}

fn convert_atom(
    atom: &Atom,
    dest_addr: u32,
    ops: &mut Vec<Operation>,
    var_map: &HashMap<id::T, usize>,
    block_map: &HashMap<id::L, usize>,
    reg_start: usize,
    var_start: usize,
    stack_start: usize,
    _sp_addr: usize,
    constants: &HashMap<id::T, ConstVal>,
) {
    match atom {
        Atom::Unit => ops.push(Operation::SetImm(dest_addr, 0)),
        Atom::Int(i) => ops.push(Operation::SetImm(dest_addr, *i)),
        Atom::Float(_) => panic!("Float not supported"),
        Atom::Var(x) => {
            let src_addr = (var_start + var_map.get(x).unwrap() * 32) as u32;
            ops.push(Operation::MoveData(dest_addr, src_addr, 32));
        }
        Atom::Add(x, y) => {
            let addr_x = (var_start + var_map.get(x).unwrap() * 32) as u32;
            let addr_y = (var_start + var_map.get(y).unwrap() * 32) as u32;
            ops.push(Operation::Add(dest_addr, addr_x, addr_y));
        }
        Atom::Sub(x, y) => {
            let addr_x = (var_start + var_map.get(x).unwrap() * 32) as u32;
            let addr_y = (var_start + var_map.get(y).unwrap() * 32) as u32;
            ops.push(Operation::Sub(dest_addr, addr_x, addr_y));
        }
        Atom::FNeg(_)
        | Atom::FAdd(_, _)
        | Atom::FSub(_, _)
        | Atom::FMul(_, _)
        | Atom::FDiv(_, _) => {
            panic!("Float ops not supported");
        }
        Atom::Neg(x) => {
            let src_addr = (var_start + var_map.get(x).unwrap() * 32) as u32;
            ops.push(Operation::Neg(dest_addr, src_addr));
        }
        Atom::GetStack(i) => {
            let src_addr = (stack_start + i * 32) as u32;
            ops.push(Operation::MoveData(dest_addr, src_addr, 32));
        }

        Atom::LoadLabel(l) => {
            if let Some(idx) = block_map.get(l) {
                ops.push(Operation::SetImm(dest_addr, (*idx as i32) + 1));
            } else {
                panic!("LoadLabel: Label not found: {}", l);
            }
        }
        Atom::Tuple(xs) => {
            let sp_addr = (reg_start + 0) as u32;
            ops.push(Operation::MoveData(dest_addr, sp_addr, 32));

            for x in xs {
                let src_addr = (var_start + var_map.get(x).unwrap() * 32) as u32;
                ops.push(Operation::Push(src_addr));
            }
        }
        Atom::Get(x, y) => {
            let addr_x = (var_start
                + match var_map.get(x) {
                    Some(v) => *v,
                    None => panic!("Get: Variable not found in var_map: {}", x),
                } * 32) as u32;
            let dest_addr = dest_addr;

            let mut optim_success = false;
            if let Some(ConstVal::Int(i)) = constants.get(y) {
                if let Some(ConstVal::Tuple(fields)) = constants.get(x) {
                    if *i >= 0 && (*i as usize) < fields.len() {
                        let field_var = &fields[*i as usize];
                        if let Some(ConstVal::LoadLabel(l)) = constants.get(field_var) {
                            if let Some(block_idx) = block_map.get(l) {
                                ops.push(Operation::SetImm(dest_addr, *block_idx as i32));
                                optim_success = true;
                            }
                        }
                    }
                }
            }

            if !optim_success {
                let addr_y = (var_start + var_map.get(y).unwrap() * 32) as u32;
                let tmp_addr = (reg_start + 64) as u32;
                ops.push(Operation::MoveData(tmp_addr, addr_y, 32));
                for _ in 0..5 {
                    ops.push(Operation::Add(tmp_addr, tmp_addr, tmp_addr));
                }
                ops.push(Operation::MoveData(dest_addr, addr_x, 32));
                ops.push(Operation::Add(tmp_addr, dest_addr, tmp_addr));
                panic!("Get Load not supported");
            }
        }
        Atom::Put(x, y, z) => {
            let addr_x = (var_start + var_map.get(x).unwrap() * 32) as u32;
            let addr_y = (var_start + var_map.get(y).unwrap() * 32) as u32;
            let _addr_z = (var_start + var_map.get(z).unwrap() * 32) as u32;
            let tmp_addr = (reg_start + 64) as u32;
            ops.push(Operation::MoveData(tmp_addr, addr_y, 32));
            for _ in 0..5 {
                ops.push(Operation::Add(tmp_addr, tmp_addr, tmp_addr));
            }
            ops.push(Operation::Add(tmp_addr, addr_x, tmp_addr));

            panic!("Put Store not supported");
        }
        Atom::ExtArray(_l) => {
            ops.push(Operation::SetImm(dest_addr, 0));
        }
        Atom::Push(x) => {
            let src_addr = (var_start
                + match var_map.get(x) {
                    Some(v) => *v,
                    None => {
                        panic!("Push: Variable not found in var_map: {}", x);
                    }
                } * 32) as u32;
            ops.push(Operation::Push(src_addr));
            ops.push(Operation::SetImm(dest_addr, 0));
        }
        Atom::Pop => {
            ops.push(Operation::Pop(dest_addr));
        }

        Atom::CallDir(l, args) => {
            for (i, x) in args.iter().enumerate() {
                let dst_stack_addr = (stack_start + i * 32) as u32;
                if let Some(offset) = var_map.get(x) {
                    let src_addr = (var_start + offset * 32) as u32;
                    ops.push(Operation::MoveData(dst_stack_addr, src_addr, 32));
                } else if let Some(block_idx) = block_map.get(x) {
                    ops.push(Operation::SetImm(dst_stack_addr, *block_idx as i32));
                } else {
                    panic!("CallDir: Variable or Label not found: {}", x);
                }
            }
            ops.push(Operation::CallExternal(l.clone()));
            ops.push(Operation::SetImm(dest_addr, 0));
        }
    }
}

use std::fmt;

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operation::SetImm(dest, val) => write!(f, "SetImm({}, {})", dest, val),
            Operation::Neg(dest, src) => write!(f, "Neg({}, {})", dest, src),
            Operation::Add(dest, src1, src2) => write!(f, "Add({}, {}, {})", dest, src1, src2),
            Operation::Sub(dest, src1, src2) => write!(f, "Sub({}, {}, {})", dest, src1, src2),
            Operation::SubZ(dest, src1, src2) => write!(f, "SubZ({}, {}, {})", dest, src1, src2),
            Operation::JumpIfZero(cond, l1, l2) => {
                write!(f, "JumpIfZero({}, {}, {})", cond, l1, l2)
            }

            Operation::Jump(target) => write!(f, "Jump({})", target),
            Operation::JumpVar(src) => write!(f, "JumpVar({})", src),
            Operation::MoveData(dest, src, size) => {
                write!(f, "MoveData({}, {}, {})", dest, src, size)
            }
            Operation::CallExternal(name) => write!(f, "CallExternal({})", name),

            Operation::Push(src) => write!(f, "Push({})", src),
            Operation::Pop(dest) => write!(f, "Pop({})", dest),
            Operation::Halt => write!(f, "Halt"),
        }
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for op in &self.ops {
            writeln!(f, "  {}", op)?;
        }
        Ok(())
    }
}

impl fmt::Display for Prog {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Virtual Prog:")?;
        writeln!(f, "Block Count: {}", self.block_count)?;
        writeln!(f, "Var Count: {}", self.var_count)?;
        for (i, block) in self.blocks.iter().enumerate() {
            writeln!(f, "Block {}:", i)?;
            write!(f, "{}", block)?;
        }
        Ok(())
    }
}
