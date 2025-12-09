use crate::id;
/*
currently, float, tuple, array are not supported.

[0..BlockSize] : Activate Block; 0 is entry point so that Block should be 1-indexed
[BlockSize + 1..BlockSize + RegSize] : "Registers"; place to calculate everything, RegSize is 256
[BlockSize + RegSize + 1..BlockSize + RegSize + VarSize * 4] : Variables; assumed bf's cell size is unsigned char, and int is 32bit
[BlockSize + RegSize + 1 + VarSize * 4..] : Stack; Place to store args(thanks to lambda lifting, args are only stored in stack)

ops (eg. Neg, Add, Sub) should first move data to register, then perform operation, then move data back to variable
all operands u32 are pointer which points to the head-cell of variable
*/
#[derive(Debug)]
pub enum Operation {
    SetImm(u32, i32),          // x = i
    Neg(u32, u32),             // z = -x
    Add(u32, u32, u32),        // z = x + y
    Sub(u32, u32, u32),        // z = x - y
    SubZ(u32, u32, u32),       // z = max(0, x - y)
    JumpIfZero(u32, u32, u32), // if x == 0 goto y
    Jump(u32),                 // Unconditional jump to block y
    MoveData(u32, u32, usize), // move and copy data from x to y, size is in bytes
    CallExternal(String),      // Call external function (e.g. print_int)
    InputByte(u32),            // Read byte to address
    OutputByte(u32),           // Write byte from address
}

#[derive(Debug)]
pub struct Block {
    pub ops: Vec<Operation>,
}

use crate::intermediate::{self, Atom, Term};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Prog {
    pub blocks: Vec<Block>,
    pub block_count: usize,
    pub var_count: usize,
    pub reg_start: usize,
    pub var_start: usize,
    pub stack_start: usize,
}

impl Prog {
    pub fn new(
        blocks: Vec<Block>,
        block_count: usize,
        var_count: usize,
        reg_start: usize,
        var_start: usize,
        stack_start: usize,
    ) -> Self {
        Prog {
            blocks,
            block_count,
            var_count,
            reg_start,
            var_start,
            stack_start,
        }
    }
}

pub fn f(prog: &intermediate::Prog) -> Prog {
    let block_count = prog.layout.block_count;
    let var_count = prog.layout.var_count;
    let reg_size = 256;

    // Memory Layout Constants
    // [0 .. block_count] : Block Flags
    // [block_count + 1 .. block_count + reg_size] : Registers
    // [block_count + reg_size + 1 .. ] : Variables

    // We add 1 to block_count for alignment or just separation?
    // The comment said [0..BlockSize] is Activate Block.
    // If BlockSize is N, indices are 0..N-1.
    // Registers start at BlockSize + 1. So index BlockSize is skipped.
    // Let's follow the comment strictly.
    let reg_start = block_count + 1;
    let var_start = reg_start + reg_size + 1; // +1 based on comment [BlockSize + RegSize + 1 ..]
    let stack_start = var_start + var_count * 32;

    let mut blocks = Vec::new();
    let block_map = &prog.layout.block_map;
    let var_map = &prog.layout.var_map;

    // Sort blocks by index to ensure order matches block_map values?
    // virtual::Block doesn't have ID. It's just a list of ops.
    // The interpreter will index into `blocks` vector using the block index.
    // So we must push blocks in the order of their indices.

    let mut sorted_blocks: Vec<(&id::L, &intermediate::Block)> =
        prog.blocks.iter().map(|b| (&b.id, b)).collect();
    // We need to sort by the index assigned in layout.
    sorted_blocks.sort_by_key(|(id, _)| *block_map.get(*id).unwrap());

    for (_, block) in sorted_blocks {
        let mut ops = Vec::new();
        convert_term(
            &block.term,
            &mut ops,
            var_map,
            block_map,
            reg_start,
            var_start,
            stack_start,
        );
        blocks.push(Block { ops: ops });
    }

    Prog::new(
        blocks,
        block_count,
        var_count,
        reg_start,
        var_start,
        stack_start,
    )
}

fn convert_term(
    term: &Term,
    ops: &mut Vec<Operation>,
    var_map: &HashMap<id::T, usize>,
    block_map: &HashMap<id::L, usize>,
    reg_start: usize,
    var_start: usize,
    stack_start: usize,
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
            );
            convert_term(
                e,
                ops,
                var_map,
                block_map,
                reg_start,
                var_start,
                stack_start,
            );
        }
        Term::Jump(l) => {
            if let Some(target_idx) = block_map.get(l) {
                // Unconditional jump to block
                ops.push(Operation::Jump(*target_idx as u32));
            } else if let Some(offset) = var_map.get(l) {
                // Jump to address stored in variable
                let addr = (var_start + offset * 32) as u32;
                // Replace JumpIndirect with OutputByte
                ops.push(Operation::OutputByte(addr));
            } else {
                // External call
                ops.push(Operation::CallExternal(l.clone()));
            }
        }
        Term::IfEq(x, y, l1, l2) => {
            let addr_x = (var_start + var_map.get(x).unwrap() * 32) as u32;
            let addr_y = (var_start + var_map.get(y).unwrap() * 32) as u32;
            let idx_l1 = *block_map.get(l1).unwrap() as u32;
            let idx_l2 = *block_map.get(l2).unwrap() as u32;

            // Use a temp register for comparison
            let tmp_addr = reg_start as u32;

            // tmp = x - y
            ops.push(Operation::Sub(tmp_addr, addr_x, addr_y));

            // if tmp == 0 goto l1 else goto l2
            // JumpIfZero(cond, true_label_flag_addr, false_label_flag_addr)
            ops.push(Operation::JumpIfZero(tmp_addr, idx_l1, idx_l2));
        }
        Term::IfLE(x, y, l1, l2) => {
            let addr_x = (var_start + var_map.get(x).unwrap() * 32) as u32;
            let addr_y = (var_start + var_map.get(y).unwrap() * 32) as u32;
            let idx_l1 = *block_map.get(l1).unwrap() as u32;
            let idx_l2 = *block_map.get(l2).unwrap() as u32;

            let tmp_addr = reg_start as u32;

            // tmp = max(0, x - y)
            // if x <= y, x - y <= 0, so max(0, x-y) == 0.
            ops.push(Operation::SubZ(tmp_addr, addr_x, addr_y));

            ops.push(Operation::JumpIfZero(tmp_addr, idx_l1, idx_l2));
        }
        Term::Atom(_) => panic!("Atom at tail position should not happen in blocked IR"),
        Term::LetTuple(_, _, _) => panic!("LetTuple not supported in virtual BF"),
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
) {
    match atom {
        Atom::Unit => {
            ops.push(Operation::SetImm(dest_addr, 0));
        }
        Atom::Int(i) => {
            ops.push(Operation::SetImm(dest_addr, *i));
        }
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
        Atom::GetStack(i) => {
            let src_addr = (stack_start + i * 32) as u32;
            ops.push(Operation::MoveData(dest_addr, src_addr, 32));
        }
        Atom::SetArgs(xs) => {
            for (i, x) in xs.iter().enumerate() {
                let dst_addr = (stack_start + i * 32) as u32;
                if let Some(offset) = var_map.get(x) {
                    let src_addr = (var_start + offset * 32) as u32;
                    ops.push(Operation::MoveData(dst_addr, src_addr, 32));
                } else if let Some(block_idx) = block_map.get(x) {
                    // It's a label (continuation), pass the block index
                    ops.push(Operation::SetImm(dst_addr, *block_idx as i32));
                } else {
                    panic!("SetArgs: Variable or Label not found: {}", x);
                }
            }
            // SetArgs returns Unit, so set dest to 0
            ops.push(Operation::SetImm(dest_addr, 0));
        }
        Atom::Neg(x) => {
            let src_addr = (var_start + var_map.get(x).unwrap() * 32) as u32;
            ops.push(Operation::Neg(dest_addr, src_addr));
        }
        _ => panic!("Unsupported atom: {:?}", atom),
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
            Operation::MoveData(dest, src, size) => {
                write!(f, "MoveData({}, {}, {})", dest, src, size)
            }
            Operation::CallExternal(name) => write!(f, "CallExternal({})", name),
            Operation::InputByte(addr) => write!(f, "InputByte({})", addr),
            Operation::OutputByte(addr) => write!(f, "OutputByte({})", addr),
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
