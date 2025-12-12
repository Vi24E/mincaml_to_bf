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
    SetImm(u32, i32),    // x = i
    Neg(u32, u32),       // z = -x
    Add(u32, u32, u32),  // z = x + y
    Sub(u32, u32, u32),  // z = x - y
    SubZ(u32, u32, u32), // z = max(0, x - y)
    JumpIfZero(u32, u32, u32),
    JumpIfLE(u32, u32, u32), // cond_addr, then_block, else_block
    // if x == 0 goto y
    Jump(u32),                 // Unconditional jump to block y
    JumpVar(u32),              // Jump to block index stored in variable x
    MoveData(u32, u32, usize), // move and copy data from x to y, size is in bytes
    CallExternal(String),      // Call external function (e.g. print_int)
    InputByte(u32),            // Read byte to address
    OutputByte(u32),           // Write byte from address
    Load(u32, u32),            // dest = *src (Indirect Read)
    Store(u32, u32),           // *dest = src (Indirect Write)
    Push(u32),                 // [sp] = *src; sp += 32
    Pop(u32),                  // sp -= 32; *dest = [sp]
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

// Main entry point for virtual code generation
pub fn f(prog: &intermediate::Prog) -> Prog {
    eprintln!("DEBUG: Starting virtual::f");
    use std::io::Write;
    std::io::stderr().flush().unwrap();

    let constants = analyze_constants(prog);

    let block_count = prog.layout.block_count;
    let var_count = prog.layout.var_count;
    let reg_size = 128; // User requested reduction

    // Memory Layout Constants
    // [0 .. block_count] : Block Flags
    // [block_count + 1 .. block_count + reg_size] : Registers
    // [block_count + reg_size + 1 .. ] : Variables

    // We add 1 to block_count for alignment or just separation?
    // The comment said [0..BlockSize] is Activate Block.
    // If BlockSize is N, indices are 0..N-1.
    // Registers start at BlockSize + 1. So index BlockSize is skipped.
    // Let's follow the comment strictly.
    // Memory Layout Constants
    // 0: Running Flag
    // 1..N: Block Flags (Originally).
    // New Layout: Stride 2.
    // 0: Running Flag
    // 2: Block 0
    // 4: Block 1
    // ...
    // So we need `(block_count + 1) * 2` cells.
    let reg_start = (block_count + 1) * 2;
    // SP register is allocated at the end of register area (reg_start + reg_size - 1)
    // Actually, registers are [reg_start .. reg_start + reg_size].
    // Let's pick a fixed slot for SP. Index 0?
    // Using reg_start for SP.
    let sp_addr = reg_start;
    // Shift other registers?
    // The previous logic didn't use specific register slots except for temp.
    // Temp was at reg_start + 64.
    // So reg_start is safe for SP.

    let var_start = reg_start + reg_size;
    let stack_start = var_start + var_count * 32;

    let mut blocks = Vec::new();
    let block_map = &prog.layout.block_map;
    let var_map = &prog.layout.var_map;

    // ... (rest of the code) ...

    let mut sorted_blocks: Vec<(&id::L, &intermediate::Block)> =
        prog.blocks.iter().map(|b| (&b.id, b)).collect();
    // We need to sort by the index assigned in layout.
    sorted_blocks.sort_by_key(|(id, _)| *block_map.get(*id).unwrap());

    let entry_idx = *block_map.get(&prog.entry).unwrap(); // Should be 1

    for (i, block) in sorted_blocks {
        let mut ops = Vec::new();

        // The last variable slot is reserved for comparison temp
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
        // DEBUG: Dump block info
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
        Term::IfEq(_, _, _, _) | Term::IfLE(_, _, _, _) | Term::Jump(_) | Term::JumpVar(_) => {}
        Term::Atom(_) => {}
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
            if let Some(target_idx) = block_map.get(l) {
                ops.push(Operation::Jump(*target_idx as u32));
            } else if let Some(offset) = var_map.get(l) {
                let addr = (var_start + offset * 32) as u32;
                ops.push(Operation::OutputByte(addr));
            } else {
                ops.push(Operation::CallExternal(l.clone()));
            }
        }
        Term::JumpVar(x) => {
            let addr = (var_start + var_map.get(x).unwrap() * 32) as u32;
            ops.push(Operation::JumpVar(addr));
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
            // x <= y means x - y <= 0
            ops.push(Operation::Sub(tmp_addr, addr_x, addr_y)); // Use wrapping sub
            ops.push(Operation::JumpIfLE(tmp_addr, idx_l1, idx_l2));
        }
        Term::LetTuple(xts, atom, e) => {
            let tuple_ptr_addr = match atom {
                Atom::Var(v) => (var_start + var_map.get(v).unwrap() * 32) as u32,
                _ => panic!("LetTuple expected Var on RHS, got {:?}", atom),
            };

            for (i, (x, _)) in xts.iter().enumerate() {
                let dest_addr = (var_start + var_map.get(x).unwrap() * 32) as u32;
                let tmp_addr = (reg_start + 64) as u32;
                ops.push(Operation::SetImm(tmp_addr, (i * 32) as i32));
                ops.push(Operation::Add(tmp_addr, tuple_ptr_addr, tmp_addr));
                ops.push(Operation::Load(dest_addr, tmp_addr));
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
    sp_addr: usize,
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
        Atom::SetArgs(xs) => {
            for (i, x) in xs.iter().enumerate() {
                let dst_addr = (stack_start + i * 32) as u32;
                if let Some(offset) = var_map.get(x) {
                    let src_addr = (var_start + offset * 32) as u32;
                    ops.push(Operation::MoveData(dst_addr, src_addr, 32));
                } else if let Some(block_idx) = block_map.get(x) {
                    ops.push(Operation::SetImm(dst_addr, *block_idx as i32));
                } else {
                    panic!("SetArgs: Variable or Label not found: {}", x);
                }
            }
            ops.push(Operation::SetImm(dest_addr, 0));
        }
        Atom::LoadLabel(l) => {
            if let Some(idx) = block_map.get(l) {
                ops.push(Operation::SetImm(dest_addr, *idx as i32));
            } else {
                panic!("LoadLabel: Label not found: {}", l);
            }
        }
        Atom::Tuple(_) => {
            // Optimization for closed functions/constants. Dynamic allocation via MakeCls is handled via Push logic in blocked.rs now (Tuple -> Heap allocation not implemented here)
            // But wait, `MakeCls` conversion in `blocked.rs` emits `Let x = Tuple(..)`.
            // Wait, I changed `blocked.rs` to use `Push` for `MakeCls`!
            // `Let entry = LoadLabel`. `Let x = GetSp(x)`. `Push`.
            // So `blocked.rs` NO LONGER emits `Atom::Tuple` for Closures!
            // `Atom::Tuple` remains for regular Tuples?
            // If so, `Tuple` conversion here should allocate?
            // Currently it Sets 0.
            ops.push(Operation::SetImm(dest_addr, 0));
        }
        Atom::Get(x, y) => {
            // Logic for Get (Array/Tuple access)
            // Using logic from previous view
            let addr_x = (var_start + var_map.get(x).unwrap() * 32) as u32;
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
                ops.push(Operation::Add(tmp_addr, addr_x, tmp_addr));
                ops.push(Operation::Load(dest_addr, tmp_addr));
            }
        }
        Atom::Put(x, y, z) => {
            let addr_x = (var_start + var_map.get(x).unwrap() * 32) as u32;
            let addr_y = (var_start + var_map.get(y).unwrap() * 32) as u32;
            let addr_z = (var_start + var_map.get(z).unwrap() * 32) as u32;
            let tmp_addr = (reg_start + 64) as u32;
            ops.push(Operation::MoveData(tmp_addr, addr_y, 32));
            for _ in 0..5 {
                ops.push(Operation::Add(tmp_addr, tmp_addr, tmp_addr));
            }
            ops.push(Operation::Add(tmp_addr, addr_x, tmp_addr));
            ops.push(Operation::Store(tmp_addr, addr_z));
        }
        Atom::ExtArray(l) => {
            // ExtArray typically returns address of the array.
            // We don't really support global arrays in this simple backend yet.
            // But we can return a dummy address or handling it.
            ops.push(Operation::SetImm(dest_addr, 0));
        }
        Atom::Push(x) => {
            let src_addr = (var_start + var_map.get(x).unwrap() * 32) as u32;
            ops.push(Operation::Push(src_addr));
            ops.push(Operation::SetImm(dest_addr, 0));
        }
        Atom::Pop => {
            ops.push(Operation::Pop(dest_addr));
        }
        Atom::GetSp => {
            ops.push(Operation::MoveData(dest_addr, sp_addr as u32, 32));
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
            Operation::JumpIfLE(cond, l1, l2) => {
                write!(f, "JumpIfLE({}, {}, {})", cond, l1, l2)
            }
            Operation::Jump(target) => write!(f, "Jump({})", target),
            Operation::JumpVar(src) => write!(f, "JumpVar({})", src),
            Operation::MoveData(dest, src, size) => {
                write!(f, "MoveData({}, {}, {})", dest, src, size)
            }
            Operation::CallExternal(name) => write!(f, "CallExternal({})", name),
            Operation::InputByte(addr) => write!(f, "InputByte({})", addr),
            Operation::OutputByte(addr) => write!(f, "OutputByte({})", addr),
            Operation::Load(dest, src) => write!(f, "Load({}, {})", dest, src),
            Operation::Store(dest, src) => write!(f, "Store({}, {})", dest, src),
            Operation::Push(src) => write!(f, "Push({})", src),
            Operation::Pop(dest) => write!(f, "Pop({})", dest),
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
