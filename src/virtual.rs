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
    JumpVar(u32),              // Jump to block index stored in variable x
    MoveData(u32, u32, usize), // move and copy data from x to y, size is in bytes
    CallExternal(String),      // Call external function (e.g. print_int)
    InputByte(u32),            // Read byte to address
    OutputByte(u32),           // Write byte from address
    Load(u32, u32),            // dest = *src (Indirect Read)
    Store(u32, u32),           // *dest = src (Indirect Write)
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
    let var_start = reg_start + reg_size;
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

    // Initialize reg_hp
    // Create an initialization block at index 0?
    // No, index 0 is Main? No, 0 is Entry Block?
    // Intermediate layout: "Ensure entry block is index 1".
    // 0 is usually the "Start of Program" implicit logic in emit.rs.
    // emit.rs manually activates block 1.
    // We can inject `SetImm(reg_hp, heap_start)` into the first block (Entry)?
    // Or simpler: Just emit it at the beginning of `blocks`.

    // We need to find the entry block and Prepend op.
    // Or just create a new Block 1 that inits HP and jumps to real Entry?
    // But Block 1 is `entry_label`.

    // Let's prepend to the actual entry block.
    // Which block corresponds to entry_label?
    // block_map[entry_label] == 1.

    let entry_idx = *block_map.get(&prog.entry).unwrap(); // Should be 1

    for (_, block) in sorted_blocks {
        let mut ops = Vec::new();

        // If this is the entry block, init HP
        if *block_map.get(&block.id).unwrap() == entry_idx {
            let reg_hp = (reg_start + 120) as u32;
            let heap_start_addr = (stack_start + 1024) as i32;
            ops.push(Operation::SetImm(reg_hp, heap_start_addr));
        }

        // The last variable slot is reserved for comparison temp
        // ...
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
            &constants,
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
                constants,
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

            // Use reserved global temp variable for comparison to avoid collision
            let tmp_addr = cmp_temp_addr as u32;

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

            let tmp_addr = cmp_temp_addr as u32;

            // tmp = max(0, x - y)
            // if x <= y, x - y <= 0, so max(0, x-y) == 0.
            ops.push(Operation::SubZ(tmp_addr, addr_x, addr_y));

            ops.push(Operation::JumpIfZero(tmp_addr, idx_l1, idx_l2));
        }
        Term::JumpVar(x) => {
            // Unconditional jump to Dynamic Variable
            // The "running loop" checks if `running_flag` matches `block_id`.
            // So we just need to set `running_flag` to the value of variable `x`.
            // `running_flag` is at address 0.
            let addr_x = (var_start + var_map.get(x).unwrap() * 32) as u32;
            ops.push(Operation::JumpVar(addr_x));
        }
        Term::Atom(_) => panic!("Atom at tail position should not happen in blocked IR"),
        Term::LetTuple(xts, atom, e) => {
            // Let (x, y) = Tuple(...) in ...
            // Atom must be Atom::Tuple?
            // Or Atom::Var if it's already a tuple pointer?
            // blocked.rs handles LetTuple by just converting body.
            // But intermediate.rs has LetTuple struct.
            // We need to implement tuple destructuring.
            // x = atom[0], y = atom[1]...
            // If atom is Var(t), then x = *(t+0), y = *(t+1)

            let tuple_ptr_addr = match atom {
                Atom::Var(v) => (var_start + var_map.get(v).unwrap() * 32) as u32,
                _ => panic!("LetTuple expected Var on RHS, got {:?}", atom),
            };

            for (i, (x, _)) in xts.iter().enumerate() {
                let dest_addr = (var_start + var_map.get(x).unwrap() * 32) as u32;
                // dest = *(tuple_ptr + i*4) ?
                // No, heap is 32-bit aligned?
                // If tuple_ptr points to 32-bit cell index?
                // BF Memory is u32 array.
                // So `Load(dest, tuple_ptr + i*32)`.
                // We need to `Add` i*32 to tuple_ptr and Load.
                // We need a temporary register for address calculation.
                let tmp_addr = (reg_start + 64) as u32; // Scratch register

                // tmp = tuple_ptr + offset
                ops.push(Operation::SetImm(tmp_addr, (i * 32) as i32));
                ops.push(Operation::Add(tmp_addr, tuple_ptr_addr, tmp_addr));

                // dest = *tmp
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
                constants,
            );
        }
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
    constants: &HashMap<id::T, ConstVal>,
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
        Atom::LoadLabel(l) => {
            if let Some(idx) = block_map.get(l) {
                ops.push(Operation::SetImm(dest_addr, *idx as i32));
            } else {
                panic!("LoadLabel: Label not found: {}", l);
            }
        }
        Atom::Tuple(_xs) => {
            // Static Optimization: We assume all tuple usage is optimized away or unused dynamically (for closed functions).
            // So we don't need to allocate on heap.
            // Just set dest to 0.
            ops.push(Operation::SetImm(dest_addr, 0));
        }
        Atom::Get(x, y) => {
            // dest = x[y]
            let addr_x = (var_start
                + match var_map.get(x) {
                    Some(v) => *v,
                    None => {
                        use std::io::Write;
                        eprintln!("DEBUG: var_map keys: {:?}", var_map.keys());
                        std::io::stderr().flush().unwrap();
                        panic!(
                            "virtual::convert_atom: Get(x, y) - x not found in var_map: {}",
                            x
                        );
                    }
                } * 32) as u32;
            let addr_y = (var_start + var_map.get(y).unwrap() * 32) as u32;
            let dest_addr = dest_addr; // Make sure we have dest_addr in scope? It is argument.

            // Static Optimization: Check if x is a known Tuple constant and y is a known Int index
            let mut optim_success = false;

            // Resolve y to int index if possible
            let idx_val = if let Some(ConstVal::Int(i)) = constants.get(y) {
                Some(*i)
            } else {
                // Should check if y refers to something that is Int?
                // analyze_constants handles Int atoms.
                None
            };

            if let Some(idx) = idx_val {
                if let Some(ConstVal::Tuple(fields)) = constants.get(x) {
                    if idx >= 0 && (idx as usize) < fields.len() {
                        let field_var = &fields[idx as usize];
                        // Now we want the value of field_var.
                        // If field_var itself is a LoadLabel, we can emit SetImm(dest, block_idx).
                        if let Some(ConstVal::LoadLabel(l)) = constants.get(field_var) {
                            if let Some(block_idx) = block_map.get(l) {
                                // Optimized! Emit SetImm directly.
                                ops.push(Operation::SetImm(dest_addr, *block_idx as i32));
                                optim_success = true;
                            }
                        }
                    }
                }
            }

            if !optim_success {
                // Fallback to dynamic Load
                let addr_x = (var_start
                    + match var_map.get(x) {
                        Some(v) => *v,
                        None => {
                            use std::io::Write;
                            eprintln!("DEBUG: var_map keys: {:?}", var_map.keys());
                            std::io::stderr().flush().unwrap();
                            panic!(
                                "virtual::convert_atom: Get(x, y) - x not found in var_map: {}",
                                x
                            );
                        }
                    } * 32) as u32;
                let addr_y = (var_start + var_map.get(y).unwrap() * 32) as u32;

                let tmp_addr = (reg_start + 64) as u32;

                // y * 32
                ops.push(Operation::MoveData(tmp_addr, addr_y, 32));
                for _ in 0..5 {
                    ops.push(Operation::Add(tmp_addr, tmp_addr, tmp_addr));
                }

                // tmp = x + (y*32)
                ops.push(Operation::Add(tmp_addr, addr_x, tmp_addr));

                // dest = *tmp
                ops.push(Operation::Load(dest_addr, tmp_addr));
            }
        }
        Atom::Put(x, y, z) => {
            // x[y] = z
            let addr_x = (var_start + var_map.get(x).unwrap() * 32) as u32;
            let addr_y = (var_start + var_map.get(y).unwrap() * 32) as u32;
            let addr_z = (var_start + var_map.get(z).unwrap() * 32) as u32;

            let tmp_addr = (reg_start + 64) as u32;
            ops.push(Operation::MoveData(tmp_addr, addr_y, 32));
            for _ in 0..5 {
                ops.push(Operation::Add(tmp_addr, tmp_addr, tmp_addr));
            }
            ops.push(Operation::Add(tmp_addr, addr_x, tmp_addr));

            // *tmp = z
            ops.push(Operation::Store(tmp_addr, addr_z));
        }

        Atom::ExtArray(_) => panic!("ExtArray not supported"),
        Atom::Float(_)
        | Atom::FNeg(_)
        | Atom::FAdd(_, _)
        | Atom::FSub(_, _)
        | Atom::FMul(_, _)
        | Atom::FDiv(_, _) => {
            panic!("Float operations not supported")
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
            Operation::InputByte(addr) => write!(f, "InputByte({})", addr),
            Operation::OutputByte(addr) => write!(f, "OutputByte({})", addr),
            Operation::Load(dest, src) => write!(f, "Load({}, {})", dest, src),
            Operation::Store(dest, src) => write!(f, "Store({}, {})", dest, src),
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
