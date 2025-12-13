use crate::r#virtual::{Operation, Prog};
use std::convert::TryInto;
use std::io::{self, Write};

pub struct Simulator {
    pub memory: Vec<i32>, // Using i32 to store cell values (0/1 usually, but allows debug)
    pub pc: usize,        // Current Block Index
    pub steps: usize,
    pub max_steps: usize,
    pub running: bool,
    pub output: Vec<u8>,
}

impl Simulator {
    pub fn new(prog: &Prog, max_steps: usize) -> Self {
        // Memory size: enough for stack.
        // Stack starts at prog.stack_start.
        // Let's allocate reasonably large memory.
        let mem_size = prog.stack_start + 1000000; // Increased memory size for CPS stack usage

        // Initialize SP
        // SP starts at prog.sp_addr (variable holding SP value).
        // The *value* of SP should be stack_start.
        // But in `impl Simulator`, `write_int` writes to memory.
        // So we need `sim` instance first.

        let mut sim = Simulator {
            memory: vec![0; mem_size],
            pc: 0, // Entry block is expected to be index 0
            steps: 0,
            max_steps,
            running: true,
            output: Vec::new(),
        };

        // Initialize SP register to point to stack_start
        sim.write_int(prog.sp_addr, prog.stack_start as i32);

        sim
    }

    pub fn run(&mut self, prog: &Prog) -> Result<(), String> {
        self.pc = 0; // Entry block is index 0 based on emit.rs init
        self.running = true;

        while self.running && self.steps < self.max_steps {
            if self.pc >= prog.blocks.len() {
                return Err(format!("PC out of bounds: {}", self.pc));
            }

            let block = &prog.blocks[self.pc];
            eprintln!("DEBUG: Executing Block {}", self.pc);

            // Execute Ops
            let current_pc = self.pc;
            for op in &block.ops {
                self.execute_op(op, prog)?;
                if !self.running {
                    break;
                }
                if self.pc != current_pc {
                    // Jump occurred
                    break;
                }
            }

            // Standard PC increment if no Jump
            if self.pc == current_pc {
                self.pc += 1;
            }

            self.steps += 1;
        }

        if self.steps >= self.max_steps {
            return Err("Max steps reached".to_string());
        }

        Ok(())
    }

    fn execute_op(&mut self, op: &Operation, prog: &Prog) -> Result<(), String> {
        match op {
            Operation::SetImm(dest, val) => {
                self.write_int(*dest as usize, *val);
            }
            Operation::Halt => {
                // Stop execution
                println!("Halt executed.");
                panic!("HALT");
            }
            Operation::Neg(dest, src) => {
                let val = self.read_int(*src as usize);
                // 32-bit wrapping negation
                // In BF emit, it relies on wrapping ?
                // rust `wrapping_neg` for i32.
                let res = val.wrapping_neg();
                self.write_int(*dest as usize, res);
            }
            Operation::Add(dest, src1, src2) => {
                let v1 = self.read_int(*src1 as usize);
                let v2 = self.read_int(*src2 as usize);
                let res = v1.wrapping_add(v2);
                self.write_int(*dest as usize, res);
            }
            Operation::Sub(dest, src1, src2) => {
                let v1 = self.read_int(*src1 as usize);
                let v2 = self.read_int(*src2 as usize);
                let res = v1.wrapping_sub(v2);
                self.write_int(*dest as usize, res);
            }
            Operation::SubZ(dest, src1, src2) => {
                let v1 = self.read_int(*src1 as usize);
                let v2 = self.read_int(*src2 as usize);
                let res = if v1 <= v2 { 0 } else { v1 - v2 };
                self.write_int(*dest as usize, res);
            }
            Operation::JumpIfZero(addr, l1, l2) => {
                let val = self.read_int(*addr as usize);
                if val == 0 {
                    self.pc = (*l1 - 1) as usize;
                } else {
                    self.pc = (*l2 - 1) as usize;
                }
            }
            Operation::JumpIfLE(addr, l1, l2) => {
                let val = self.read_int(*addr as usize);
                if val <= 0 {
                    self.pc = (*l1 - 1) as usize;
                } else {
                    self.pc = (*l2 - 1) as usize;
                }
            }
            Operation::Jump(target) => {
                self.pc = (*target - 1) as usize;
            }
            Operation::JumpVar(src) => {
                // `src` holds the block index (1-based)
                let target = self.read_int(*src as usize);
                if target == 0 {
                    return Err("Jump to Block 0 (Invalid)".to_string());
                }
                self.pc = (target - 1) as usize;
            }
            Operation::MoveData(dest, src, size_bytes) => {
                // size is in bytes?
                // virtual.rs: `MoveData(u32, u32, usize)` -> size is in 'bf cells' or 'bytes'?
                // emit.rs calls `copy(..., size)`.
                // virtual.rs:377: `MoveData(..., 32)`.
                // So size is count of cells.
                let count = *size_bytes as usize;
                for i in 0..count {
                    let val = self.memory[*src as usize + i];
                    self.memory[*dest as usize + i] = val;
                }
            }
            Operation::CallExternal(name) => {
                if name == "halt" {
                    self.running = false;
                    // eprintln!("DEBUG: Halt called"); // Optional debug
                } else if name == "print_int" || name == "min_caml_print_int" {
                    // Stack-Only Convention: [Val, K] (Top is K)
                    // No Self.

                    let sp_addr = prog.sp_addr;
                    let mut sp = self.read_int(sp_addr) as usize;

                    // Peek values
                    // [sp-32] = K (Continuation Label)
                    // [sp-64] = Val (Value to print)

                    let k_label = self.read_int(sp - 32);
                    let val = self.read_int(sp - 64);

                    // Print
                    println!("{}", val);
                    self.output
                        .extend_from_slice(format!("{}\n", val).as_bytes());

                    // Pop arguments (Val, K) -> 2 * 32 = 64 bytes
                    sp -= 64;

                    // Call Continuation: K(Unit)
                    // Push Unit (Result 0)
                    self.write_int(sp, 0);
                    sp += 32;

                    // Update SP
                    self.write_int(sp_addr, sp as i32);

                    // Jump to K (Label)
                    // k_label is now 0-based block index.
                    // 0 is valid (main or entry).
                    self.pc = k_label as usize;
                } else {
                    eprintln!("Warning: Unknown external call {}", name);
                }
            }
            Operation::InputByte(addr) => {
                // Not supported for now
            }
            Operation::OutputByte(addr) => {
                let val = self.memory[*addr as usize];
                print!("{}", val as u8 as char);
            }
            Operation::Load(dest, src) => {
                let addr = self.read_int(*src as usize);
                let val = self.read_int(addr as usize);
                self.write_int(*dest as usize, val);
            }
            Operation::Store(dest, src) => {
                let addr = self.read_int(*dest as usize);
                let val = self.read_int(*src as usize);
                self.write_int(addr as usize, val);
            }
            Operation::Push(src) => {
                let sp_addr = prog.sp_addr;
                let sp = self.read_int(sp_addr); // Read current SP
                let val = self.read_int(*src as usize); // Read value to push

                self.write_int(sp as usize, val); // *sp = val

                let new_sp = sp + 32; // sp += 32
                self.write_int(sp_addr, new_sp);
            }
            Operation::Pop(dest) => {
                let sp_addr = prog.sp_addr;
                let mut sp = self.read_int(sp_addr); // Read current SP

                sp -= 32; // sp -= 32
                let val = self.read_int(sp as usize); // val = *sp

                self.write_int(*dest as usize, val); // *dest = val
                self.write_int(sp_addr, sp);
            }
        }
        Ok(())
    }

    pub fn read_int(&self, addr: usize) -> i32 {
        let mut res: i32 = 0;
        for i in 0..32 {
            if self.memory[addr + i] != 0 {
                res |= 1 << i;
            }
        }
        res
    }

    pub fn write_int(&mut self, addr: usize, val: i32) {
        for i in 0..32 {
            self.memory[addr + i] = if (val & (1 << i)) != 0 { 1 } else { 0 };
        }
    }
}
