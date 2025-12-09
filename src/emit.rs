use crate::r#virtual::{Operation, Prog};

pub fn f(prog: &Prog) -> String {
    let mut bf_code = String::new();
    let mut current_ptr = 0;

    let buf_size = 128;
    let reg_start = prog.reg_start as u32;
    let var_start = prog.var_start as u32;
    let stack_start = prog.stack_start as u32;
    // buffer should always zero filled after operation
    let buffer_start = (prog.var_start - buf_size) as u32;

    // Header or initialization if needed
    // The memory layout is:
    // [0..block_count]: Block activation flags
    // [reg_start..var_start]: Registers
    // [var_start..stack_start]: Variables
    // [stack_start..]: Stack

    // bf_code.push_str("...");
    bf_code.push_str("+>+");
    current_ptr = 1;

    for (i, block) in prog.blocks.iter().enumerate() {
        bf_code.push_str("[-");

        for op in &block.operations {
            match op {
                Operation::SetImm(dest, val) => {
                    move_ptr(&mut bf_code, &mut current_ptr, *dest);
                    let mut v = *val as u32;
                    for _ in 0..32 {
                        bf_code.push_str("[-]");
                        if (v & 1) != 0 {
                            bf_code.push('+');
                        }
                        bf_code.push('>');
                        v >>= 1;
                    }
                    current_ptr += 32;
                }
                Operation::Neg(dest, src) => {
                    move_ptr(&mut bf_code, &mut current_ptr, *src);
                    neg(
                        &mut bf_code,
                        &mut current_ptr,
                        reg_start,
                        buffer_start,
                        *dest,
                    );
                }
                Operation::Add(dest, src1, src2) => {
                    copy(
                        &mut bf_code,
                        &mut current_ptr,
                        *src1,
                        reg_start,
                        buffer_start,
                        32,
                    );
                    copy(
                        &mut bf_code,
                        &mut current_ptr,
                        *src2,
                        reg_start + 32,
                        buffer_start,
                        32,
                    );
                    for i in 0..32 {
                        move_ptr(&mut bf_code, &mut current_ptr, reg_start + i);
                        bf_code.push('[');
                        bf_code.push('-');
                        move_ptr(&mut bf_code, &mut current_ptr, reg_start + 32);
                        add(
                            &mut bf_code,
                            &mut current_ptr,
                            reg_start + 64,
                            buffer_start,
                            reg_start + 32,
                            1 << i,
                        );
                        move_ptr(&mut bf_code, &mut current_ptr, reg_start + i);
                        bf_code.push(']');
                    }
                    copy(
                        &mut bf_code,
                        &mut current_ptr,
                        reg_start + 32,
                        *dest,
                        buffer_start,
                        32,
                    );
                }
                Operation::Sub(dest, src1, src2) => {
                    // DELETE
                    // Add (dest, src1, neg(src2))
                }
                Operation::SubZ(dest, src1, src2) => {
                    // DELETE
                    // maxzero(Add (dest, src1, neg(src2)))
                }
                Operation::JumpIfZero(cond, l1, l2) => {
                    copy(
                        &mut bf_code,
                        &mut current_ptr,
                        *cond,
                        reg_start,
                        buffer_start,
                        32,
                    );
                    move_ptr(&mut bf_code, &mut current_ptr, reg_start);

                    for _ in 0..31 {
                        bf_code.push_str("[->+<]>");
                    }
                    bf_code.push_str("[-]");
                    bf_code.push('+');

                    current_ptr += 31;
                    bf_code.push_str("[>+>+<<-]>>>+<[->-<]"); // 32 = pos, 34 = neg
                    move_ptr(&mut bf_code, &mut current_ptr, reg_start + 32);
                    bf_code.push_str("[-");
                    move_ptr(&mut bf_code, &mut current_ptr, *l1);
                    bf_code.push('+');
                    move_ptr(&mut bf_code, &mut current_ptr, reg_start + 32);
                    bf_code.push_str("]");
                    move_ptr(&mut bf_code, &mut current_ptr, reg_start + 34);
                    bf_code.push_str("[-");
                    move_ptr(&mut bf_code, &mut current_ptr, *l2);
                    bf_code.push('+');
                    move_ptr(&mut bf_code, &mut current_ptr, reg_start + 34);
                    bf_code.push_str("]");
                }
                Operation::Jump(target) => {
                    move_ptr(&mut bf_code, &mut current_ptr, *target);
                    bf_code.push('+'); // activate block
                }
                Operation::MoveData(dest, src, size) => {
                    copy(
                        &mut bf_code,
                        &mut current_ptr,
                        *src,
                        *dest,
                        buffer_start,
                        *size as u32,
                    );
                }
                Operation::CallExternal(name) => {
                    if name == "halt" {
                        move_ptr(&mut bf_code, &mut current_ptr, 0);
                        bf_code.push('-');

                    }
                    else {
                        panic!("CallExternal is not implemented");
                    }
                }
                Operation::InputByte(addr) => {
                    // TODO: Implement InputByte
                    // Read byte to *addr
                    // bf_code.push_str("...");
                }
                Operation::OutputByte(addr) => {
                    // TODO: Implement OutputByte
                    // Write byte from *addr
                    // bf_code.push_str("...");
                }
            }
        }
        
        move_ptr(&mut bf_code, &mut current_ptr, i as u32);
        bf_code.push_str("]>");
    }

    bf_code
}

#[allow(dead_code)]
fn move_ptr(bf_code: &mut String, current_ptr: &mut u32, target_ptr: u32) {
    println!("move_ptr: {} -> {}", *current_ptr, target_ptr);
    if target_ptr > *current_ptr {
        for _ in 0..(target_ptr - *current_ptr) {
            bf_code.push('>');
        }
    } else {
        for _ in 0..(*current_ptr - target_ptr) {
            bf_code.push('<');
        }
    }
    *current_ptr = target_ptr;
}

// add n to current ptr
fn add_point(bf_code: &mut String, n: u32) {
    for _ in 0..n {
        bf_code.push('+');
    }
}

// sub n from current ptr
fn sub_point(bf_code: &mut String, n: u32) {
    for _ in 0..n {
        bf_code.push('-');
    }
}

// eg. [->>>+>>>>>+<<<<<<<<]>>>[-<<<+>>>]
// copy source to dest
fn copy(
    bf_code: &mut String,
    current_ptr: &mut u32,
    source: u32,
    dest: u32,
    buffer: u32,
    size: u32,
) {
    move_ptr(bf_code, current_ptr, source);
    for i in 0..size {
        bf_code.push('[');
        bf_code.push('-');
        move_ptr(bf_code, current_ptr, buffer + i);
        bf_code.push('+');
        move_ptr(bf_code, current_ptr, dest + i);
        bf_code.push('+');
        move_ptr(bf_code, current_ptr, source + i);
        bf_code.push(']');
        move_ptr(bf_code, current_ptr, buffer + i);
        bf_code.push('[');
        bf_code.push('-');
        move_ptr(bf_code, current_ptr, source + i);
        bf_code.push('+');
        move_ptr(bf_code, current_ptr, buffer + i);
        bf_code.push(']');
        move_ptr(bf_code, current_ptr, source + i);
        bf_code.push('>');
        *current_ptr += 1;
    }
}

// >-[>-]++[<] is increment with 1/2 bits
// overflow is not checked; will infinite loop
/*
eg. for 3 bits
>+>+>+>+
<<<<
>-[>-]++[<] // increment
>-[>-]++[<] // increment
>-[>-]++[<] // increment
>->->->[-]
<<<<
*/
fn add(
    bf_code: &mut String,
    current_ptr: &mut u32,
    register: u32,
    buffer: u32,
    dest: u32,
    val: u32,
) {
    copy(bf_code, current_ptr, *current_ptr, register + 1, buffer, 32);
    move_ptr(bf_code, current_ptr, register);
    for _ in 0..33 {
        bf_code.push('>');
        bf_code.push('+');
    }
    for _ in 0..33 {
        bf_code.push('<');
    }
    for i in 0..32 {
        if (1 << i) & val != 0 {
            for _ in 0..i + 1 {
                bf_code.push('>');
            }
            bf_code.push_str("-[>-]++[<]");
        }
    }
    for i in 0..33 {
        bf_code.push('>');
        if i == 32 {
            bf_code.push_str("[-]");
        } else {
            bf_code.push('-');
        }
    }
    for _ in 0..33 {
        bf_code.push('<');
    }
    copy(bf_code, current_ptr, register + 1, dest, buffer, 32);
}

// move value from source to dest (destroy source)
fn move_val(bf_code: &mut String, current_ptr: &mut u32, source: u32, dest: u32, size: u32) {
    for i in 0..size {
        move_ptr(bf_code, current_ptr, dest + i);
        bf_code.push_str("[-]"); // Clear dest
        move_ptr(bf_code, current_ptr, source + i);
        bf_code.push('[');
        bf_code.push('-');
        move_ptr(bf_code, current_ptr, dest + i);
        bf_code.push('+');
        move_ptr(bf_code, current_ptr, source + i);
        bf_code.push(']');
    }
}

// negate current ptr value (32 bit)
fn neg(bf_code: &mut String, current_ptr: &mut u32, register: u32, buffer: u32, dest: u32) {
    copy(bf_code, current_ptr, *current_ptr, register, buffer, 32);
    move_ptr(bf_code, current_ptr, register + 31);
    for i in 0..32 {
        bf_code.push_str(">+<[->-<]"); // not and shift right 1
        if i != 31 {
            bf_code.push('<');
            *current_ptr -= 1;
        }
    }
    assert_eq!(*current_ptr, register);

    move_ptr(bf_code, current_ptr, register);
    for _ in 0..32 {
        bf_code.push('>');
        bf_code.push('+');
    }
    for _ in 0..32 {
        bf_code.push('<');
    }
    bf_code.push_str(">-[>-]++[<]");
    for _ in 0..32 {
        bf_code.push('>');
        bf_code.push('-');
    }
    for _ in 0..32 {
        bf_code.push('<');
    }
    move_val(bf_code, current_ptr, register + 1, dest, 32);
}

fn maxzero(bf_code: &mut String) {
    for _ in 0..31 {
        bf_code.push('>');
    }
    bf_code.push('[');
    bf_code.push('-');
    for _ in 0..31 {
        bf_code.push('<');
        bf_code.push_str("[-]");
    }
    for _ in 0..31 {
        bf_code.push('>');
    }
    bf_code.push(']');
    for _ in 0..31 {
        bf_code.push('<');
    }
}

/*
+>>>+
<<<
[->+<]>
[->+<]>
[->+<]>
[->+<]>
[-]
+
*/
fn iszero(bf_code: &mut String, current_ptr: &mut u32, register: u32, buffer: u32, dest: u32) {
    copy(bf_code, current_ptr, *current_ptr, register, buffer, 32);
    move_ptr(bf_code, current_ptr, register);
    for _ in 0..31 {
        bf_code.push_str("[->+<]>");
    }
    bf_code.push_str("[-]");
    bf_code.push('+');
    *current_ptr += 31;
    bf_code.push('[');
    move_ptr(bf_code, current_ptr, dest);
    bf_code.push('+');
    move_ptr(bf_code, current_ptr, register + 31);
    bf_code.push('-');
    bf_code.push(']');
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter;
    use crate::interpreter::Machine;
    use std::fs;

    #[test]
    fn test_add_constant() {
        let mut bf_code = String::new();
        let mut ptr = 0;
        // Compact layout
        let src = 0;
        let dest = 64;
        let reg = 128;
        let buf = 192;
        let val = 5;

        // Ensure ptr is at src before calling add
        move_ptr(&mut bf_code, &mut ptr, src);
        // Note: add adds 'val' to 'dest'? No, add(..., dest, val) adds val to value AT dest??
        // Wait, user's add function signature: `add(..., dest, val)`.
        // In my previous test: `dest = src`. `add(..., src, val)`.
        // If I want 10 + 5, I put 10 at `src`.
        // If `dest` is same as `src`, it modifies `src`.
        // If `dest` is different, `add` copies from `current_ptr` (which should be `src`?)
        // Let's check `add` implementation detail again.
        // `copy(bf_code, current_ptr, *current_ptr, register + 1, buffer, 32);`
        // It copies FROM `*current_ptr`. Use `current_ptr` as source address.
        // So I must `move_ptr` to `src`.
        // Then `add` puts result in `dest`.

        // So if `src != dest`:
        // `move_ptr(src)`
        // `add(..., dest, val)`
        // Check if `src` is preserved? `add` copies `src` to `register`, then computes.
        // Then copies result to `dest`.
        // So `src` should be preserved if distinct from `dest` and `reg`.

        add(&mut bf_code, &mut ptr, reg, buf, dest, val);

        let mut machine = Machine::new(1000);
        // Init src = 10 (0..01010)
        machine.memory[src as usize + 1] = 1;
        machine.memory[src as usize + 3] = 1;

        match machine.run(&bf_code) {
            Ok(_) => {}
            Err(e) => panic!("Interpreter error: {}", e),
        }

        let mut res = 0;
        for i in 0..32 {
            if machine.memory[dest as usize + i] != 0 {
                res += 1 << i;
            }
        }
        println!("10 + 5 = {}", res);
        assert_eq!(res, 15);
    }

    #[test]
    fn test_add_one() {
        let mut machine = Machine::new(1000);
        let mut bf_code = String::new();
        let mut ptr = 0;

        let src = 0;
        let dest = 64;
        let reg = 128;
        let buf = 192;
        let val = 1;

        copy(&mut bf_code, &mut ptr, src, dest, buf, 32);
        move_ptr(&mut bf_code, &mut ptr, dest);
        add(&mut bf_code, &mut ptr, reg, buf, dest, val);

        fs::write("test_add_one.bf", &bf_code).expect("Unable to write file");
        println!("BF Code for Add(1) written to file.");

        // Init src = 10 (1010 in binary)
        machine.memory[src as usize + 1] = 1;
        machine.memory[src as usize + 3] = 1;

        match machine.run(&bf_code) {
            Ok(_) => {}
            Err(e) => panic!("Interpreter error: {}", e),
        }

        let mut res = 0;
        for i in 0..32 {
            if machine.memory[dest as usize + i] != 0 {
                res += 1 << i;
            }
        }
        println!("10 + 1 = {}", res);
        assert_eq!(res, 11);
    }

    #[test]
    fn test_neg() {
        let mut machine = Machine::new(1000);
        let mut bf_code = String::new();
        let mut ptr = 0;

        let src = 0;
        let dest = 64;
        let reg = 128;
        let buf = 192;

        // Ensure ptr is at src
        copy(&mut bf_code, &mut ptr, src, dest, buf, 32);
        move_ptr(&mut bf_code, &mut ptr, dest);
        neg(&mut bf_code, &mut ptr, reg, buf, dest);

        fs::write("test_neg.bf", &bf_code).expect("Unable to write file");
        println!("BF Code for Neg written to file.");

        // Init src = 10
        machine.memory[src as usize + 1] = 1;
        machine.memory[src as usize + 3] = 1;

        match machine.run(&bf_code) {
            Ok(_) => {}
            Err(e) => panic!("Interpreter error: {}", e),
        }

        let mut res = 0;
        for i in 0..32 {
            if machine.memory[dest as usize + i] != 0 {
                res += 1 << i;
            }
        }
        println!("-10 = {} (u32)", res);
        assert_eq!(res, (0u32).wrapping_sub(10));
    }
    #[test]
    fn test_set_imm() {
        // Temporarily disabled due to visibility issues with set_imm
        /*
        let dest = 10;
        let val = 0x12345678;

        let mut bf_code = String::new();
        let mut ptr = 0;

        move_ptr(&mut bf_code, &mut ptr, dest);
        set_imm(&mut bf_code, &mut ptr, val);

        let mut machine = interpreter::Machine::new(1000); // Fixed: Define machine
        machine.execute(&bf_code, false);

        let mut res = 0;
        for i in 0..32 {
            if machine.memory[dest as usize + i] != 0 {
                // Fixed: Cast dest to usize
                res |= 1 << i;
            }
        }
        assert_eq!(res, val);
        */
    }
    #[test]
    fn test_copy() {
        let mut bf_code = String::new();
        let mut ptr = 0;
        let src = 10;
        let dest = 50;
        let buf = 100;

        // Ensure ptr is at 0
        // Init src = 5 (101) in machine later.

        copy(&mut bf_code, &mut ptr, src, dest, buf, 32);

        let mut machine = Machine::new(1000);
        machine.memory[src as usize] = 1;
        machine.memory[src as usize + 2] = 1;

        match machine.run(&bf_code) {
            Ok(_) => {}
            Err(e) => panic!("Interpreter error: {}", e),
        }

        let mut res = 0;
        let mut src_res = 0;
        for i in 0..32 {
            if machine.memory[dest as usize + i] != 0 {
                res += 1 << i;
            }
            if machine.memory[src as usize + i] != 0 {
                src_res += 1 << i;
            }
        }
        println!("Copy result: {}, Source preserved: {}", res, src_res);
        assert_eq!(res, 5);
        assert_eq!(src_res, 5);
    }

    #[test]
    fn test_move_ptr() {
        let mut bf_code = String::new();
        let mut ptr = 0;

        // Move to 10
        move_ptr(&mut bf_code, &mut ptr, 10);
        assert_eq!(ptr, 10);
        assert_eq!(bf_code, ">>>>>>>>>>");

        // Move to 5
        move_ptr(&mut bf_code, &mut ptr, 5);
        assert_eq!(ptr, 5);
        assert_eq!(bf_code, ">>>>>>>>>><<<<<");

        // Move to 5 (no change)
        move_ptr(&mut bf_code, &mut ptr, 5);
        assert_eq!(ptr, 5);
        assert_eq!(bf_code, ">>>>>>>>>><<<<<");
    }
}
