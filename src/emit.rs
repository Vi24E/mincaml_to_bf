use crate::r#virtual::{Operation, Prog};

pub fn f(prog: &Prog) -> String {
    let mut bf_code = String::new();
    let mut current_ptr = 0;

    let buf_size = 32; // User requested reduction
    let reg_start = prog.reg_start as u32;
    let var_start = prog.var_start as u32;
    let stack_start = prog.stack_start as u32;
    // buffer should always zero filled after operation
    let buffer_start = (prog.var_start - buf_size) as u32;
    let hp_ptr = 10000;
    let hp_reg_start = hp_ptr + 32;
    let _heap_start = hp_reg_start + 64; // Temporarily unused
    // heap: [hp_ptr][hp_regs...(128)][heap]
    // heap(with 33 bit) will grow upper

    // Metadata for Debugger (Ignored by BF as comments)
    // Format: DEBUG_METADATA{{REG_START:{} BUFFER_START:{} VAR_START:{} STACK_START:{}}}
    // Ensure no BF command chars in this string.
    bf_code.push_str(&format!(
        "DEBUG_METADATA{{REG_START:{} BUFFER_START:{} VAR_START:{} STACK_START:{}}}\n",
        reg_start, buffer_start, var_start, stack_start
    ));

    // Header or initialization if needed
    // The memory layout is:
    // [0..block_count]: Block activation flags
    // [reg_start..var_start]: Registers
    // [var_start..stack_start]: Variables
    // [stack_start..]: Stack

    // Initialize: Activate Entry Block (1) and Running Flag (0)
    let running_flag = 0;

    // Set Running Flag (0)
    move_ptr(&mut bf_code, &mut current_ptr, running_flag);
    bf_code.push('+');

    // Set Entry Block Flag (1 -> 2)
    move_ptr(&mut bf_code, &mut current_ptr, 2);
    bf_code.push('+');
    move_ptr(&mut bf_code, &mut current_ptr, running_flag);

    // External Loop Start
    bf_code.push('[');

    for (i, block) in prog.blocks.iter().enumerate() {
        bf_code.push_str(&(format!("\n# block {} Expected: {}\n", i, current_ptr).to_string()));
        move_ptr(&mut bf_code, &mut current_ptr, ((i + 1) * 2) as u32);
        bf_code.push_str("[-");

        for op in &block.ops {
            match op {
                Operation::Push(src) => {
                    bf_code.push_str(
                        &(format!("\n# Push src:{} Expected: {}\n", src, current_ptr).to_string()),
                    );
                    copy(
                        &mut bf_code,
                        &mut current_ptr,
                        *src,
                        stack_start,
                        buffer_start,
                        32,
                    );
                    move_ptr(&mut bf_code, &mut current_ptr, stack_start);
                    /*
                    3 bit push
                     +>>+
                     << // data
                     [->>> >>>>[>>>>]<<<+<[<<<<]<<<]
                     >[->> >>>>[>>>>]<<+<<[<<<<]<<]
                     >[-> >>>>[>>>>]<+<<<[<<<<]<]
                     > >>>>[>>>>]+[<<<<]
                     <<< // push

                     +>+>
                     << // data
                     [->>> >>>>[>>>>]<<<+<[<<<<]<<<]
                     >[->> >>>>[>>>>]<<+<<[<<<<]<<]
                     >[-> >>>>[>>>>]<+<<<[<<<<]<]
                     > >>>>[>>>>]+[<<<<]
                     <<< // push
                     */
                    for i in 0..32 {
                        bf_code.push_str("[-");
                        bf_code.push_str(&">".to_string().repeat(32 - i));
                        bf_code.push_str(&">".to_string().repeat(33));
                        bf_code.push('[');
                        bf_code.push_str(&">".to_string().repeat(33));
                        bf_code.push(']');
                        bf_code.push_str(&"<".to_string().repeat(32 - i));
                        bf_code.push('+');
                        bf_code.push_str(&"<".to_string().repeat(i + 1));
                        bf_code.push('[');
                        bf_code.push_str(&"<".to_string().repeat(33));
                        bf_code.push(']');
                        bf_code.push_str(&"<".to_string().repeat(32 - i));
                        bf_code.push_str("]>");
                    }
                    bf_code.push_str(&">".to_string().repeat(33));
                    bf_code.push('[');
                    bf_code.push_str(&">".to_string().repeat(33));
                    bf_code.push(']');
                    bf_code.push('+');
                    bf_code.push('[');
                    bf_code.push_str(&"<".to_string().repeat(33));
                    bf_code.push(']');
                    bf_code.push_str(&"<".to_string().repeat(32));
                }
                Operation::Pop(dest) => {
                    // bug
                    /*
                    3 bit pop
                    >>> >>>>[>>>>]<<<< <<< // move to last
                    [->>>[<<<<]<<<+>>> >>>>[>>>>]<<< <<<<]
                    >[->>[<<<<]<<+>> >>>>[>>>>]<< <<<<]
                    >[->[<<<<]<+> >>>>[>>>>]< <<<<]
                    >-<<<<[<<<<]<<< // used bit; pop complete
                     */

                    bf_code.push_str(
                        &(format!("\n# Pop dest:{} Expected: {}\n", dest, current_ptr).to_string()),
                    );
                    move_ptr(&mut bf_code, &mut current_ptr, stack_start);

                    bf_code.push_str(&">".to_string().repeat(32));
                    bf_code.push_str(&">".to_string().repeat(33));
                    bf_code.push('[');
                    bf_code.push_str(&">".to_string().repeat(33));
                    bf_code.push(']');
                    bf_code.push_str(&"<".to_string().repeat(33));
                    bf_code.push_str(&"<".to_string().repeat(32));
                    for i in 0..32 {
                        bf_code.push_str("[-");
                        bf_code.push_str(&">".to_string().repeat(32 - i));
                        bf_code.push('[');
                        bf_code.push_str(&"<".to_string().repeat(33));
                        bf_code.push(']');
                        bf_code.push_str(&"<".to_string().repeat(32 - i));
                        bf_code.push('+');
                        bf_code.push_str(&">".to_string().repeat(32 - i));
                        bf_code.push_str(&">".to_string().repeat(33));
                        bf_code.push('[');
                        bf_code.push_str(&">".to_string().repeat(33));
                        bf_code.push(']');
                        bf_code.push_str(&"<".to_string().repeat(32 - i));
                        bf_code.push_str(&"<".to_string().repeat(33));
                        bf_code.push_str("]>");
                    }
                    bf_code.push('-');
                    bf_code.push_str(&"<".to_string().repeat(33));
                    bf_code.push('[');
                    bf_code.push_str(&"<".to_string().repeat(33));
                    bf_code.push(']');
                    bf_code.push_str(&"<".to_string().repeat(32));
                    move_val(&mut bf_code, &mut current_ptr, stack_start, *dest, 32);
                }
                Operation::SetImm(dest, val) => {
                    bf_code.push_str(
                        &(format!(
                            "\n# SetImm dest:{} val:{} Expected: {}\n",
                            dest, val, current_ptr
                        )
                        .to_string()),
                    );
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
                    bf_code.push_str(
                        &(format!(
                            "\n# Neg dest:{} src:{} Expected: {}\n",
                            dest, src, current_ptr
                        )
                        .to_string()),
                    );
                    move_ptr(&mut bf_code, &mut current_ptr, *src);
                    neg(
                        &mut bf_code,
                        &mut current_ptr,
                        reg_start,
                        buffer_start,
                        *dest,
                    );
                    clear_range(&mut bf_code, &mut current_ptr, reg_start, 128);
                }
                Operation::Add(dest, src1, src2) => {
                    bf_code.push_str(
                        &(format!(
                            "\n# Add dest:{} src1:{} src2:{} Expected: {}\n",
                            dest, src1, src2, current_ptr
                        )
                        .to_string()),
                    );
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
                    move_val(&mut bf_code, &mut current_ptr, reg_start + 32, *dest, 32);
                    clear_range(&mut bf_code, &mut current_ptr, reg_start, 128);
                }
                Operation::Sub(dest, src1, src2) => {
                    bf_code.push_str(
                        &(format!(
                            "\n# Sub dest:{} src1:{} src2:{} Expected: {}\n",
                            dest, src1, src2, current_ptr
                        )
                        .to_string()),
                    );
                    copy(
                        &mut bf_code,
                        &mut current_ptr,
                        *src1,
                        reg_start,
                        buffer_start,
                        32,
                    );
                    move_ptr(&mut bf_code, &mut current_ptr, *src2);
                    neg(
                        &mut bf_code,
                        &mut current_ptr,
                        reg_start + 64,
                        buffer_start,
                        reg_start + 32,
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
                    move_val(&mut bf_code, &mut current_ptr, reg_start + 32, *dest, 32);
                    clear_range(&mut bf_code, &mut current_ptr, reg_start, 128);
                }
                Operation::SubZ(dest, src1, src2) => {
                    bf_code.push_str(
                        &(format!(
                            "\n# SubZ dest:{} src1:{} src2:{} Expected: {}\n",
                            dest, src1, src2, current_ptr
                        )
                        .to_string()),
                    );
                    copy(
                        &mut bf_code,
                        &mut current_ptr,
                        *src1,
                        reg_start,
                        buffer_start,
                        32,
                    );
                    move_ptr(&mut bf_code, &mut current_ptr, *src2);
                    neg(
                        &mut bf_code,
                        &mut current_ptr,
                        reg_start + 64,
                        buffer_start,
                        reg_start + 32,
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
                    move_ptr(&mut bf_code, &mut current_ptr, reg_start + 32);
                    maxzero(&mut bf_code);
                    move_val(&mut bf_code, &mut current_ptr, reg_start + 32, *dest, 32); // bug?
                    clear_range(&mut bf_code, &mut current_ptr, reg_start, 128);
                }
                Operation::JumpIfZero(cond, l1, l2) => {
                    bf_code.push_str(
                        &(format!(
                            "\n# JumpIfZero cond:{} l1:{} l2:{} Expected: {}\n",
                            cond, l1, l2, current_ptr
                        )
                        .to_string()),
                    );
                    // unimplemented!("JumpIfZero emit not fully implemented");
                    copy(
                        &mut bf_code,
                        &mut current_ptr,
                        *cond,
                        reg_start,
                        buffer_start,
                        32,
                    );
                    // clear_range(&mut bf_code, &mut current_ptr, reg_start, 128); // DISABLED
                    move_ptr(&mut bf_code, &mut current_ptr, reg_start);

                    for _ in 0..31 {
                        bf_code.push_str("[->+<]>");
                    }
                    bf_code.push_str("[[-]>+<]>");

                    current_ptr += 32;
                    bf_code.push_str("[>+>+<<-]>>>+<[->-<]"); // 33 = pos, 35 = neg
                    current_ptr += 2;
                    move_ptr(&mut bf_code, &mut current_ptr, reg_start + 33);
                    bf_code.push_str("[-");
                    move_ptr(&mut bf_code, &mut current_ptr, (*l2 + 1) * 2);
                    bf_code.push('+');
                    move_ptr(&mut bf_code, &mut current_ptr, reg_start + 33);
                    bf_code.push_str("]");
                    move_ptr(&mut bf_code, &mut current_ptr, reg_start + 35);
                    bf_code.push_str("[-");
                    move_ptr(&mut bf_code, &mut current_ptr, (*l1 + 1) * 2);
                    bf_code.push('+');
                    move_ptr(&mut bf_code, &mut current_ptr, reg_start + 35);
                    bf_code.push_str("]");
                    clear_range(&mut bf_code, &mut current_ptr, reg_start, 128); // DISABLED
                }
                Operation::JumpIfLE(_, _, _) => {
                    // Stub for JumpIfLE
                }
                Operation::Jump(target) => {
                    bf_code.push_str(
                        &(format!("\n# Jump target:{} Expected: {}\n", target, current_ptr)
                            .to_string()),
                    );
                    move_ptr(&mut bf_code, &mut current_ptr, (*target + 1) * 2);
                    bf_code.push('+'); // activate block
                }
                Operation::JumpVar(src) => {
                    bf_code.push_str(
                        &(format!("\n# JumpVar src:{} Expected: {}\n", src, current_ptr)
                            .to_string()),
                    );

                    copy(
                        &mut bf_code,
                        &mut current_ptr,
                        *src,
                        reg_start + 35,
                        buffer_start,
                        32,
                    ); // reg[1] = *src
                    copy(
                        &mut bf_code,
                        &mut current_ptr,
                        reg_start + 35,
                        reg_start,
                        buffer_start,
                        32,
                    ); // reg[0] = reg[1]
                    move_ptr(&mut bf_code, &mut current_ptr, reg_start);
                    for _ in 0..31 {
                        bf_code.push_str("[->+<]>");
                    }
                    bf_code.push_str("[[-]>+<]>"); // reg[0]' = (reg[0] != 0)
                    current_ptr += 32;
                    bf_code.push_str("[-"); // while reg[0]':
                    move_ptr(&mut bf_code, &mut current_ptr, 2);
                    bf_code.push_str(">[>>]+[<<]>");
                    move_ptr(&mut bf_code, &mut current_ptr, reg_start + 35);
                    add(
                        &mut bf_code,
                        &mut current_ptr,
                        reg_start + 70,
                        buffer_start,
                        reg_start,
                        0xFFFFFFFF,
                    ); // reg[0] = reg[1] - 1
                    clear_range(&mut bf_code, &mut current_ptr, reg_start + 35, 32); // clear reg[1]
                    copy(
                        &mut bf_code,
                        &mut current_ptr,
                        reg_start,
                        reg_start + 35,
                        buffer_start,
                        32,
                    ); // reg[1] = reg[0]
                    move_ptr(&mut bf_code, &mut current_ptr, reg_start);
                    for _ in 0..31 {
                        bf_code.push_str("[->+<]>");
                    }
                    bf_code.push_str("[[-]>+<]>"); // reg[0]' = (reg[0] != 0)
                    current_ptr += 32;
                    bf_code.push_str("]");
                    move_ptr(&mut bf_code, &mut current_ptr, 2);
                    bf_code.push_str(">[>>]<<<+>[-<<]"); // reset sub
                    current_ptr = 1;
                }
                Operation::MoveData(dest, src, size) => {
                    bf_code.push_str(
                        &(format!(
                            "\n# MoveData dest:{} src:{} size:{} Expected: {}\n",
                            dest, src, size, current_ptr
                        )
                        .to_string()),
                    );
                    //if dest >= &stack_start {
                    clear_range(&mut bf_code, &mut current_ptr, *dest, 32);
                    //}
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
                        bf_code.push_str(
                            &(format!("\n# Halt name:{} Expected: {}\n", name, current_ptr)
                                .to_string()),
                        );
                        move_ptr(&mut bf_code, &mut current_ptr, running_flag);
                        bf_code.push_str("[-]");
                        // clear_range(&mut bf_code, &mut current_ptr, reg_start, 128); // DISABLED
                    } else if name == "min_caml_print_int" || name == "print_int" {
                        bf_code.push_str(
                            &(format!("\n# CallExternal Stub name:{}\n", name).to_string()),
                        );
                    } else {
                        panic!("CallExternal is not implemented");
                    }
                }
                Operation::Halt => {
                    bf_code
                        .push_str(&(format!("\n# Halt Expected: {}\n", current_ptr).to_string()));
                    move_ptr(&mut bf_code, &mut current_ptr, running_flag);
                    bf_code.push_str("[-]");
                }
                Operation::InputByte(addr) => {
                    bf_code.push_str(
                        &(format!("\n# InputByte addr:{} Expected: {}\n", addr, current_ptr)
                            .to_string()),
                    );
                    move_ptr(&mut bf_code, &mut current_ptr, *addr);
                    bf_code.push(',');
                }
                Operation::OutputByte(addr) => {
                    bf_code.push_str(
                        &(format!("\n# OutputByte addr:{} Expected: {}\n", addr, current_ptr)
                            .to_string()),
                    );
                    move_ptr(&mut bf_code, &mut current_ptr, *addr);
                    bf_code.push('.');
                }
            }
        }

        move_ptr(&mut bf_code, &mut current_ptr, ((i + 1) * 2) as u32);
        bf_code.push(']');
    }
    // Return to running flag for outer loop check
    move_ptr(&mut bf_code, &mut current_ptr, running_flag);
    bf_code.push(']');

    bf_code
}

#[allow(dead_code)]
fn move_ptr(bf_code: &mut String, current_ptr: &mut u32, target_ptr: u32) {
    // println!("move_ptr: {} -> {}", *current_ptr, target_ptr);
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

// clear range [start, start + size)
fn clear_range(bf_code: &mut String, current_ptr: &mut u32, start: u32, size: u32) {
    bf_code.push_str(
        &(format!(
            "\n## clear_range start:{} size:{} Expected: {}\n",
            start, size, *current_ptr
        )
        .to_string()),
    );
    move_ptr(bf_code, current_ptr, start);
    for _ in 0..size {
        bf_code.push_str("[-]>");
        *current_ptr += 1;
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
    bf_code.push_str(
        &(format!(
            "\n## copy src:{} dest:{} buf:{} size:{} Expected: {}\n",
            source, dest, buffer, size, *current_ptr
        )
        .to_string()),
    );
    clear_range(bf_code, current_ptr, dest, size);
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
    bf_code.push_str(
        &(format!(
            "\n## add reg:{} buf:{} dest:{} val:{} Expected: {}\n",
            register, buffer, dest, val, *current_ptr
        )
        .to_string()),
    );
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
    clear_range(bf_code, current_ptr, dest, 32);
    move_val(bf_code, current_ptr, register + 1, dest, 32);
    // clear_range(bf_code, current_ptr, register, 66); // DISABLED
}

// move value from source to dest (destroy source)
fn move_val(bf_code: &mut String, current_ptr: &mut u32, source: u32, dest: u32, size: u32) {
    bf_code.push_str(
        &(format!(
            "\n## move_val src:{} dest:{} size:{} Expected: {}\n",
            source, dest, size, *current_ptr
        )
        .to_string()),
    ); // Optional, but let's be consistent
    clear_range(bf_code, current_ptr, dest, size);
    for i in 0..size {
        // move_ptr(bf_code, current_ptr, dest + i);
        // bf_code.push_str("[-]"); // Clear dest
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
    bf_code.push_str(
        &(format!(
            "\n## neg reg:{} buf:{} dest:{} Expected: {}\n",
            register, buffer, dest, *current_ptr
        )
        .to_string()),
    );
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
    bf_code.push_str("\n## maxzero\n");
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
