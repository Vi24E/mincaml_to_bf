use mincaml_to_bf::emit;
use mincaml_to_bf::interpreter::Machine;
use mincaml_to_bf::r#virtual::{Block, Operation, Prog};

// Helper function to run a single operation test
fn run_test(op: Operation, expected_res: u32, res_addr: u32, test_name: &str) {
    let block_count = 1;
    let reg_start = 10;
    let var_start = 300;
    let stack_start = 1000;

    let mut blocks = Vec::new();
    blocks.push(Block {
        ops: vec![op, Operation::CallExternal("halt".to_string())],
    });

    let prog = Prog {
        blocks,
        block_count,
        var_count: 50,
        reg_start,
        var_start,
        stack_start,
        sp_addr: stack_start,
    };

    println!("DEBUG: Running {}", test_name);
    let bf_code = emit::f(&prog);

    let mut machine = Machine::new(100000);
    // machine.run might panic, capturing expected panic is hard in simple test harness without catch_unwind
    // but we expect success here.
    machine.run(&bf_code).expect("Runtime error");

    let mut val = 0;
    for i in 0..32 {
        if machine.memory[res_addr as usize + i] != 0 {
            val |= 1 << i;
        }
    }

    println!(
        "DEBUG: {} Result: {}, Expected: {}",
        test_name, val, expected_res
    );
    assert_eq!(val, expected_res, "{} failed", test_name);
}

// Memory Layout for tests
// reg_start = 10 (registers: 10..266)
// var_start = 300 (variables: 300..)
// stack_start = 1000

// Helper to check for garbage in register area
fn check_garbage(prog: &Prog, machine: &Machine, ignore_addrs: &[u32]) {
    let mut garbage_found = false;
    for i in 0..256 {
        let addr = prog.reg_start + i;
        if ignore_addrs.contains(&(addr as u32)) {
            continue;
        }
        if machine.memory[addr] != 0 {
            println!(
                "GARBAGE at offset {} (addr {}): {}",
                i, addr, machine.memory[addr]
            );
            garbage_found = true;
        }
    }
    assert!(!garbage_found, "Garbage found in register area");
}

#[test]
fn test_setimm_large() {
    let dest = 300; // var_start
    let val = 0xFFFFFFFF; // u32::MAX
    run_test(
        Operation::SetImm(dest, val as i32),
        val,
        dest,
        "SetImm u32::MAX",
    );
}

#[test]
fn test_setimm_large_2() {
    let dest = 300;
    let val = 0xAAAAAAAA; // Alternating bits
    run_test(
        Operation::SetImm(dest, val as i32),
        val,
        dest,
        "SetImm 0xAAAAAAAA",
    );
}

#[test]
fn test_add_large_no_overflow() {
    // 0x7FFFFFFF + 1 = 0x80000000
    // Test manual setup because run_test takes single op.
    // We need to set up operands first.
    let dest = 300;
    let src1 = 332;
    let src2 = 364;

    let block = Block {
        ops: vec![
            Operation::SetImm(src1, 0x7FFFFFFF),
            Operation::SetImm(src2, 1),
            Operation::Add(dest, src1, src2),
            Operation::CallExternal("halt".to_string()),
        ],
    };

    // ... manual run similar to run_test but customized ...
    let prog = Prog {
        blocks: vec![block],
        block_count: 1,
        var_count: 50,
        reg_start: 10,
        var_start: 300,
        stack_start: 1000,
        sp_addr: 1000,
    };

    let bf_code = emit::f(&prog);
    let mut machine = Machine::new(100000);
    machine.run(&bf_code).expect("Runtime error");

    let mut val = 0;
    for i in 0..32 {
        if machine.memory[dest as usize + i] != 0 {
            val |= 1 << i;
        }
    }
    assert_eq!(val, 0x80000000_u32, "Add 0x7FFFFFFF + 1 failed");
    check_garbage(&prog, &machine, &[]);
}

#[test]
fn test_add_overflow() {
    // 0xFFFFFFFF + 1 = 0 (Overflow)
    let dest = 300;
    let src1 = 332;
    let src2 = 364;

    let block = Block {
        ops: vec![
            Operation::SetImm(src1, -1), // 0xFFFFFFFF
            Operation::SetImm(src2, 1),
            Operation::Add(dest, src1, src2),
            Operation::CallExternal("halt".to_string()),
        ],
    };

    let prog = Prog {
        blocks: vec![block],
        block_count: 1,
        var_count: 50,
        reg_start: 10,
        var_start: 300,
        stack_start: 1000,
        sp_addr: 1000,
    };

    let bf_code = emit::f(&prog);
    let mut machine = Machine::new(100000);
    machine.run(&bf_code).expect("Runtime error");

    let mut val = 0;
    for i in 0..32 {
        if machine.memory[dest as usize + i] != 0 {
            val |= 1 << i;
        }
    }
    assert_eq!(val, 0, "Add 0xFFFFFFFF + 1 (Overflow) failed");
    check_garbage(&prog, &machine, &[]);
}

#[test]
fn test_sub_large_no_underflow() {
    // 0x80000000 - 1 = 0x7FFFFFFF
    let dest = 300;
    let src1 = 332;
    let src2 = 364;

    let block = Block {
        ops: vec![
            Operation::SetImm(src1, 0x80000000u32 as i32),
            Operation::SetImm(src2, 1),
            Operation::Sub(dest, src1, src2),
            Operation::CallExternal("halt".to_string()),
        ],
    };

    let prog = Prog {
        blocks: vec![block],
        block_count: 1,
        var_count: 50,
        reg_start: 10,
        var_start: 300,
        stack_start: 1000,
        sp_addr: 1000,
    };

    let bf_code = emit::f(&prog);
    let mut machine = Machine::new(100000);
    machine.run(&bf_code).expect("Runtime error");

    let mut val = 0;
    for i in 0..32 {
        if machine.memory[dest as usize + i] != 0 {
            val |= 1 << i;
        }
    }
    assert_eq!(val, 0x7FFFFFFF, "Sub 0x80000000 - 1 failed");
    check_garbage(&prog, &machine, &[]);
}

#[test]
fn test_sub_underflow() {
    // 0 - 1 = 0xFFFFFFFF (Underflow)
    let dest = 300;
    let src1 = 332;
    let src2 = 364;

    let block = Block {
        ops: vec![
            Operation::SetImm(src1, 0),
            Operation::SetImm(src2, 1),
            Operation::Sub(dest, src1, src2),
            Operation::CallExternal("halt".to_string()),
        ],
    };

    let prog = Prog {
        blocks: vec![block],
        block_count: 1,
        var_count: 50,
        reg_start: 10,
        var_start: 300,
        stack_start: 1000,
        sp_addr: 1000,
    };

    let bf_code = emit::f(&prog);
    let mut machine = Machine::new(100000);
    machine.run(&bf_code).expect("Runtime error");

    let mut val = 0;
    for i in 0..32 {
        if machine.memory[dest as usize + i] != 0 {
            val |= 1 << i;
        }
    }
    assert_eq!(val, 0xFFFFFFFF_u32, "Sub 0 - 1 (Underflow) failed");
    check_garbage(&prog, &machine, &[]);
}

#[test]
fn test_subz_large_result() {
    // 0xFFFFFFFF - 1 = 0xFFFFFFFE (Normal Sub)
    let dest = 300;
    let src1 = 332;
    let src2 = 364;

    let block = Block {
        ops: vec![
            Operation::SetImm(src1, -1),
            Operation::SetImm(src2, 1),
            Operation::SubZ(dest, src1, src2),
            Operation::CallExternal("halt".to_string()),
        ],
    };

    let prog = Prog {
        blocks: vec![block],
        block_count: 1,
        var_count: 50,
        reg_start: 10,
        var_start: 300,
        stack_start: 1000,
        sp_addr: 1000,
    };

    let bf_code = emit::f(&prog);
    let mut machine = Machine::new(100000);
    machine.run(&bf_code).expect("Runtime error");

    let mut val = 0;
    for i in 0..32 {
        if machine.memory[dest as usize + i] != 0 {
            val |= 1 << i;
        }
    }
    assert_eq!(val, 0xFFFFFFFE_u32, "SubZ 0xFFFFFFFF - 1 failed");
    check_garbage(&prog, &machine, &[]);
}

#[test]
fn test_subz_zero_result_large() {
    // 10 - 20 = 0 (Clamped)
    let dest = 300;
    let src1 = 332;
    let src2 = 364;

    let block = Block {
        ops: vec![
            Operation::SetImm(src1, 10),
            Operation::SetImm(src2, 20),
            Operation::SubZ(dest, src1, src2),
            Operation::CallExternal("halt".to_string()),
        ],
    };

    let prog = Prog {
        blocks: vec![block],
        block_count: 1,
        var_count: 50,
        reg_start: 10,
        var_start: 300,
        stack_start: 1000,
        sp_addr: 1000,
    };

    let bf_code = emit::f(&prog);
    let mut machine = Machine::new(100000);
    machine.run(&bf_code).expect("Runtime error");

    let mut val = 0;
    for i in 0..32 {
        if machine.memory[dest as usize + i] != 0 {
            val |= 1 << i;
        }
    }
    assert_eq!(val, 0, "SubZ 10 - 20 (Clamped) failed");
    check_garbage(&prog, &machine, &[]);
}

#[test]
fn test_subz_conflict_dest_reg() {
    // User pointer issue case: dest is in register area
    // 10 - 5 = 5
    let reg_start = 10;
    let dest = reg_start as u32; // <--- DANGER ZONE
    let src1 = 332;
    let src2 = 364;

    let block = Block {
        ops: vec![
            Operation::SetImm(src1, 10),
            Operation::SetImm(src2, 5),
            Operation::SubZ(dest, src1, src2),
            Operation::CallExternal("halt".to_string()),
        ],
    };

    let prog = Prog {
        blocks: vec![block],
        block_count: 1,
        var_count: 50,
        reg_start,
        var_start: 300,
        stack_start: 1000,
        sp_addr: 1000,
    };

    let bf_code = emit::f(&prog);
    let mut machine = Machine::new(100000);
    machine.run(&bf_code).expect("Runtime error");

    let mut val = 0;
    for i in 0..32 {
        if machine.memory[dest as usize + i] != 0 {
            val |= 1 << i;
        }
    }
    assert_eq!(val, 5, "SubZ dest=reg_start failed");
    // Don't check garbage for reg_start itself as it is dest, check others
    check_garbage(&prog, &machine, &[dest]);
}
