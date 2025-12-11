use mincaml_to_bf::compile;
use mincaml_to_bf::interpreter::Machine;

fn read_var(machine: &Machine, addr: usize) -> u32 {
    let mut val = 0;
    for i in 0..32 {
        if machine.memory[addr + i] != 0 {
            val |= 1 << i;
        }
    }
    val
}

fn run_test_case(code: &str, var_name: &str, expected_val: u32) {
    let (bf_code, virt_prog, layout) = compile(code).expect("Compilation failed");

    // Memory size needs to be large enough.
    // var_start + var_count * 32 + stack...
    // Let's use 100,000 to be safe.
    let mut machine = Machine::new(100000);

    // Initialize registers/stack area to 0 is done by new().

    machine.run(&bf_code).expect("Runtime error");

    // Helper to find alpha-converted variable name
    let found_name = layout.var_map.keys().find(|k| {
        *k == var_name || (k.starts_with(var_name) && k.chars().nth(var_name.len()) == Some('.'))
    });

    let var_offset = match found_name {
        Some(name) => *layout.var_map.get(name).unwrap(),
        None => panic!(
            "Variable {} not found in map. Available: {:?}",
            var_name,
            layout.var_map.keys()
        ),
    };
    let addr = virt_prog.var_start + var_offset * 32;

    println!("DEBUG: Block Map: {:?}", layout.block_map);
    println!("DEBUG: Running Flag Address: 0");
    println!("DEBUG: Entry Block Address: 1");

    let val = read_var(&machine, addr);
    // Verify registers are zeroed out
    // println!("DEBUG: reg_start = {}", virt_prog.reg_start);
    let mut garbage_found = false;
    for i in 0..256 {
        let reg_addr = virt_prog.reg_start + i;
        if machine.memory[reg_addr] != 0 {
            println!(
                "GARBAGE: Register at offset {} (addr {}) is not zero: {}",
                i, reg_addr, machine.memory[reg_addr]
            );
            garbage_found = true;
        }
    }

    // Check value match
    assert_eq!(val, expected_val, "Variable {} value mismatch", var_name);

    if garbage_found {
        panic!("Garbage found in registers!");
    }
}

#[test]
fn test_arithmetic() {
    let code = "let x = 10 + 20 in x";
    run_test_case(code, "x", 30);
}

#[test]
fn test_subz_garbage() {
    // Trigger SubZ with 10 - 5 (non-zero result)
    let program = r#"
    let x = 10 in
    let y = 5 in
    let result = if x = y then 100 else 200 in
    result
    "#;
    run_test_case(program, "result", 200);
}

#[test]
fn test_subz_zero_garbage() {
    // Trigger SubZ with 5 - 5 (zero result)
    let program = r#"
    let x = 5 in
    let y = 5 in
    let result = if x = y then 100 else 200 in
    result
    "#;
    run_test_case(program, "result", 100);
}

#[test]
fn test_sub() {
    let code = "let x = 30 - 10 in x";
    run_test_case(code, "x", 20);
}

#[test]
fn test_if_true() {
    let code = "let x = if 10 > 5 then 1 else 0 in x";
    // 10 > 5 is effectively 10 - 5 > 0?
    // MinCaml If implementation: IfEq, IfLE.
    // parser might treat > as not LE?
    // Let's use standard MinCaml constructs. `if x <= y`.
    // "not (10 <= 5)" -> "if 10 <= 5 then 0 else 1"

    let code = "let x = if 10 <= 5 then 0 else 1 in x";
    run_test_case(code, "x", 1);
}

#[test]
fn test_if_false() {
    let code = "let x = if 5 <= 10 then 1 else 0 in x";
    run_test_case(code, "x", 1);
}

#[test]
fn test_func_simple() {
    let code = "
    let rec f x = x + 1 in
    let y = f 10 in
    y
    ";
    run_test_case(code, "y", 11);
}

#[test]
fn test_fib() {
    // fib 0 = 0
    // fib 1 = 1
    // fib 2 = 1
    // fib 3 = 2
    // fib 4 = 3
    // fib 5 = 5
    // fib 6 = 8
    let code = "
    let rec fib n =
      if n <= 1 then n else
      fib (n - 1) + fib (n - 2)
    in
    let r = fib 6 in
    r
    ";
    run_test_case(code, "r", 8);
}

#[test]
fn test_nested_let() {
    let code = "
    let x = 1 in
    let y = 2 in
    let z = x + y in
    z
    ";
    run_test_case(code, "z", 3);
}

#[test]
fn test_negative() {
    let code = "let x = -5 in x";
    // -5 in 32bit:
    // run_test_case checks u32.
    // -5 as i32 is ...1111011 (2's complement)
    run_test_case(code, "x", (-5i32) as u32);
}

#[test]
fn test_emit_subz_direct() {
    use mincaml_to_bf::emit;
    use mincaml_to_bf::r#virtual::{Block, Operation, Prog};

    // Layout configuration
    let block_count = 3;
    let reg_start = 10;
    let var_start = 300;
    let stack_start = 1000;

    // Addresses
    let x_addr = (var_start) as u32;
    let y_addr = (var_start + 32) as u32;
    let z_addr = (reg_start) as u32; // Result of subtraction (Same as integration test)
    let res_addr = (var_start + 96) as u32; // Final result indicator

    let mut blocks = Vec::new();

    // Block 1 (Entry)
    blocks.push(Block {
        ops: vec![
            Operation::SetImm(x_addr, 10),
            Operation::SetImm(y_addr, 5),
            Operation::SubZ(z_addr, x_addr, y_addr), // 10 - 5 = 5 (NonZero)
            Operation::JumpIfZero(z_addr, 2, 3),     // if 0 goto 2 else 3
        ],
    });

    // Block 2 (Zero Case) - Should NOT be reached
    blocks.push(Block {
        ops: vec![
            Operation::SetImm(res_addr, 100),
            Operation::CallExternal("halt".to_string()),
        ],
    });

    // Block 3 (NonZero Case) - Should be reached
    blocks.push(Block {
        ops: vec![
            Operation::SetImm(res_addr, 200),
            Operation::CallExternal("halt".to_string()),
        ],
    });

    let prog = Prog {
        blocks,
        block_count,
        var_count: 10,
        reg_start,
        var_start,
        stack_start,
    };

    println!("DEBUG: Running test_emit_subz_direct");
    let bf_code = emit::f(&prog);

    // Run Machine
    let mut machine = Machine::new(10000);
    println!("DEBUG: Executing BF code");
    machine.run(&bf_code).expect("Runtime error");

    // Verify Result
    let res_val = read_var(&machine, res_addr as usize);
    println!("DEBUG: Result value: {}", res_val);
    assert_eq!(res_val, 200, "Should take NonZero branch");

    // Verify Garbage in Register Area
    let mut garbage_found = false;
    for i in 0..128 {
        // Check register area
        let addr = reg_start + i;
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
fn test_emit_subz_pure() {
    use mincaml_to_bf::emit;
    use mincaml_to_bf::r#virtual::{Block, Operation, Prog};

    // Layout configuration
    let block_count = 1;
    let reg_start = 10;
    let var_start = 300;
    let stack_start = 1000;

    // Addresses
    let x_addr = (var_start) as u32;
    let y_addr = (var_start + 32) as u32;
    let z_addr = (reg_start) as u32; // Result of subtraction (Targeting reg_start)

    let mut blocks = Vec::new();

    // Block 1 (Entry)
    blocks.push(Block {
        ops: vec![
            Operation::SetImm(x_addr, 10),
            Operation::SetImm(y_addr, 5),
            Operation::SubZ(z_addr, x_addr, y_addr), // 10 - 5 = 5
            Operation::CallExternal("halt".to_string()),
        ],
    });

    let prog = Prog {
        blocks,
        block_count,
        var_count: 10,
        reg_start,
        var_start,
        stack_start,
    };

    println!("DEBUG: Running test_emit_subz_pure");
    let bf_code = emit::f(&prog);

    // Run Machine
    let mut machine = Machine::new(10000);
    println!("DEBUG: Executing BF code");
    machine.run(&bf_code).expect("Runtime error");

    // Verify Result
    let res_val = read_var(&machine, z_addr as usize);
    println!("DEBUG: Result value: {}", res_val);
    assert_eq!(res_val, 5, "SubZ(10, 5) should be 5");

    // Verify Garbage in Register Area
    // Skip z_addr (which is reg_start) because it holds the valid result
    let mut garbage_found = false;
    for i in 1..128 {
        // Check register area from reg_start + 1
        let addr = reg_start + i;
        // Ignore known operands if they were in register area, but here x,y are in var_start.
        // We only care about temp registers used by SubZ.
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
