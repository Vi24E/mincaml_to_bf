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
