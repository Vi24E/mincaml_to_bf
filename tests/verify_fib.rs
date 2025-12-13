use mincaml_to_bf::r#virtual as virtual_mod;
use mincaml_to_bf::virtual_interpreter::Simulator;
use virtual_mod::{Block, Operation, Prog};

#[test]
fn test_fib_stack_cps() {
    // Memory Layout:
    // Regs: 0..4096 (stride 32)
    // SP: 1024 or so.

    // Reg Map:
    // SP = 0 (Cell 0)
    // n = 1 (Cell 32)
    // k = 2 (Cell 64)
    // r1 = 3 (Cell 96)
    // r2 = 4 (Cell 128)
    // res = 5 (Cell 160)
    // temp_cond = 6 (Cell 192)
    // temp_addr = 7 (Cell 224)

    // Block Map:
    // 0: Main (Setup)
    // 1: Fib Entry (Pop k, n)
    // 2: Base Case (n < 2 -> App(k, n))
    // 3: Rec 1 (App(fib, n-1, k_cont1))
    // 4: Rec 1 Cont (Pop r1, k, n) -> Rec 2 (App(fib, n-2, k_cont2))
    // 5: Rec 2 Cont (Pop r2, r1, k, n) -> ADD -> App(k, r1+r2)
    // 6: Main K (Pop res -> Halt)

    let r1_reg = 96;
    let r2_reg = 128;
    let res_reg = 160;
    let temp_cond = 192;

    let k_main_lbl = 7;
    let fib_lbl = 2; // Block 1 -> Label 2
    let cont1_lbl = 5; // Block 4 -> Label 5
    let cont2_lbl = 6; // Block 5 -> Label 6
    let base_case_lbl = 3; // Block 2 -> Label 3
    let rec1_lbl = 4; // Block 3 -> Label 4

    let steps = 10_000_000;
    let sp_addr = 0;
    let n_reg = 32;
    let k_reg = 64;

    let ops = vec![
        // Block 0: Main
        // k_main = LoadLabel(6)
        Operation::SetImm(k_reg as u32, k_main_lbl),
        // Push k_main
        Operation::Push(k_reg as u32), // Pushing K first?
        // Args: [n, k].
        // Order: Pushed in generic order [n, k] or [k, n]?
        // Blocked.rs: AppDir pushes generic args. `fib(n, k)`.
        // Pushes n. Pushes k.
        // Stack Top: k.

        // Push n = 10
        Operation::SetImm(n_reg as u32, 10),
        Operation::Push(n_reg as u32),
        // Push k (re-push? no, already pushed above? No, I need to push n then k)
        // Reset k
        Operation::SetImm(k_reg as u32, k_main_lbl),
        Operation::Push(k_reg as u32),
        // Jump Fib
        Operation::Jump(fib_lbl as u32),
    ];

    let fib_ops = vec![
        // Block 1: Fib Entry
        // Pop k, n (Reverse order of Push)
        // Stack: [n, k] (Top=k)
        Operation::Pop(k_reg as u32),
        Operation::Pop(n_reg as u32),
        // Check n <= 1
        Operation::SetImm(temp_cond as u32, 1),
        Operation::Sub(temp_cond as u32, n_reg as u32, temp_cond as u32), // n - 1
        Operation::JumpIfLE(temp_cond as u32, base_case_lbl, rec1_lbl), // If n-1 <= 0 (n <= 1) -> Block 2 (3), else Block 3 (4)
    ];

    let fib_base = vec![
        // Block 2: Base Case (n < 2)
        // Return n.
        // App(k, n). k is continuation.
        // Args: [n].
        // Push n.
        Operation::Push(n_reg as u32),
        // Jump k (JumpVar)
        Operation::JumpVar(k_reg as u32),
    ];

    let fib_rec1 = vec![
        // Block 3: Rec 1
        // MakeCls(cont1, [n, k]).
        // Push FVs: n, k.
        Operation::Push(n_reg as u32),
        Operation::Push(k_reg as u32),
        // Bind cont1 = LoadLabel(4)
        Operation::SetImm(192, cont1_lbl), // Using temp reg for cont1
        // App(fib, n-1, cont1)
        // Push n-1
        Operation::SetImm(160, 1),              // 1
        Operation::Sub(160, n_reg as u32, 160), // n-1 in temp
        Operation::Push(160),
        // Push cont1
        Operation::Push(192),
        // Jump Fib
        Operation::Jump(fib_lbl as u32),
    ];

    let cont1_ops = vec![
        // Block 4: Rec 1 Cont
        // Expects: Arg(r1). Stack Top: r1.
        Operation::Pop(r1_reg as u32),
        // Pop FVs: k, n (Reverse order of Push)
        // Pushed [n, k]. Top is k.
        Operation::Pop(k_reg as u32),
        Operation::Pop(n_reg as u32),
        // MakeCls(cont2, [n, k, r1])
        // Push FVs
        Operation::Push(n_reg as u32),
        Operation::Push(k_reg as u32),
        Operation::Push(r1_reg as u32),
        // Bind cont2 = LoadLabel(5)
        Operation::SetImm(192, cont2_lbl),
        // App(fib, n-2, cont2)
        // Push n-2
        Operation::SetImm(160, 2),
        Operation::Sub(160, n_reg as u32, 160),
        Operation::Push(160),
        // Push cont2
        Operation::Push(192),
        // Jump Fib
        Operation::Jump(fib_lbl as u32),
    ];

    let cont2_ops = vec![
        // Block 5: Rec 2 Cont
        // Expects: Arg(r2).
        Operation::Pop(r2_reg as u32),
        // Pop FVs: r1, k, n (Reverse order of Push)
        // Pushed [n, k, r1]. Top is r1.
        Operation::Pop(r1_reg as u32),
        Operation::Pop(k_reg as u32),
        Operation::Pop(n_reg as u32),
        // Add r1 + r2
        Operation::Add(res_reg as u32, r1_reg as u32, r2_reg as u32),
        // App(k, res)
        // Push res
        Operation::Push(res_reg as u32),
        // Jump k
        Operation::JumpVar(k_reg as u32),
    ];

    let main_k_ops = vec![
        // Block 6: Main K
        // Pop res
        Operation::Pop(res_reg as u32),
        // Halt (CallExternal logic handled by simulator? Or Loop)
        // Since verify_fib uses manual loop...
        // We just stop.
        // But the Simulator::run logic needs a HALT.
        // Simulator loops until error or max steps.
        // We can write a magic value to a known address to signal "Done".
        // Or checking `sim.pc` if it goes out of bounds?
        // Or looping forever.
        Operation::CallExternal("halt".to_string()),
    ];

    // Assemble
    let mut blocks = vec![
        ops, fib_ops, fib_base, fib_rec1, cont1_ops, cont2_ops, main_k_ops,
    ];

    // Need to flatten blocks into Simulator instructions?
    // Simulator uses `Prog`.
    // Tests use `Prog::new`.

    // Mapping Block Indices is correct (0..6).

    let v_blocks: Vec<Block> = blocks.into_iter().map(|ops| Block { ops }).collect();

    let prog = Prog::new(
        v_blocks,
        7,                // Block Count
        0,                // Var Count
        0,                // Reg Start
        0,                // Var Start
        0,                // Stack Start
        sp_addr as usize, // SP addr
    );

    // Initial SP
    // Simulator::new initializes memory.
    // SP value is set via `write_int` inside `new`.
    // But `new` uses `prog.sp_addr` to find the register address, and writes `prog.stack_start` as value.
    // Here `stack_start` is 0.
    // If we want SP to be 2000*32, we should set `stack_start` to 2000*32 in Prog?
    // Or write it manually after `new`.

    let mut sim = Simulator::new(&prog, steps);

    // Override SP value for safety (avoid 0)
    sim.write_int(sp_addr, 2000 * 32);

    let final_res = sim.run(&prog);

    match final_res {
        Ok(_) => println!("Finished successfully"),
        Err(e) => println!("Error: {:?}", e),
    }

    // Check Result: `res_reg` should implement 55.
    let result = sim.read_int(res_reg as usize);
    assert_eq!(result, 55);
}
