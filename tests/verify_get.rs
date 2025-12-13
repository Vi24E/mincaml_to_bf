use mincaml_to_bf::id;
use mincaml_to_bf::intermediate::{Atom, Block, Layout, Prog, Term};
use mincaml_to_bf::ty::Type;
use mincaml_to_bf::r#virtual::f;
use mincaml_to_bf::virtual_interpreter::Simulator;
use std::collections::{HashMap, HashSet};

#[test]
fn test_get_scaling() {
    // Test:
    // Let x = Tuple([0, 0]) (Alloc 2 words)
    // Let idx = 1
    // Let val = 99
    // Put(x, idx, val) -> x[1] = 99
    // Let res = Get(x, idx) -> res = x[1]
    // Assert res == 99

    let entry: id::L = "entry".to_string();
    let x: id::T = "x".to_string();
    let idx: id::T = "idx".to_string();
    let val: id::T = "val".to_string();
    let res: id::T = "res".to_string();
    let tuple_0: id::T = "t0".to_string();
    let tuple_1: id::T = "t1".to_string();
    let dummy: id::T = "dummy".to_string();

    let exit: id::L = "exit".to_string();

    // Exit block: Jump(exit) to loop safely
    let exit_term = Term::Jump(exit.clone());

    let term = Term::Let(
        (tuple_0.clone(), Type::Int),
        Atom::Int(0),
        Box::new(Term::Let(
            (tuple_1.clone(), Type::Int),
            Atom::Int(0),
            Box::new(Term::Let(
                (x.clone(), Type::Tuple(vec![Type::Int, Type::Int])),
                Atom::Tuple(vec![tuple_0.clone(), tuple_1.clone()]),
                Box::new(Term::Let(
                    (idx.clone(), Type::Int),
                    Atom::Int(1),
                    Box::new(Term::Let(
                        (val.clone(), Type::Int),
                        Atom::Int(99),
                        Box::new(Term::Let(
                            (dummy.clone(), Type::Unit),
                            Atom::Put(x.clone(), idx.clone(), val.clone()),
                            Box::new(Term::Let(
                                (res.clone(), Type::Int),
                                Atom::Get(x.clone(), idx.clone()),
                                Box::new(Term::Jump(exit.clone())),
                            )),
                        )),
                    )),
                )),
            )),
        )),
    );

    // Layout construction
    let mut var_map = HashMap::new();
    var_map.insert(tuple_0.clone(), 0);
    var_map.insert(tuple_1.clone(), 1);
    var_map.insert(x.clone(), 2);
    var_map.insert(idx.clone(), 3);
    var_map.insert(val.clone(), 4);
    var_map.insert(dummy.clone(), 5);
    var_map.insert(res.clone(), 6);

    let mut block_map = HashMap::new();
    block_map.insert(entry.clone(), 0);
    block_map.insert(exit.clone(), 1);

    let layout = Layout {
        block_map,
        var_map: var_map.clone(),
        block_count: 2,
        var_count: 10,
        frame_sizes: HashMap::new(),
    };

    let prog = Prog {
        blocks: vec![
            Block {
                id: entry.clone(),
                term,
            },
            Block {
                id: exit.clone(),
                term: exit_term,
            },
        ],
        entry,
        layout,
    };

    let virt_prog = f(&prog);
    let mut interp = Simulator::new(&virt_prog, 1000);
    let _ = interp.run(&virt_prog); // Ignore max steps error

    // Check 'res' variable in memory
    // Address of 'res' is 5.
    // Memory uses 32-word indexing? No, Simulator uses Address (byte/cell) indexing if memory accessed via `read_int`.
    // Wait. `Simulator::memory` is `Vec<i32>` (bits).
    // `Operation::Store(dest/addr)` writes 32 cells.
    // Address 5 implies `var_start + 5*32`.
    // `virtual.rs`: var_start is usually early?
    // `f` assigns addresses based on `var_map`.
    // `reg_start`, `var_start`, `stack_start`.
    // We need to know `var_start`.
    // `Simulator` doesn't expose `var_start`.
    // But `Simulator` memory is flat.
    // Address calculation in `virtual.rs`: `var_start + map[x] * 32`.
    // `reg_start = 0`. `var_start = 64 * 32`. (from `virtual.rs`)
    // I need to check `virtual.rs` constants.

    // Address calc:
    // reg_start = (block_count + 1) * 2 = (2 + 1) * 2 = 6
    // var_start = 6 + 128 = 134
    // res index = 6
    // res_addr = 134 + 6 * 32 = 134 + 192 = 326

    let bf_block_count = 2usize;
    let reg_size = 128usize;
    let reg_start = (bf_block_count + 1) * 2;
    let var_start = reg_start + reg_size;
    let res_offset = 6 * 32;
    let res_addr = var_start + res_offset;

    let res_val = interp.read_int(res_addr);

    assert_eq!(res_val, 99);
}
