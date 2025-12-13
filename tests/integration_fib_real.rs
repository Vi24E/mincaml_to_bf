use mincaml_to_bf::compile_to_virtual;
use mincaml_to_bf::virtual_interpreter::Simulator;

#[test]
fn test_fib_integration_real() {
    let code = "
let rec fib n =
  if n <= 1 then n else
  fib (n - 1) + fib (n - 2)
in
print_int (fib 10)
    ";

    // Compile to virtual machine program
    let (prog, _layout) = compile_to_virtual(code).expect("Compilation failed");

    eprintln!("DEBUG: Virtual Prog:\n{}", prog);

    // Run using Simulator
    // Max steps 10 million to be safe
    let mut sim = Simulator::new(&prog, 10_000_000);

    // Run
    sim.run(&prog).expect("Simulation failed");

    // Check output
    let output_str = String::from_utf8(sim.output).expect("Invalid UTF-8 output");
    println!("Simulator Output: {}", output_str);

    assert_eq!(output_str.trim(), "55");
}
