pub mod alpha;
pub mod blocked;
pub mod closure;
pub mod cps;
pub mod debug;
pub mod emit;
pub mod id;
pub mod intermediate;
pub mod interpreter;
pub mod k_normal;
pub mod parser;
pub mod syntax;
pub mod ty;
pub mod typing;
pub mod r#virtual;

use intermediate::Layout;
use r#virtual::Prog;

pub fn compile(code: &str) -> Result<(String, Prog, Layout), String> {
    let (_, syntax) = parser::parse(code).map_err(|e| format!("{:?}", e))?;
    let typed_syntax = typing::f(&syntax).map_err(|e| format!("{:?}", e))?;
    let k_norm = k_normal::f(&typed_syntax);
    let alpha_norm = alpha::f(&k_norm);
    let closure_prog = closure::f(&alpha_norm);
    let cps_prog = cps::f(&closure_prog);
    let blocked_prog = blocked::f(&cps_prog);
    let intermediate_prog = intermediate::f(&blocked_prog, &closure_prog);
    eprintln!("DEBUG: Intermediate Prog:\n{}", intermediate_prog);
    use std::io::Write;
    std::io::stderr().flush().unwrap();
    let virtual_prog = r#virtual::f(&intermediate_prog);
    eprintln!(
        "DEBUG: Virtual Prog generated. Blocks: {}",
        virtual_prog.blocks.len()
    );
    eprintln!("DEBUG: Calling emit::f");
    let bf_code = emit::f(&virtual_prog);
    eprintln!("DEBUG: Emit finished. Code length: {}", bf_code.len());
    Ok((bf_code, virtual_prog, intermediate_prog.layout))
}
