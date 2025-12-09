pub mod alpha;
pub mod blocked;
pub mod closure;
pub mod cps; // Added cps module
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

use std::env;
use std::fs;
use std::io::{self, Read};

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let code = if args.len() > 1 {
        fs::read_to_string(&args[1])?
    } else {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        buffer
    };

    match parser::parse(&code) {
        Ok((_, syntax)) => {
            match typing::f(&syntax) {
                Ok(typed_syntax) => {
                    let k_norm = k_normal::f(&typed_syntax);
                    // println!("K-Normal: {:?}", k_norm);
                    let alpha_norm = alpha::f(&k_norm);
                    let closure_prog = closure::f(&alpha_norm);
                    // println!("{}", closure_prog);

                    let cps_prog = cps::f(&closure_prog);
                    // println!("CPS: {}", cps_prog);

                    let blocked_prog = blocked::f(&cps_prog);
                    // println!("Blocked Prog:\n{}", blocked_prog);

                    let intermediate_prog = intermediate::f(&blocked_prog, &closure_prog);
                    println!("Intermediate Prog:\n{}", intermediate_prog);

                    let virtual_prog = r#virtual::f(&intermediate_prog);
                    println!("{}", virtual_prog);

                    let bf_code = emit::f(&virtual_prog);
                    println!("{}", bf_code);
                }
                Err(e) => eprintln!("Type Error: {:?}", e),
            }
        }
        Err(e) => eprintln!("Parse Error: {:?}", e),
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::Syntax;

    #[test]
    fn test_simple_parse() {
        let code = "let x = 1 in x + 2";
        let (_, syntax) = parser::parse(code).unwrap();
        match syntax {
            Syntax::Let((x, _), val, body) => {
                assert_eq!(x, "x");
                assert_eq!(*val, Syntax::Int(1));
                match *body {
                    Syntax::Add(v1, v2) => {
                        assert_eq!(*v1, Syntax::Var("x".to_string()));
                        assert_eq!(*v2, Syntax::Int(2));
                    }
                    _ => panic!("Expected Add"),
                }
            }
            _ => panic!("Expected Let"),
        }
    }
}
