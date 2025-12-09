use mincaml_to_bf::compile;
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

    match compile(&code) {
        Ok((bf_code, _, _)) => {
            println!("{}", bf_code);
        }
        Err(e) => eprintln!("Error: {}", e),
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use mincaml_to_bf::parser;
    use mincaml_to_bf::syntax::Syntax;

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
