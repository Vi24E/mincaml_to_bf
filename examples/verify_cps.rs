use mincaml_to_bf::{closure, cps, k_normal, parser, typing};

fn main() {
    let code = "let x = 10 in let rec f y = x + y in f 20";
    let (_, syntax) = parser::parse(code).unwrap();
    let typed_syntax = typing::f(&syntax).unwrap();
    let k_norm = k_normal::f(&typed_syntax);
    let alpha_norm = mincaml_to_bf::alpha::f(&k_norm);
    let closure_prog = closure::f(&alpha_norm);
    let cps_prog = cps::f(&closure_prog);
    println!("{}", cps_prog);
}
