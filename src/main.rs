use std::io;

use string_calc::calc::{tokenize, calculate, convert_div_sub, convert_implicit_mul, parse_functions};

fn main() {

    let mut input_expr = String::new();

    io::stdin().read_line(&mut input_expr).expect("");

    let tokens = tokenize(&input_expr).unwrap();
    println!("Tokens: {tokens:?}");

    // let parsed = parse_functions(tokens);
    // println!("{parsed:?}");

    let c_res = calculate(&tokens).unwrap();

    println!("Converted res: {c_res:?}");
}
