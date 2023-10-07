use std::io;

use string_calc::calc::{tokenize, calculate, convert_div_sub, convert_implicit_mul};

fn main() {

    let mut input_expr = String::new();

    io::stdin().read_line(&mut input_expr).expect("");

    let tokens = tokenize(&input_expr).unwrap();
    println!("Tokens: {tokens:?}");

    let c_res = calculate(&tokens);

    println!("Converted res: {c_res:?}");
}
