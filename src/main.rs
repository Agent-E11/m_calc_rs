use std::io;

use string_calc::calc::{tokenize, calculate, convert_div_sub};

fn main() {

    let mut input_expr = String::new();

    io::stdin().read_line(&mut input_expr).expect("");

    let tokens = tokenize(&input_expr).unwrap();
    println!("Tokens: {tokens:?}");

    let converted = convert_div_sub(tokens.clone()).unwrap();
    println!("Converted: {converted:?}");

    let c_res = calculate(&converted);

    let u_res = calculate(&tokens);
    println!("");
    println!("Unconverted res: {u_res:?}");
    println!("Converted res: {c_res:?}");
}
