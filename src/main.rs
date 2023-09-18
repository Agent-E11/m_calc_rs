use std::io;

use string_calc::calc::{tokenize, calculate, calculate_operator};

fn main() {

    let mut input_expr = String::new();

    io::stdin().read_line(&mut input_expr).expect("");

    let tokens = tokenize(&input_expr);

    let res = calculate_operator(tokens, "*");

    println!("Res: {res:?}");
}
