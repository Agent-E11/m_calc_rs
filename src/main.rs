use std::io;

use string_calc::calc::{tokenize, calculate};

fn main() {

    let mut input_expr = String::new();

    io::stdin().read_line(&mut input_expr).expect("");

    let tokens = tokenize(&input_expr);

    let res = calculate(&tokens);

    println!("Res: {res:?}");
}
