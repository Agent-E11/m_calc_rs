use std::io;

use string_calc::calc::{tokenize, simple_calc};

fn main() {

    let mut input_expr = String::new();

    io::stdin().read_line(&mut input_expr).expect("");

    let tokens = tokenize(&input_expr);

    let res = simple_calc(&tokens);

    println!("Res: {res:?}");
}
