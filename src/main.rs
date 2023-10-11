use std::io;
use std::collections::HashMap;

use string_calc::calc::{tokenize, calculate, simple_syntax_check};

fn main() {

    let mut input_expr = String::new();

    io::stdin().read_line(&mut input_expr).expect("");

    let mut context = HashMap::new();
    let tokens = tokenize(&input_expr).unwrap();
    println!("Tokens: {tokens:?}");

    let res = simple_syntax_check(&tokens);

    if let Err(e) = &res {
        println!("Error:\n{e:?}");
    } else {
        println!("No errors");
    }

    println!("Calc: {:?}", calculate(&tokens, &mut context).unwrap())
}
