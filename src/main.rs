use std::io;

use string_calc::calc::tokenize;

fn main() {

    let mut input_expr = String::new();

    io::stdin().read_line(&mut input_expr).expect("");

    for token in tokenize(&input_expr) {
        println!("{token:?}")
    }
}
