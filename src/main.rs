use std::io;
use std::collections::HashMap;

use m_calc::{calculate, tokenize};

fn main() {

    let mut buf = String::new();

    io::stdin().read_line(&mut buf).unwrap();

    let mut context = HashMap::new();
    let tokens = tokenize(&buf).unwrap();
    println!("Tokens: {tokens:?}");

    println!("\n--- Calculation ---\nResult:  {:?}\nContext: {:?}", calculate(&tokens, &mut context), context);
}
