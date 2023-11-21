use std::io;
use std::collections::HashMap;

use m_calc::{calculate, tokenize};

fn main() {
    let mut buf = String::new();
    let mut context = HashMap::new();

    loop {
        io::stdin().read_line(&mut buf).unwrap();

        let tokens = tokenize(&buf).unwrap();

        println!("\n--- Calculation ---\nResult:  {:?}\nContext: {:?}", calculate(&tokens, &mut context), context);

        buf.clear();
    }
}
