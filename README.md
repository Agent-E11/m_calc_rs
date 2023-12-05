# m_calc

> !!! this crate is under development and future updates will include breaking changes !!!

[`m_calc`](https://crates.io/crates/m_calc) is a Rust crate for calculating mathematical expressions in strings.

For instance, a string like `"2*3^(4-2)+14%3"` should be calculated to `20`

It also supports mathematical functions like: `\sqrt(3.14159)` is approximately `0`

And variables: `"a=10;5+a"` -> `15`

## Usage

Add to cargo project

```sh
cargo add m_calc
```

Parse a string into a vector of tokens, and calculate the vector of tokens.

```rust
use m_calc::{tokenize, calculate};
use std::collections::HashMap;

let tokens = tokenize("2*3^(4-2)+14%3").unwrap();

let mut context = HashMap::new(); // The `context` is where variable assignments are stored

let res = calculate(&tokens, &mut context).unwrap();

println!("{}", res.display()); // `display` is a method of `Token`s that displays it as a simple expression (like `10`, or `a`)

assert_eq!("20", res.display());
```
