
pub mod calc {

    pub fn tokenize<'a>(expr: &'a str) -> Vec<Token> {

        let mut tokens: Vec<Token> = Vec::new();
        let mut last_processed: i32 = -1;
        for (index, current_char) in expr.char_indices() {
            if index as i32 <= last_processed || current_char.is_whitespace() {
                continue;
            }

            let mut indcs_chars = expr.char_indices();
            let mut indx_char = indcs_chars.nth(index).unwrap(); // Should never panic
            println!("Current char tuple: {indx_char:?}");

            match indx_char.1 {
                n if n.is_numeric() => {
                    while indx_char.1.is_numeric() {
                        println!("(I, C): `{indx_char:?}`");
                        last_processed = indx_char.0 as i32;
                        indx_char = indcs_chars.next().unwrap();
                    }
                    tokens.push(Token::Num(&expr[index..last_processed as usize + 1]))
                },
                a if a.is_alphabetic() => { // TODO: Or if a is '\'? (to start function identifier)
                    while indx_char.1.is_alphabetic() {
                        println!("(I, C): `{indx_char:?}`");
                        last_processed = indx_char.0 as i32;
                        indx_char = indcs_chars.next().unwrap();
                    }
                    tokens.push(Token::Identifier(&expr[index..last_processed as usize + 1]))
                    // TODO: Call `unimplemented!()` macro here
                },
                o if !o.is_alphabetic() && !o.is_numeric() => {
                    println!("(I, C): `{indx_char:?}`");
                    last_processed = indx_char.0 as i32;
                    tokens.push(Token::Operator(&expr[index..last_processed as usize + 1]))
                },
                _ => (),
            }
        }

        println!("Token list: {tokens:?}");
        tokens
    }

    #[derive(Debug)]
    pub enum Token<'a> {
        Num(&'a str),
        Operator(&'a str),
        Identifier(&'a str),
    }

}
