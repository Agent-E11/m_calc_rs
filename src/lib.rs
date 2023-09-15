
// TODO: Add simple calculation (single operator)
// TODO: Add complex calculation 1 (Multiple operators)
// TODO: Add complex calculation 2 (Order of operations)
// TODO: Add complex calculation 3 (Parentheses)
// TODO: Add support for decimal numbers
// TODO: Create an `Error` enum: (Length error, parsing error, other?)

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

    pub fn simple_calc<'a>(tokens: &Vec<Token<'a>>) -> Token<'a> {

        let token_values = extract_token_values(tokens);
        let res;

        match token_values.1 {
            "+" => {res = Token::Num("Add");},
            "-" => {res = Token::Num("Sub");},
            "*" => {res = Token::Num("Mul");},
            "/" => {res = Token::Num("Div");},
            _ => {res = Token::Num("Error");},
        }

        println!("Ts: {token_values:?}");

        res
    }

    fn extract_token_values<'a>(tokens: &Vec<Token<'a>>) -> (&'a str, &'a str, &'a str) {
        // FIXME: Add better error handling

        let token0;
        let token1;
        let token2;

        if tokens.len() != 3 {
            panic!("Length must be 3");
        }

        if let Token::Num(t) = tokens[0] {
            token0 = t;
        } else {
            panic!("First token must be a `Token::Num`")
        }

        if let Token::Operator(t) = tokens[1] {
            token1 = t;
        } else {
            panic!("Middle token must be a `Token::Operator`")
        }

        if let Token::Num(t) = tokens[2] {
            token2 = t;
        } else {
            panic!("Last token must be a `Token::Num`")
        }

        (token0, token1, token2)
    }

    #[derive(Debug)]
    pub enum Token<'a> {
        Num(&'a str),
        Operator(&'a str),
        Identifier(&'a str),
    }

}
