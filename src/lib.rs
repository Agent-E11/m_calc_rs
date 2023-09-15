
// TODO: Add simple calculation (single operator)
// TODO: Add complex calculation 1 (Multiple operators)
// TODO: Add complex calculation 2 (Order of operations)
// TODO: Add complex calculation 3 (Parentheses)
// TODO: Add support for decimal numbers
// TODO: Create an `Error` enum: (Length error, parsing error, other?)

pub mod calc {

    /// Constructs a `Vec<Token>` from a given `&str` representation of a mathematical expression
    pub fn tokenize<'a>(expr: &'a str) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        let mut last_processed: i32 = -1;

        // Loop through all characters in the expression
        for (index, current_char) in expr.char_indices() {
            // If the character has already been processed, or is whitespace, skip it
            if index as i32 <= last_processed || current_char.is_whitespace() {
                continue;
            }

            let mut indcs_chars = expr.char_indices();
            let mut indx_char = indcs_chars.nth(index).unwrap(); // Should never panic

            // Match the character in the `indx_char` tuple
            match indx_char.1 {
                n if n.is_numeric() => {
                    // If it is a number, walk through the next characters until it is not a number, 
                    // then add that slice to `tokens` as a `Token::Num`
                    while indx_char.1.is_numeric() {
                        println!("(I, C): `{indx_char:?}`");
                        last_processed = indx_char.0 as i32;
                        indx_char = indcs_chars.next().unwrap();
                    }
                    tokens.push(Token::Num(&expr[index..last_processed as usize + 1]))
                },
                a if a.is_alphabetic() => { // TODO: Or if a is '\'? (to start function identifier)
                    // If it is a letter, walk through the next characters until it is not a letter, 
                    // then add that slice to `tokens` as a `Token::Identifier`
                    while indx_char.1.is_alphabetic() {
                        println!("(I, C): `{indx_char:?}`");
                        last_processed = indx_char.0 as i32;
                        indx_char = indcs_chars.next().unwrap();
                    }
                    tokens.push(Token::Identifier(&expr[index..last_processed as usize + 1]))
                    // TODO: Call `unimplemented!()` macro here
                },
                o if !o.is_alphabetic() && !o.is_numeric() => {
                    // If it neither a number, nor a letter,
                    // add the _single_ character to `tokens` as a `Token::Operator`
                    println!("(I, C): `{indx_char:?}`");
                    last_processed = indx_char.0 as i32;
                    tokens.push(Token::Operator(&expr[index..last_processed as usize + 1]))
                },
                // If it is none of these, do nothing
                _ => (),
            }
        }

        println!("Token list: {tokens:?}");
        tokens
    }

    /// Generate a `Token` from a given `&Vec<Token>`
    /// 
    /// # Panics
    /// Panics if the `tokens` vector is not in the format `[Token::Num, Token::Operator, Token::Num]`
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

    /// Generates a `(&str, &str, &str)` containing the values of the given `&Vec<Token>`
    /// 
    /// # Panics
    /// Panics if the `tokens` vector is not in the format `[Token::Num, Token::Operator, Token::Num]`
    fn extract_token_values<'a>(tokens: &Vec<Token<'a>>) -> (&'a str, &'a str, &'a str) {
        // FIXME: Add better error handling

        let token0;
        let token1;
        let token2;

        // Verify length
        if tokens.len() != 3 {
            panic!("Length must be 3");
        }

        // Verify tokens and extract values
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

    /// Represents a token in a mathematical expression
    #[derive(Debug, PartialEq)]
    pub enum Token<'a> {
        Num(&'a str),
        Operator(&'a str),
        Identifier(&'a str),
    }

}
