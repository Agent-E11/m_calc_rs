
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
                    tokens.push(Token::Num(expr[index..last_processed as usize + 1].to_owned()))
                },
                a if a.is_alphabetic() => { // TODO: Or if a is '\'? (to start function identifier)
                    // If it is a letter, walk through the next characters until it is not a letter, 
                    // then add that slice to `tokens` as a `Token::Identifier`
                    while indx_char.1.is_alphabetic() {
                        println!("(I, C): `{indx_char:?}`");
                        last_processed = indx_char.0 as i32;
                        indx_char = indcs_chars.next().unwrap();
                    }
                    tokens.push(Token::Identifier(expr[index..last_processed as usize + 1].to_owned()))
                    // TODO: Call `unimplemented!()` macro here
                },
                o if !o.is_alphabetic() && !o.is_numeric() => {
                    // If it neither a number, nor a letter,
                    // add the _single_ character to `tokens` as a `Token::Operator`
                    println!("(I, C): `{indx_char:?}`");
                    last_processed = indx_char.0 as i32;
                    tokens.push(Token::Operator(expr[index..last_processed as usize + 1].to_owned()))
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
    /// Panics if the `tokens` vector is not in the format `[Token::Num, Token::Operator, Token::Num]`, or if the operator is not valid
    pub fn simple_calc<'a>(tokens: &Vec<Token>) -> Token {

        let token_values = extract_token_values(tokens);
        println!("Tokens values passed to `simple_calc`: {token_values:?}");

        let left: i32 = token_values.0.parse().unwrap();
        let right: i32 = token_values.2.parse().unwrap();

        let res = match token_values.1.as_str() {
            "+" => Token::Num((left+right).to_string()),
            "-" => Token::Num((left-right).to_string()),
            "*" => Token::Num((left*right).to_string()),
            "/" => Token::Num((left/right).to_string()),
            "%" => Token::Num((left%right).to_string()),
            "^" => Token::Num(left.pow(right.try_into().unwrap()).to_string()),
            _ =>   panic!("Error: Invalid operator."),
        };

        println!("Result of `simple_calc`: {res:?}");
        res
    }

    pub fn calculate(tokens: &Vec<Token>) -> Token {
        let mut tokens = tokens.clone();

        // TODO: Validate syntax. Check correct number of parentheses, make sure no 2 operators in a row, etc

        // Calculate and substitute parentheses

        
        println!("Started `calculate`");
        for i in 0..tokens.len() {
            // TODO: .get token from index, check if out of bounds, 

            let token;

            // Get check if index is out of bounds, get token
            match tokens.get(i) {
                None => {
                    println!("Got to end of tokens");
                    break;
                },
                Some(t) => token = t.clone(),
            }

            print!("\n\n");
            println!("Tokens: {tokens:?}");
            println!("Token at index `{i}`: {:?}", token);

            // FIXME: Is this unnecessary?
            if tokens.len() == 1 {
                return tokens[0].clone();
            }

            // FIXME: This MIGHT work, untested
            if token == Token::Operator("(".to_string()) {
                let mut j = i;

                while tokens[j] != Token::Operator(")".to_string()) {
                    j += 1;
                }

                let subsection = &Vec::from(&tokens[i..j+1]);

                let res = calculate(subsection);

                tokens.drain(i..j+1);
                tokens.insert(i, res);
            }

            if let Token::Operator(_) = &token {
                println!("Token is an operator");
                let res = simple_calc(&Vec::from(&tokens[i-1..i+2]));

                println!("Range to be deleted: {:?}", i-1..i+2);

                tokens.drain(i-1..i+2);
                tokens.insert(i-1, res);
            }
        }

        tokens[0].clone()
    }

    /// Generates a `(String, String, String)` containing the same values as the given `&Vec<Token>`
    /// 
    /// # Panics
    /// Panics if the `tokens` vector is not in the format `[Token::Num, Token::Operator, Token::Num]`
    fn extract_token_values<'a>(tokens: &Vec<Token>) -> (String, String, String) {
        // FIXME: Add better error handling

        let token0;
        let token1;
        let token2;

        // Verify length
        if tokens.len() != 3 {
            panic!("Length must be 3");
        }

        // Verify tokens and extract values
        if let Token::Num(t) = &tokens[0] {
            token0 = t;
        } else {
            panic!("First token must be a `Token::Num`")
        }
        if let Token::Operator(t) = &tokens[1] {
            token1 = t;
        } else {
            panic!("Middle token must be a `Token::Operator`")
        }
        if let Token::Num(t) = &tokens[2] {
            token2 = t;
        } else {
            panic!("Last token must be a `Token::Num`")
        }

        (token0.to_owned(), token1.to_owned(), token2.to_owned())
    }

    /// Represents a token in a mathematical expression
    #[derive(Debug, PartialEq, Clone)]
    pub enum Token {
        Num(String),
        Operator(String),
        Identifier(String),
    }

    

}
