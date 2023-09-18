
// TODO: Add tests for functions (tokenize, simple_calc, calculate_operator, calculate, extract_token_values)
// TODO: Add complex calculation 2 (Order of operations)
// TODO: Add support for implicit multiplication (function that takes a `Vec<Token>` and converts all of its implicit multiplication to explicit)
// TODO: Add support for decimal numbers
// TODO: Create an `Error` enum: (Length error, parsing error, other?)
// TODO: Add support for some functions (sqrt, log, cbrt, floor/ceiling, abs, factorial, round, trig functions, maybe multiple variable functions?) (Add this inside of the Parentheses pass?)
// TODO: Add support for identifiers / variables (also `;`?)

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

        // Perform operation based on operator
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

    /// Takes a `Vec<Token>` and calculates all occurrences of a `&str` operator
    pub fn calculate_operator(tokens: Vec<Token>, operator: &str) -> Vec<Token> {
        let mut tokens = tokens;
        let mut i = 0;
        loop {
            if tokens.get(i) == None {
                break;
            }
            if let Token::Operator(o) = &tokens[i] {
                println!("Operator `{o}` at `{i}`");
                if o == operator {
                    println!("Operator matches");
                    println!("Calculating: {:?}", &tokens[i-1..i+2]);
                    let res = simple_calc(&Vec::from(&tokens[i-1..i+2]));
                    println!("Result: {res:?}");
                    println!("Deleting: {:?}", &tokens[i-1..i+2]);

                    tokens.drain(i-1..i+2);
                    tokens.insert(i-1, res);

                    i = 0;
                    continue;
                }
            }
            i += 1;
        }
        tokens
    }

    /// Takes a `&Vec<Token>` and computes the mathematical expression and returns the resulting number wrapped in a `Token::Num()`
    /// 
    /// # Panics
    /// Panics if 
    pub fn calculate(tokens: &Vec<Token>) -> Token {
        let mut tokens = tokens.clone();

        // TODO: Validate syntax. Check correct number of parentheses, make sure no 2 operators in a row, etc
        println!("Started `calculate`");

        // First pass: Calculate and substitute parentheses
        println!("Starting \"Parentheses\" pass");
        let mut i = 0;
        loop {
            let token;

            // Check if index is out of bounds, get token
            match tokens.get(i) {
                None => {
                    println!("Index out of bounds. Going to next pass");
                    break;
                },
                Some(t) => token = t.clone(),
            }

            print!("\n\n");
            println!("Tokens: {tokens:?}");
            println!("Token at index `{i}`: {:?}", token);

            // Find an open parentheses
            if token == Token::Operator("(".to_string()) {
                println!("Open parentheses at {i}");
                let mut j = i;

                // Find index of closing parentheses
                let mut open_parens = 1;
                loop {
                    j += 1;
                    if let Token::Operator(o) = &tokens[j] {
                        match o.as_str() {
                            "(" => open_parens += 1,
                            ")" => open_parens -= 1,
                            _ => (),
                        }
                    }
                    if open_parens == 0 {
                        break;
                    }
                }

                println!("Closing parentheses at {j}");

                let subsection = &Vec::from(&tokens[i+1..j]);
                println!("Subsection passed to `calculate`: {subsection:?}");

                // Calculate subsection
                let res = calculate(subsection);

                println!("Range to be deleted: {:?}", i..j+1);

                // Replace subsection with calculation
                tokens.drain(i..j+1);
                tokens.insert(i, res);

                // Go back to beginning of `tokens` and keep searching
                i = 0;
                continue;
            }

            i += 1;
        }

        // Second pass: operators(?) TODO:
        println!("Starting \"Operator\" pass");
        let mut i = 0;
        loop {
            let token;

            match tokens.get(i) {
                None => {
                    println!("Index out of bounds, going to next pass");
                    break;
                },
                Some(t) => token = t.clone(),
            }
            
            print!("\n\n");
            println!("Tokens: {tokens:?}");
            println!("Token at index `{i}`: {:?}", token);

            if let Token::Operator(_) = &token {
                println!("Token is an operator");
                let res = simple_calc(&Vec::from(&tokens[i-1..i+2]));

                println!("Range to be deleted: {:?}", i-1..i+2);

                tokens.drain(i-1..i+2);
                tokens.insert(i-1, res);

                i = 0;
                continue;
            }

            i += 1;
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
