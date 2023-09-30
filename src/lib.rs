
// TODO: Replace `String`s in Token with `f32`s and enums (where appropriate)
// TODO: Add tests for functions (tokenize, simple_calc, calculate_operator, calculate, extract_token_values)
// TODO: Add complex calculation 2 (Order of operations)
// TODO: Add support for implicit multiplication
//      (function that takes a `Vec<Token>` and converts all of its implicit multiplication to explicit)
//      (this happens after the "parentheses" pass, and just inserts a `Token::Op("*")` between any two `Token::Num()`)
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
                n if n.is_numeric() || n == '.' => {
                    // If it is a number, walk through the next characters until it is not a number, 
                    // then add that slice to `tokens` as a `Token::Num`
                    while indx_char.1.is_numeric() || indx_char.1 == '.' {
                        println!("(I, C): `{indx_char:?}`");
                        last_processed = indx_char.0 as i32;
                        indx_char = indcs_chars.next().unwrap();
                    }
                    tokens.push(Token::Num(expr[index..last_processed as usize + 1].to_owned()))
                },
                a if a.is_alphabetic() => { // TODO: Or if a is '\'? (to start function identifier)
                    // If it is a letter, walk through the next characters until it is not a letter, 
                    // then add that slice to `tokens` as a `Token::Id`
                    while indx_char.1.is_alphabetic() {
                        println!("(I, C): `{indx_char:?}`");
                        last_processed = indx_char.0 as i32;
                        indx_char = indcs_chars.next().unwrap();
                    }
                    tokens.push(Token::Id(expr[index..last_processed as usize + 1].to_owned()))
                    // TODO: Call `unimplemented!()` macro here
                },
                o if !o.is_alphabetic() && !o.is_numeric() => {
                    // If it neither a number, nor a letter,
                    // add the _single_ character to `tokens` as a `Token::Op`
                    println!("(I, C): `{indx_char:?}`");
                    last_processed = indx_char.0 as i32;
                    tokens.push(Token::Op(expr[index..last_processed as usize + 1].to_owned()))
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
    /// Panics if the `tokens` vector is not in the format `[Token::Num, Token::Op, Token::Num]`, or if the operator is not valid
    pub fn simple_calc<'a>(tokens: &Vec<Token>) -> Token {

        let token_values = extract_token_values(tokens);
        println!("Tokens values passed to `simple_calc`: {token_values:?}");

        let left: f32 = token_values.0.parse().unwrap();
        let right: f32 = token_values.2.parse().unwrap();

        // Perform operation based on operator
        let res = match token_values.1.as_str() {
            "+" => Token::Num((left+right).to_string()),
            "-" => Token::Num((left-right).to_string()),
            "*" => Token::Num((left*right).to_string()),
            "/" => Token::Num((left/right).to_string()),
            "%" => Token::Num((left%right).to_string()),
            "^" => Token::Num(left.powf(right.try_into().unwrap()).to_string()),
            _ =>   panic!("Error: Invalid operator."),
        };

        println!("Result of `simple_calc`: {res:?}");
        res
    }

    /// Takes a `Vec<Token>` and converts all occurrences of division and subtraction to their equivalents in multiplication and addition
    pub fn convert_div_sub(tokens: Vec<Token>) -> Vec<Token> {
        let mut tokens = tokens;
        
        let mut i = 0;
        loop {
            let token = match tokens.get(i) {
                None => break,
                Some(t) => t,
            };

            if let Token::Op(o) = token {
                match o.as_str() {
                    "/" => {
                        println!("Div found at `{i}`");
                        if let Token::Num(n) = tokens[i+1].clone() {
                            tokens[i] = Token::Op("*".to_string());
                            tokens[i+1] = Token::Num((1.0/n.parse::<f32>().unwrap()).to_string());
                        }
                    },
                    "-" => {
                        println!("Sub found at `{i}`");
                        if let Token::Num(n) = tokens[i+1].clone() {
                            tokens[i] = Token::Op("+".to_string());
                            tokens[i+1] = Token::Num((-n.parse::<f32>().unwrap()).to_string());
                        }
                    },
                    _ => (),
                }
            }

            i += 1;
        }

        tokens
    }

    /// Takes a `Vec<Token>` and calculates all occurrences of a `&str` operator
    pub fn calculate_operator(tokens: Vec<Token>, operator: &str) -> Vec<Token> {
        let mut tokens = tokens;
        let mut i = 0;
        loop {
            if tokens.get(i) == None {
                break;
            }
            if let Token::Op(o) = &tokens[i] {
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
            if token == Token::Op("(".to_string()) {
                println!("Open parentheses at {i}");
                let mut j = i;

                // Find index of closing parentheses
                let mut open_parens = 1;
                loop {
                    j += 1;
                    if let Token::Op(o) = &tokens[j] {
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

        // Second pass: operators
        println!("Starting \"Operator\" pass");

        println!("Tokens before pass: {tokens:?}");
        tokens = calculate_operator(tokens, "^");
        println!("After `^`: {tokens:?}");
        tokens = calculate_operator(tokens, "*");
        println!("After `*`: {tokens:?}");
        tokens = calculate_operator(tokens, "/");
        println!("After `/`: {tokens:?}");
        tokens = calculate_operator(tokens, "+");
        println!("After `+`: {tokens:?}");
        tokens = calculate_operator(tokens, "-");
        println!("After `-`: {tokens:?}");

        tokens[0].clone()
    } 

    /// Generates a `(String, String, String)` containing the same values as the given `&Vec<Token>`
    /// 
    /// # Panics
    /// Panics if the `tokens` vector is not in the format `[Token::Num, Token::Op, Token::Num]`
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
        if let Token::Op(t) = &tokens[1] {
            token1 = t;
        } else {
            panic!("Middle token must be a `Token::Op`")
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
        Op(String),
        Id(String),
    }

    impl std::fmt::Display for Token {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Token::Num(n) => write!(f, "Num(\"{n}\")"),
                Token::Op(n) => write!(f, "Op(\"{n}\")"),
                Token::Id(n) => write!(f, "Id(\"{n}\")"),
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use super::Token::{Num, Op, Id};

        use std::panic::catch_unwind;

        #[test]
        fn test_tokenize() {
            let expr1 = "6^8*(4-1)*(3%2+(4*2))";
            let expr2 = "abc123!@#";

            assert_eq!(
                tokenize(expr1),
                vec![
                    Num("6".to_string()),
                    Op( "^".to_string()),
                    Num("8".to_string()),
                    Op( "*".to_string()),
                    Op( "(".to_string()),
                    Num("4".to_string()),
                    Op( "-".to_string()),
                    Num("1".to_string()),
                    Op( ")".to_string()),
                    Op( "*".to_string()),
                    Op( "(".to_string()),
                    Num("3".to_string()),
                    Op( "%".to_string()),
                    Num("2".to_string()),
                    Op( "+".to_string()),
                    Op( "(".to_string()),
                    Num("4".to_string()),
                    Op( "*".to_string()),
                    Num("2".to_string()),
                    Op( ")".to_string()),
                    Op( ")".to_string()),
                ]
            );

            assert_eq!(
                tokenize(expr2),
                vec![
                    Id("abc".to_string()),
                    Num("123".to_string()),
                    Op("!".to_string()),
                    Op("@".to_string()),
                    Op("#".to_string()),
                ]
            )
        }

        #[test]
        fn test_simple_calc() {
            let tokens1 = &vec![Num("2".to_string()), Op("*".to_string()), Num("4".to_string())];
            let tokens2 = &vec![Num("3".to_string()), Op("%".to_string()), Num("2".to_string())];
            let tokens3 = &vec![Num("4".to_string()), Op("^".to_string()), Num("4".to_string())];

            assert_eq!(simple_calc(tokens1), Num("8".to_string()));
            assert_eq!(simple_calc(tokens2), Num("1".to_string()));
            assert_eq!(simple_calc(tokens3), Num("256".to_string()));
        }

        #[test]
        fn test_panic_simple_calc() {
            let tokens1 = &vec![Num("1".to_string())]; // Wrong length
            let tokens2 = &vec![Num("2".to_string()), Num("2".to_string()), Num("2".to_string())]; // Wrong operator
            let tokens3 = &vec![Num("a".to_string()), Op("*".to_string()), Num("1".to_string())]; // Non-number

            let res1 = catch_unwind(|| {simple_calc(tokens1)});
            let res2 = catch_unwind(|| {simple_calc(tokens2)});
            let res3 = catch_unwind(|| {simple_calc(tokens3)});

            assert!(res1.is_err());
            assert!(res2.is_err());
            assert!(res3.is_err());
        }

        #[test]
        fn test_calculate_operator() {
            let tokens1 = vec![
                Num("2".to_string()),
                Op( "^".to_string()),
                Num("2".to_string()),
                Op( "+".to_string()),
                Num("5".to_string())
            ];

            assert_eq!(
                calculate_operator(tokens1.clone(), "^"),
                vec![Num("4".to_string()), Op("+".to_string()), Num("5".to_string())]
            );

            assert_eq!(
                calculate_operator(tokens1.clone(), "+"),
                vec![Num("2".to_string()), Op("^".to_string()), Num("7".to_string())]
            );
        }

        #[test]
        fn test_calculate() {
            todo!();
        }
    }
}
