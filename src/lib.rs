// TODO: Change `CalcError` from an enum to a struct. Rename to `CalcErr`

// TODO: Validate syntax. Check correct number of parentheses, make sure no 2 operators in a row, etc
// TODO: Add tests for functions (tokenize, simple_calc, calculate_operator, calculate, extract_token_values)
// TODO: Add complex calculation 2 (Order of operations)

// TODO: Add support for some functions
//      (sqrt, log, cbrt, floor/ceiling, abs, factorial, round, trig functions, maybe multiple variable functions?)
//      (before parentheses pass(?), convert any section in the format `Op(FnStart), Id, Op::(LPar), "Tokens", Op::(RPar)` to `Fn("Tokens")`)
// TODO: Add support for identifiers / variables (also `;`?)

pub mod calc {

    /// Constructs a `Vec<Token>` from a given `&str` representation of a mathematical expression
    pub fn tokenize(expr: &str) -> Result<Vec<Token>, CalcError> {

        let mut tokens: Vec<Token> = Vec::new();
        let mut last_processed: i32 = -1;

        // Loop through all characters in the expression
        for (index, current_char) in expr.char_indices() {
            // If the character has already been processed, or is whitespace, skip it
            if index as i32 <= last_processed || current_char.is_whitespace() {
                continue;
            }

            let mut indcs_chars = expr.char_indices();
            let mut indx_char = indcs_chars.nth(index).unwrap();

            // Match the character in the `indx_char` tuple
            match indx_char.1 {
                n if n.is_numeric() || n == '.' => {
                    // If it is a number, walk through the next characters until it is not a number, 
                    // then add that slice to `tokens` as a `Token::Num`
                    while indx_char.1.is_numeric() || indx_char.1 == '.' {
                        println!("(I, C): `{indx_char:?}`");
                        last_processed = indx_char.0 as i32;
                        indx_char = match indcs_chars.next() {
                            Some(i_c) => i_c,
                            None => return Err(CalcError::Parse(String::from("Index out of bounds"))),
                        };
                    }
                    tokens.push(Token::Num(expr[index..last_processed as usize + 1].parse::<f32>()?));
                },
                a if a.is_alphabetic() => { // TODO: Or if a is '\'? (to start function identifier)
                    // If it is a letter, walk through the next characters until it is not a letter, 
                    // then add that slice to `tokens` as a `Token::Id`
                    while indx_char.1.is_alphabetic() {
                        println!("(I, C): `{indx_char:?}`");
                        last_processed = indx_char.0 as i32;
                        indx_char = match indcs_chars.next() {
                            Some(i_c) => i_c,
                            None => return Err(CalcError::Parse(String::from("Index out of bounds"))),
                        };
                    }
                    tokens.push(Token::Id(expr[index..last_processed as usize + 1].to_owned()))
                    // TODO: Call `unimplemented!()` macro here?
                },
                o if !o.is_alphabetic() && !o.is_numeric() => {
                    // If it neither a number, nor a letter,
                    // add the _single_ character to `tokens` as a `Token::Op`
                    println!("(I, C): `{indx_char:?}`");
                    last_processed = indx_char.0 as i32;
                    tokens.push(Token::Op(Oper::from(&expr[index..last_processed as usize + 1])))
                },
                // If it is none of these, return an error
                _ => return Err(CalcError::Parse(String::from("Invalid character in expression"))),
            }
        }

        println!("Token list: {tokens:?}");
        Ok(tokens)
    }

    /// Generate a `Token` from a given `&Vec<Token>`
    /// 
    /// # Panics
    /// Panics if the `tokens` vector is not in the format `[Token::Num, Token::Op, Token::Num]`, or if the operator is not valid
    pub fn simple_calc(tokens: &Vec<Token>) -> Result<Token, CalcError> {
        if tokens.is_empty() { return Err(CalcError::Length(String::from("Length cannot be `0`"))); }

        let token_values = extract_token_values(tokens)?;
        println!("Tokens values passed to `simple_calc`: {token_values:?}");

        let left = token_values.0;
        let right = token_values.2;

        // Perform operation based on operator
        let res = match token_values.1 {
            Oper::Exp => Ok(Token::Num(left.powf(right))),
            Oper::Mul => Ok(Token::Num(left*right)),
            Oper::Div => Ok(Token::Num(left/right)),
            Oper::Mod => Ok(Token::Num(left%right)),
            Oper::Add => Ok(Token::Num(left+right)),
            Oper::Sub => Ok(Token::Num(left-right)),
            Oper::LPar => Err(CalcError::Parse(String::from("`(` is an invalid calculation operator"))),
            Oper::RPar => Err(CalcError::Parse(String::from("`)` is an invalid calculation operator"))),
            Oper::FnStart => Err(CalcError::Parse(String::from("`\\` is an invalid calculation operator"))),
        };

        println!("Result of `simple_calc`: {res:?}");
        res
    }

    /// Takes a `Vec<Token>` and converts all occurrences of division and subtraction to their equivalents in multiplication and addition
    pub fn convert_div_sub(tokens: Vec<Token>) -> Result<Vec<Token>, CalcError> {
        if tokens.is_empty() { return Err(CalcError::Length(String::from("Length cannot be `0`"))); }
        let mut tokens = tokens;
        
        let mut i = 0;
        loop {
            let token = match tokens.get(i) {
                None => break,
                Some(t) => t,
            };

            if let Token::Op(o) = token {
                match o {
                    Oper::Div => {
                        println!("Div found at `{i}`");
                        match tokens[i+1].clone() {
                            Token::Num(n) => {
                                tokens[i] = Token::Op(Oper::Mul);
                                tokens[i+1] = Token::Num(1.0/n);
                            },
                            _ => return Err(CalcError::Parse(format!("token at index `{} (after `/`) is not a `Token::Num`", i + 1)))
                        }
                    },
                    Oper::Sub => {
                        println!("Sub found at `{i}`");
                        match tokens[i+1].clone() {
                            Token::Num(n) => {
                                tokens[i] = Token::Op(Oper::Add);
                                tokens[i+1] = Token::Num(-n);
                            },
                            _ => return Err(CalcError::Parse(format!("token at index `{} (after `-`) is not a `Token::Num`", i + 1)))
                        }
                    },
                    _ => (),
                }
            }

            i += 1;
        }

        Ok(tokens)
    }

    /// Takes a `Vec<Token>` and calculates all occurrences of a `&str` operator
    pub fn calculate_operator(tokens: Vec<Token>, operators: &[Oper]) -> Result<Vec<Token>, CalcError> {
        if tokens.is_empty() { return Err(CalcError::Length(String::from("Length cannot be `0`"))); }

        let mut tokens = tokens;
        let mut i = 0;
        loop {
            if tokens.get(i).is_none() {
                break;
            }
            if let Token::Op(o) = &tokens[i] {
                println!("Operator `{o}` at `{i}`");
                if operators.contains(o) {
                    println!("Operator matches");
                    println!("Calculating: {:?}", &tokens[i-1..i+2]);
                    let res = simple_calc(&Vec::from(&tokens[i-1..i+2]))?;
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
        Ok(tokens)
    }

    /// Takes a `&Vec<Token>` and computes the mathematical expression and returns the resulting number wrapped in a `Token::Num()`
    /// 
    /// # Panics
    /// Panics if 
    pub fn calculate(tokens: &Vec<Token>) -> Result<Token, CalcError> { // TODO: Address Clippy issue (no `drain` method)
        if tokens.is_empty() { return Err(CalcError::Length(String::from("Length cannot be `0`"))); }
        let mut tokens = tokens.clone();

        // TODO: Validate syntax. Check correct number of parentheses, make sure no 2 operators in a row, etc
        println!("Started `calculate`");

        // First pass: Calculate and substitute parentheses
        println!("Starting \"Parentheses\" pass");
        let mut i = 0;
        loop {

            // Check if index is out of bounds, get token
            let token = match tokens.get(i) {
                None => {
                    println!("Index out of bounds. Going to next pass");
                    break;
                },
                Some(t) => t.clone(),
            };

            print!("\n\n");
            println!("Tokens: {tokens:?}");
            println!("Token at index `{i}`: {:?}", token);

            // Find an open parentheses
            if token == Token::Op(Oper::LPar) {
                println!("Open parentheses at {i}");
                let mut j = i;

                // Find index of closing parentheses
                let mut open_parens = 1;
                loop {
                    j += 1;
                    if let Token::Op(o) = &tokens[j] {
                        match o {
                            Oper::LPar => open_parens += 1,
                            Oper::RPar => open_parens -= 1,
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
                let res = calculate(subsection)?;

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

        tokens = convert_implicit_mul(tokens)?;

        // Second pass: operators
        println!("Starting \"Operator\" pass");

        println!("Tokens before pass: {tokens:?}");
        tokens = calculate_operator(tokens, &[Oper::Exp])?;
        println!("After `^`: {tokens:?}");
        tokens = calculate_operator(tokens, &[Oper::Mul, Oper::Div, Oper::Mod])?;
        println!("After `*, /, %`: {tokens:?}");
        tokens = calculate_operator(tokens, &[Oper::Add, Oper::Sub])?;
        println!("After `+, -`: {tokens:?}");

        // If we end up with multiple tokens at the end, return an error
        if tokens.len() > 1 || !matches!(tokens[0], Token::Num(_)) {
            return Err(CalcError::Parse(String::from("calculation error, final result was not a single `Token::Num`")));
        }

        Ok(tokens[0].clone())
    } 

    /// Generates a `(String, String, String)` containing the same values as the given `&Vec<Token>`
    /// 
    /// # Panics
    /// Panics if the `tokens` vector is not in the format `[Token::Num, Token::Op, Token::Num]`
    fn extract_token_values(tokens: &Vec<Token>) -> Result<(f32, Oper, f32), CalcError> {
        if tokens.is_empty() { return Err(CalcError::Length(String::from("Length cannot be `0`"))); }

        let token0;
        let token1;
        let token2;

        // Verify length
        if tokens.len() != 3 {
            return Err(CalcError::Length(String::from("Length must be 3")));
        }

        // Verify tokens and extract values
        if let Token::Num(t) = &tokens[0] {
            token0 = t;
        } else {
            return Err(CalcError::Parse(String::from("First token must be a `Token::Num`")));
        }
        if let Token::Op(t) = &tokens[1] {
            token1 = t;
        } else {
            return Err(CalcError::Parse(String::from("Middle token must be a `Token::Op`")));
        }
        if let Token::Num(t) = &tokens[2] {
            token2 = t;
        } else {
            return Err(CalcError::Parse(String::from("Last token must be a `Token::Num`")));
        }

        Ok((*token0, token1.clone(), *token2))
    }

    pub fn convert_implicit_mul(mut tokens: Vec<Token>) -> Result<Vec<Token>, CalcError> {
        // Return error if `tokens` contains parentheses
        if tokens.contains(&Token::Op(Oper::LPar)) || tokens.contains(&Token::Op(Oper::RPar)) {
            return Err(CalcError::Parse(String::from("cannot call `convert_implicit_mul` on a `Vec<Token>` that contains parentheses")));
        }

        // Insert `Token::Op(Oper::Mul)` between any `Token::Num` or `Token::Id`
        let mut i = 0;
        loop {

            match tokens[i] {
                Token::Num(_) | Token::Id(_) => match tokens.get(i+1) {
                    Some(t) => match t {
                        Token::Num(_) | Token::Id(_) => {
                            tokens.insert(i+1, Token::Op(Oper::Mul));
                            i = 0;
                            continue;
                        },
                        Token::Op(_) | Token::Fn(_)=> (),
                    },
                    None => break,
                },
                Token::Op(_) | Token::Fn(_) => (),
            }

            i += 1;
        }

        Ok(tokens)
    }

    /// Represents a mathematical operator
    #[derive(Debug, PartialEq, Clone)]
    pub enum Oper {
        LPar, // Left parentheses
        RPar, // RIght parentheses
        Exp, // Exponent
        Mul, // Multiply
        Div, // Divide
        Mod, // Modulus
        Add, // Add
        Sub, // Subtract
        FnStart, // Signifies that the next token (an Id), will be the name of a function
    }
    impl From<&str> for Oper {
        fn from(value: &str) -> Self {
            match value {
                "(" => Oper::LPar,
                ")" => Oper::RPar,
                "^" => Oper::Exp,
                "*" => Oper::Mul,
                "/" => Oper::Div,
                "%" => Oper::Mod,
                "+" => Oper::Add,
                "-" => Oper::Sub,
                "\\" => Oper::FnStart,
                _ => panic!(),
            }
        }
    }
    impl std::fmt::Display for Oper {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{self:?}")
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum Func {
        Sqrt(Vec<Token>),
        Cbrt(Vec<Token>),
        Log(Vec<Token>),
        Floor(Vec<Token>),
        Ceil(Vec<Token>),
        Abs(Vec<Token>),
        Round(Vec<Token>),
        Sin(Vec<Token>),
        Cos(Vec<Token>),
        Tan(Vec<Token>),
    }
    impl Func {
        pub fn calc(&self) -> Result<Token, CalcError> {
            let error = Err(CalcError::Parse(String::from("`calculate` did not return a `Token::Num`")));
            match self {
                Func::Sqrt(v) => if let Token::Num(n) = calculate(v)? {
                    Ok(Token::Num(n.sqrt()))
                } else { error },
                Func::Cbrt(v) => if let Token::Num(n) = calculate(v)? {
                    Ok(Token::Num(n.cbrt()))
                } else { error },
                Func::Log(v) => if let Token::Num(n) = calculate(v)? {
                    Ok(Token::Num(n.log10()))
                } else { error },
                Func::Floor(v) => if let Token::Num(n) = calculate(v)? {
                    Ok(Token::Num(n.floor()))
                } else { error },
                Func::Ceil(v) => if let Token::Num(n) = calculate(v)? {
                    Ok(Token::Num(n.ceil()))
                } else { error },
                Func::Abs(v) => if let Token::Num(n) = calculate(v)? {
                    Ok(Token::Num(n.abs()))
                } else { error },
                Func::Round(v) => if let Token::Num(n) = calculate(v)? {
                    Ok(Token::Num(n.round()))
                } else { error },
                Func::Sin(v) => if let Token::Num(n) = calculate(v)? {
                    Ok(Token::Num(n.sin()))
                } else { error },
                Func::Cos(v) => if let Token::Num(n) = calculate(v)? {
                    Ok(Token::Num(n.cos()))
                } else { error },
                Func::Tan(v) => if let Token::Num(n) = calculate(v)? {
                    Ok(Token::Num(n.tan()))
                } else { error },
            }
        }
    }

    /// Represents a token in a mathematical expression
    #[derive(Debug, PartialEq, Clone)]
    pub enum Token {
        Num(f32),
        Op(Oper),
        Id(String),
        Fn(Func),
    }
    impl std::fmt::Display for Token {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Token::Num(v) => write!(f, "Num({v})"),
                Token::Op(v) => write!(f, "Op({v})"),
                Token::Id(v) => write!(f, "Id(\"{v}\")"),
                Token::Fn(v) => write!(f, "Fn({v:?})"),
            }
        }
    }

    /// Represents possible errors when calculating
    #[derive(Debug)]
    pub enum CalcError {
        Length(String),
        Parse(String),
    }
    impl From<std::num::ParseFloatError> for CalcError {
        fn from(value: <f32 as std::str::FromStr>::Err) -> Self {
            CalcError::Parse(value.to_string())
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use super::Token::{Num, Op, Id};
        use super::Oper::{LPar, RPar, Exp, Mul, Div, Mod, Add, Sub};
        use super::Func::{Sqrt, Cbrt, Log, Floor, Ceil, Abs, Round, Sin, Cos, Tan};

        #[test]
        fn test_tokenize() {
            let expr1 = "6^8*(4-1)*(3%2+(4*2))";
            let expr2 = "abc123%^*";

            assert_eq!(
                tokenize(expr1).unwrap(),
                vec![
                    Num(6.0),
                    Op(Exp),
                    Num(8.0),
                    Op(Mul),
                    Op(LPar),
                    Num(4.0),
                    Op(Sub),
                    Num(1.0),
                    Op(RPar),
                    Op(Mul),
                    Op(LPar),
                    Num(3.0),
                    Op(Mod),
                    Num(2.0),
                    Op(Add),
                    Op(LPar),
                    Num(4.0),
                    Op(Mul),
                    Num(2.0),
                    Op(RPar),
                    Op(RPar),
                ]
            );

            assert_eq!(
                tokenize(expr2).unwrap(),
                vec![
                    Id("abc".to_string()),
                    Num(123.0),
                    Op(Mod),
                    Op(Exp),
                    Op(Mul),
                ]
            )
        }

        #[test]
        fn test_simple_calc() {
            let tokens1 = &vec![Num(2.0), Op(Mul), Num(4.0)];
            let tokens2 = &vec![Num(3.0), Op(Mod), Num(2.0)];
            let tokens3 = &vec![Num(4.0), Op(Exp), Num(4.0)];

            assert_eq!(simple_calc(tokens1).unwrap(), Num(8.0));
            assert_eq!(simple_calc(tokens2).unwrap(), Num(1.0));
            assert_eq!(simple_calc(tokens3).unwrap(), Num(256.0));
        }

        #[test]
        fn test_calculate_operator() {
            let tokens1 = vec![
                Num(2.0),
                Op( Exp),
                Num(2.0),
                Op( Add),
                Num(5.0)
            ];

            assert_eq!(
                calculate_operator(tokens1.clone(), &[Exp]).unwrap(),
                vec![Num(4.0), Op(Add), Num(5.0)]
            );

            assert_eq!(
                calculate_operator(tokens1, &[Add]).unwrap(),
                vec![Num(2.0), Op(Exp), Num(7.0)]
            );
        }

        #[test]
        fn test_calculate() {
            let tokens1 = vec![Num(4.0), Op(Add), Num(2.0), Op(Mul), Num(2.0)]; // Order of operations
            let tokens2 = vec![Num(2.0), Op(Sub), Num(3.0), Op(Add), Num(4.0), Op(Mod), Num(5.0), Op(Div), Num(6.0), Op(Mul), Num(7.0), Op(Exp), Num(8.0)];
            let tokens3 = vec![Op(LPar), Num(4.0), Op(Add), Num(2.0), Op(RPar), Op(Mul), Num(2.0)]; // Parentheses
            let tokens4 = vec![Op(LPar), Num(2.0), Op(RPar)]; // Unnecessary parentheses
            let tokens5 = vec![Num(2.0), Op(LPar), Num(3.0), Op(RPar)]; // Implicit multiplication

            assert_eq!(Token::Num(8.0), calculate(&tokens1).unwrap());
            assert_eq!(Token::Num(3_843_199.8), calculate(&tokens2).unwrap());
            assert_eq!(Token::Num(12.0), calculate(&tokens3).unwrap());
            assert_eq!(Token::Num(2.0), calculate(&tokens4).unwrap());
            assert_eq!(Token::Num(6.0), calculate(&tokens5).unwrap());
        }

        #[test]
        fn test_convert_implicit_mul() {
            let tokens1 = vec![Num(4.0), Num(4.0)];
            let tokens2 = vec![Id(String::from("a")), Id(String::from("b"))];
            let tokens3 = vec![Num(4.0), Id(String::from("a"))];

            assert_eq!(vec![Num(4.0), Op(Mul), Num(4.0)], convert_implicit_mul(tokens1).unwrap());
            assert_eq!(vec![Id(String::from("a")), Op(Mul), Id(String::from("b"))], convert_implicit_mul(tokens2).unwrap());
            assert_eq!(vec![Num(4.0), Op(Mul), Id(String::from("a"))], convert_implicit_mul(tokens3).unwrap());
        }

        #[test]
        fn test_func_calc() {
            let f01 = Sqrt(vec![Num(4.0), Op(Mul), Num(8.0)]);
            assert_eq!(Token::Num(32.0_f32.sqrt()), f01.calc().unwrap());

            let f02 = Cbrt(vec![Num(27.0)]);
            assert_eq!(Token::Num(3.0), f02.calc().unwrap());

            let f03 = Log(vec![Num(1000.0)]);
            assert_eq!(Token::Num(3.0), f03.calc().unwrap());

            let f04 = Floor(vec![Num(3.15)]);
            assert_eq!(Token::Num(3.0), f04.calc().unwrap());

            let f05 = Ceil(vec![Num(2.15)]);
            assert_eq!(Token::Num(3.0), f05.calc().unwrap());

            let f06 = Abs(vec![Num(-10.0)]);
            assert_eq!(Token::Num(10.0), f06.calc().unwrap());

            let f07 = Round(vec![Num(3.1)]);
            assert_eq!(Token::Num(3.0), f07.calc().unwrap());

            let f08 = Sin(vec![Num(2.0)]);
            assert_eq!(Token::Num(2.0_f32.sin()), f08.calc().unwrap());

            let f09 = Cos(vec![Num(2.0)]);
            assert_eq!(Token::Num(2.0_f32.cos()), f09.calc().unwrap());

            let f10 = Tan(vec![Num(2.0)]);
            assert_eq!(Token::Num(2.0_f32.tan()), f10.calc().unwrap());
        }
    }
}
