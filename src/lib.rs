// TODO: Validate syntax. Check correct number of parentheses, make sure no 2 operators in a row, etc
// TODO: Add tests for functions (tokenize, simple_calc, calculate_operator, calculate, extract_token_values)

// TODO: Add support for identifiers / variables (also `;` and a "context" to store variables?)

pub mod calc {
    use std::num::ParseFloatError;

    /// Constructs a `Vec<Token>` from a given `&str` representation of a mathematical expression
    pub fn tokenize(expr: &str) -> Result<Vec<Token>, CalcErr> {

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
                            None => return Err(CalcErr::from("Index out of bounds")),
                        };
                    }
                    tokens.push(Token::Num(expr[index..last_processed as usize + 1].parse::<f32>()?));
                },
                a if a.is_alphabetic() => {
                    // If it is a letter, walk through the next characters until it is not a letter, 
                    // then add that slice to `tokens` as a `Token::Id`
                    while indx_char.1.is_alphabetic() {
                        println!("(I, C): `{indx_char:?}`");
                        last_processed = indx_char.0 as i32;
                        indx_char = match indcs_chars.next() {
                            Some(i_c) => i_c,
                            None => return Err(CalcErr::from("Index out of bounds")),
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
                _ => return Err(CalcErr::from("Invalid character in expression")),
            }
        }

        println!("Token list: {tokens:?}");
        Ok(tokens)
    }

    /// Generate a `Token` from a given `&Vec<Token>`
    pub fn simple_calc(tokens: &Vec<Token>) -> Result<Token, CalcErr> {
        if tokens.is_empty() { return Err(CalcErr::from("Length cannot be `0`")); }

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
            Oper::LPar => Err(CalcErr::from("`(` is an invalid calculation operator")),
            Oper::RPar => Err(CalcErr::from("`)` is an invalid calculation operator")),
            Oper::FnStart => Err(CalcErr::from("`\\` is an invalid calculation operator")),
        };

        println!("Result of `simple_calc`: {res:?}");
        res
    }

    /// Takes a `Vec<Token>` and converts all occurrences of division and subtraction to their equivalents in multiplication and addition
    pub fn convert_div_sub(tokens: Vec<Token>) -> Result<Vec<Token>, CalcErr> {
        if tokens.is_empty() { return Err(CalcErr::from("Length cannot be `0`")); }
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
                            _ => return Err(CalcErr(format!("token at index `{} (after `/`) is not a `Token::Num`", i + 1)))
                        }
                    },
                    Oper::Sub => {
                        println!("Sub found at `{i}`");
                        match tokens[i+1].clone() {
                            Token::Num(n) => {
                                tokens[i] = Token::Op(Oper::Add);
                                tokens[i+1] = Token::Num(-n);
                            },
                            _ => return Err(CalcErr(format!("token at index `{} (after `-`) is not a `Token::Num`", i + 1)))
                        }
                    },
                    _ => (),
                }
            }

            i += 1;
        }

        Ok(tokens)
    }

    /// Takes a `Vec<Token>` and replaces all occurrences of `Op(FnStart), Id(), Op(LPar), ... , Op(RPar)` with `Fn(Func(...))`
    pub fn parse_functions(mut tokens: Vec<Token>) -> Result<Vec<Token>, CalcErr> {
        let mut i = 0;
        println!("\nstarting function parse");
        loop {

            // Get current token, break if end of `Vec`
            match tokens.get(i) {
                None => break,
                Some(token) => {
                    println!("Element `{token}` at index `{i}` of vec `{tokens:?}`");
                    // Check to see if the current token is a function start operator, if it isn't, do nothing
                    if token == &Token::Op(Oper::FnStart) {
                        println!("starting function parse at `{i}`");
                        // `id_str` is the string contained in the next token. If the next token is not a `Token::Id` return an error
                        let id_str = match tokens.get(i+1).cloned() {
                            None => return { println!("error: no token after `FnStart`"); Err(CalcErr::from("unexpected end of `Vec`")) },
                            Some(t) => if let Token::Id(id) = t {
                                println!("`id_str` is `{id}`");
                                id
                            } else {
                                return Err(CalcErr(format!("`{}` is an invalid function id", tokens.get(i+1).unwrap())));
                            }
                        };
                        // `func_tokens` is the `Vec<Token>` to be passed into the `Func`
                        let func_tokens: Vec<Token> = if let Some(t) = tokens.get(i+2) {
                            if t == &Token::Op(Oper::LPar) {
                                let start = i + 3;
                                let mut end = i + 4;
                                let mut open_parens = 1;
                                // Loop through elements inside the parentheses
                                loop {
                                    match tokens.get(end) {
                                        None => return Err(CalcErr::from("unexpected end of `Vec`")),
                                        Some(t) => if let Token::Op(o) = t {
                                            // Increase/decrease `open_parens` if the `Op` is a parentheses
                                            match o {
                                                Oper::LPar => open_parens += 1,
                                                Oper::RPar => open_parens -= 1,
                                                _ => (),
                                            }
                                        }
                                    }
                                    if open_parens <= 0 {
                                        // If all the parentheses are closed,
                                        // break and assign the contained `Vec` to `func_tokens` (including parentheses)
                                        break tokens.drain(start-1..end+1).collect();
                                    }
                                    end += 1;
                                }
                            } else {
                                // If the next token is not an open parentheses, return an error
                                return Err(CalcErr::from("missing open parentheses after function id"));
                            }
                        } else {
                            println!("no token after `Id`");
                            return Err(CalcErr::from("unexpected end of `Vec`"));
                        };

                        println!("`func_tokens` is `{func_tokens:?}`");
                        // Get the index of the first `Op(FnStart)` (this is the one currently being parsed)
                        let func_index = tokens.iter().position(|t| t == &Token::Op(Oper::FnStart)).unwrap();
                        // Delete the `Op(FnStart)` and `Id()`
                        let _ = tokens.drain(func_index..func_index+2);
                        // Insert `Fn`
                        tokens.insert(
                            func_index,
                            Token::Fn(
                                Func::try_from( // Generate a `Func` from the `id_str` and `func_tokens` (without the parentheses)
                                    (id_str.as_str(), func_tokens[1..func_tokens.len()-1].to_vec())
                                )?
                            )
                        );
                        i = 0;
                        continue;
                    }
                }
            }
            i += 1;
        }
        Ok(tokens)
    }

    /// Takes a `Vec<Token>` and calculates all occurrences of a `&str` operator
    pub fn calculate_operator(tokens: Vec<Token>, operators: &[Oper]) -> Result<Vec<Token>, CalcErr> {
        if tokens.is_empty() { return Err(CalcErr::from("Length cannot be `0`")); }

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
    pub fn calculate(tokens: &Vec<Token>) -> Result<Token, CalcErr> {
        if tokens.is_empty() { return Err(CalcErr::from("Length cannot be `0`")); }
        let mut tokens = tokens.clone();

        // TODO: Validate syntax. Check correct number of parentheses, make sure no 2 operators in a row, etc
        println!("Started `calculate`");

        // Pass 1: Parse each function, calculate each function
        tokens = parse_functions(tokens)?.into_iter().map(|t| {
            if let Token::Fn(f) = t {
                f.calc()
            } else {
                Ok(t)
            }
        }).collect::<Result<Vec<Token>, CalcErr>>()?;

        // Pass 2: Calculate and substitute parentheses
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

        // Pass 3: operators
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
            return Err(CalcErr::from("calculation error, final result was not a single `Token::Num`"));
        }

        Ok(tokens[0].clone())
    } 

    /// Generates a `(String, String, String)` containing the same values as the given `&Vec<Token>`
    fn extract_token_values(tokens: &Vec<Token>) -> Result<(f32, Oper, f32), CalcErr> {
        if tokens.is_empty() { return Err(CalcErr::from("Length cannot be `0`")); }

        let token0;
        let token1;
        let token2;

        // Verify length
        if tokens.len() != 3 {
            return Err(CalcErr::from("Length must be 3"));
        }

        // Verify tokens and extract values
        if let Token::Num(t) = &tokens[0] {
            token0 = t;
        } else {
            return Err(CalcErr::from("First token must be a `Token::Num`"));
        }
        if let Token::Op(t) = &tokens[1] {
            token1 = t;
        } else {
            return Err(CalcErr::from("Middle token must be a `Token::Op`"));
        }
        if let Token::Num(t) = &tokens[2] {
            token2 = t;
        } else {
            return Err(CalcErr::from("Last token must be a `Token::Num`"));
        }

        Ok((*token0, token1.clone(), *token2))
    }

    pub fn convert_implicit_mul(mut tokens: Vec<Token>) -> Result<Vec<Token>, CalcErr> {
        // Return error if `tokens` contains parentheses
        if tokens.contains(&Token::Op(Oper::LPar)) || tokens.contains(&Token::Op(Oper::RPar)) {
            return Err(CalcErr::from("cannot call `convert_implicit_mul` on a `Vec<Token>` that contains parentheses"));
        }

        // Insert `Token::Op(Oper::Mul)` between any `Token::Num` or `Token::Id`
        let mut i = 0;
        loop {

            match tokens[i] {
                Token::Num(_) | Token::Id(_) | Token::Fn(_) => match tokens.get(i+1) {
                    Some(t) => match t {
                        Token::Num(_) | Token::Id(_) | Token::Fn(_) => {
                            tokens.insert(i+1, Token::Op(Oper::Mul));
                            i = 0;
                            continue;
                        },
                        Token::Op(_) => (),
                    },
                    None => break,
                },
                Token::Op(_) => (),
            }

            i += 1;
        }

        Ok(tokens)
    }

    /// Does some simple syntax checks on a `Vec<Token>` and returns an error if there are any mistakes
    pub fn simple_syntax_check(tokens: &Vec<Token>) -> Result<(), CalcErr> {
        Err(CalcErr::from("unimplemented"))
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
        pub fn calc(&self) -> Result<Token, CalcErr> {
            let error = Err(CalcErr::from("`calculate` did not return a `Token::Num`"));
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
    impl TryFrom<(&str, Vec<Token>)> for Func {
        type Error = CalcErr;

        fn try_from(value: (&str, Vec<Token>)) -> Result<Self, Self::Error> {
            match value.0.to_lowercase().as_str() {
                "sqrt" => Ok(Func::Sqrt(value.1)),
                "cbrt" => Ok(Func::Cbrt(value.1)),
                "log" => Ok(Func::Log(value.1)),
                "floor" => Ok(Func::Floor(value.1)),
                "ceil" => Ok(Func::Ceil(value.1)),
                "abs" => Ok(Func::Abs(value.1)),
                "round" => Ok(Func::Round(value.1)),
                "sin" => Ok(Func::Sin(value.1)),
                "cos" => Ok(Func::Cos(value.1)),
                "tan" => Ok(Func::Tan(value.1)),
                _ => Err(CalcErr(format!("invalid function name: `{}`", value.0))),
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
    pub struct CalcErr(String);
    impl From<ParseFloatError> for CalcErr {
        fn from(value: ParseFloatError) -> Self {
            CalcErr(value.to_string())
        }
    }
    impl From<&str> for CalcErr {
        fn from(value: &str) -> Self {
            CalcErr(value.to_string())
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use super::Token::{Num, Op, Id, Fn};
        use super::Oper::{LPar, RPar, Exp, Mul, Div, Mod, Add, Sub, FnStart};
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
            let tokens6 = vec![Op(FnStart), Id(String::from("sqrt")), Op(LPar), Num(4.0), Op(RPar), Op(FnStart), Id(String::from("log")), Op(LPar), Num(100.0), Op(RPar)]; // Functions
            // let tokens7 = vec![]; // Implicit multiplication
            

            assert_eq!(Token::Num(8.0), calculate(&tokens1).unwrap());
            assert_eq!(Token::Num(3_843_199.8), calculate(&tokens2).unwrap());
            assert_eq!(Token::Num(12.0), calculate(&tokens3).unwrap());
            assert_eq!(Token::Num(2.0), calculate(&tokens4).unwrap());
            assert_eq!(Token::Num(6.0), calculate(&tokens5).unwrap());
            assert_eq!(Token::Num(4.0), calculate(&tokens6).unwrap());
        }

        #[test]
        fn test_convert_implicit_mul() {
            let tokens1 = vec![Num(4.0), Num(4.0)]; // `Num`
            let tokens2 = vec![Id(String::from("a")), Id(String::from("b"))]; // `Id`
            let tokens3 = vec![Num(4.0), Id(String::from("a"))]; // `Num` `Id`
            let tokens4 = vec![Fn(Sqrt(vec![Num(2.0)])), Fn(Sqrt(vec![Num(2.0)]))]; // `Fn`
            let tokens5 = vec![Fn(Sqrt(vec![Num(2.0)])), Num(2.0)]; // `Fn` `Num`

            assert_eq!(vec![Num(4.0), Op(Mul), Num(4.0)], convert_implicit_mul(tokens1).unwrap());
            assert_eq!(vec![Id(String::from("a")), Op(Mul), Id(String::from("b"))], convert_implicit_mul(tokens2).unwrap());
            assert_eq!(vec![Num(4.0), Op(Mul), Id(String::from("a"))], convert_implicit_mul(tokens3).unwrap());
            assert_eq!(vec![Fn(Sqrt(vec![Num(2.0)])), Op(Mul), Fn(Sqrt(vec![Num(2.0)]))], convert_implicit_mul(tokens4).unwrap());
            assert_eq!(vec![Fn(Sqrt(vec![Num(2.0)])), Op(Mul), Num(2.0)], convert_implicit_mul(tokens5).unwrap());
        }

        #[test]
        fn test_parse_functions() {
            let tokens1 = vec![Op(FnStart), Id(String::from("sqrt")), Op(LPar), Num(2.0), Op(RPar)];
            let tokens2 = vec![Op(FnStart), Id(String::from("Sqrt")), Op(LPar), Num(2.0), Op(RPar)]; // Capitalization
            let tokens3 = vec![Op(FnStart), Id(String::from("log")), Op(LPar), Num(2.0), Op(Mul), Num(2.0), Op(RPar)]; // Expression passed to function
            let tokens4 = vec![Op(FnStart), Id(String::from("sin")), Op(LPar), Num(2.0), Op(RPar)];

            assert_eq!(vec![Fn(Func::Sqrt(vec![Num(2.0)]))], parse_functions(tokens1).unwrap());
            assert_eq!(vec![Fn(Func::Sqrt(vec![Num(2.0)]))], parse_functions(tokens2).unwrap());
            assert_eq!(vec![Fn(Func::Log(vec![Num(2.0), Op(Mul), Num(2.0)]))], parse_functions(tokens3).unwrap());
            assert_eq!(vec![Fn(Func::Sin(vec![Num(2.0)]))], parse_functions(tokens4).unwrap());

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
    
        #[test]
        fn test_simple_syntax_check() {
            // TODO: Add more test cases
            let tokens1 = &vec![Token::Num(2.0)];

            assert!(simple_syntax_check(tokens1).is_ok());
        }
    }
}
