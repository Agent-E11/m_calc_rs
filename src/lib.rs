// TODO: Create doc tests for all pub functions
// TODO: Delete all debug `println`s

pub use calc::calculate;
pub use calc::tokenize;
pub use calc::display_expr;
pub use calc::init_logger;
pub use calc::{Token, Oper, Func};
pub use calc::CalcErr;

pub mod calc {
    use std::cmp::Ordering;
    use std::num::ParseFloatError;
    use std::collections::HashMap;
    use std::convert::TryFrom;
    use env_logger::Env;
    use log::{error, warn, info, debug, trace};

    pub fn init_logger(log_var: &str, style_var: &str) {
        let env = Env::default()
            .filter_or(log_var, "error")
            .write_style_or(style_var, "always");

        env_logger::init_from_env(env);
    }

    /// Creates a `String` representation of a `Vec<Token>`
    /// 
    /// # Examples
    /// 
    /// ```
    /// use m_calc::{display_expr, Token, Oper};
    /// 
    /// let tokens = vec![
    ///     Token::Num(2.0),
    ///     Token::Op(Oper::Add),
    ///     Token::Num(3.15),
    ///     Token::Op(Oper::Mul),
    ///     Token::Id("x".to_string()),
    /// ];
    /// 
    /// assert_eq!("2+3.15*x", display_expr(&tokens));
    /// 
    /// use m_calc::Func;
    /// 
    /// let tokens = vec![
    ///     Token::Fn(Func::Sin(vec![
    ///         Token::Num(2.11),
    ///         Token::Op(Oper::Add),
    ///         Token::Num(1.04),
    ///     ])),
    /// ];
    /// 
    /// assert_eq!("\\sin(2.11+1.04)", display_expr(&tokens));
    /// ```
    pub fn display_expr(tokens: &[Token]) -> String {
        tokens.iter().map(|token| token.display()).collect()
    }

    /// Constructs a `Vec<Token>` from a given `&str` representation of a mathematical expression
    /// 
    /// # Examples
    /// 
    /// ```
    /// use m_calc::{tokenize, Token, Func, Oper};
    /// 
    /// let tokens = tokenize("2+3.15*x%(\\sin(3.14)+1)").unwrap();
    /// 
    /// assert_eq!(
    ///     vec![
    ///         Token::Num(2.0),
    ///         Token::Op(Oper::Add),
    ///         Token::Num(3.15),
    ///         Token::Op(Oper::Mul),
    ///         Token::Id("x".to_string()),
    ///         Token::Op(Oper::Mod),
    ///         Token::Op(Oper::LPar),
    ///             Token::Op(Oper::FnStart),
    ///             Token::Id("sin".to_string()),
    ///             Token::Op(Oper::LPar),
    ///             Token::Num(3.14),
    ///             Token::Op(Oper::RPar),
    ///             Token::Op(Oper::Add),
    ///             Token::Num(1.0),
    ///         Token::Op(Oper::RPar),
    ///     ],
    ///     tokens
    /// );
    /// ```
    pub fn tokenize(expr: &str) -> Result<Vec<Token>, CalcErr> {
        debug!("started `tokenize` with string: `{expr}`");

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
                        debug!("index: `{}`, char: `{}`", indx_char.0, indx_char.1);
                        last_processed = indx_char.0 as i32;
                        indx_char = match indcs_chars.next() {
                            Some(i_c) => i_c,
                            None => break,
                        };
                    }
                    let num_token = Token::Num(expr[index..last_processed as usize + 1].parse::<f32>()?);
                    debug!("adding number to tokens: `{}`", num_token);
                    tokens.push(num_token);
                },
                a if a.is_alphabetic() => {
                    // If it is a letter, walk through the next characters until it is not a letter, 
                    // then add that slice to `tokens` as a `Token::Id`
                    while indx_char.1.is_alphabetic() {
                        debug!("index: `{}`, char: `{}`", indx_char.0, indx_char.1);
                        last_processed = indx_char.0 as i32;
                        indx_char = match indcs_chars.next() {
                            Some(i_c) => i_c,
                            None => break,
                        };
                    }
                    let id_token = Token::Id(expr[index..last_processed as usize + 1].to_owned());
                    debug!("adding id to tokens: `{}`", id_token);
                    tokens.push(id_token);
                },
                o if !o.is_alphabetic() && !o.is_numeric() => {
                    // If it neither a number, nor a letter,
                    // add the _single_ character to `tokens` as a `Token::Op`
                    debug!("index: `{}`, char: `{}`", indx_char.0, indx_char.1);
                    last_processed = indx_char.0 as i32;
                    let oper_token = Token::Op(Oper::try_from(&expr[index..last_processed as usize + 1])?);
                    debug!("adding operator to tokens: `{}`", oper_token);
                    tokens.push(oper_token);
                },
                // If it is none of these, return an error
                _ => return Err(CalcErr::from("invalid character in expression")),
            }
        }

        debug!("returning from `tokenize` with tokens: `{:?}`", tokens);
        Ok(tokens)
    }

    /// Generate a `Token` from a given `&Vec<Token>`
    pub fn simple_calc(tokens: &Vec<Token>) -> Result<Token, CalcErr> {
        if tokens.is_empty() { return Err(CalcErr::from("length cannot be `0`")); }

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
            Oper::Eql => Err(CalcErr::from("`=` is an invalid calculation operator")),
            Oper::LnBr => Err(CalcErr::from("`;` is an invalid calculation operator")),
            Oper::FnStart => Err(CalcErr::from("`\\` is an invalid calculation operator")),
        };

        println!("Result of `simple_calc`: {res:?}");
        res
    }

    /// Takes a `Vec<Token>` and converts all occurrences of division and subtraction to their equivalents in multiplication and addition
    pub fn convert_div_sub(tokens: Vec<Token>) -> Result<Vec<Token>, CalcErr> {
        if tokens.is_empty() { return Err(CalcErr::from("length cannot be `0`")); }
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
                            _ => return Err(CalcErr(format!("token at index `{}` (after `/`) is not a `Token::Num`", i + 1)))
                        }
                    },
                    Oper::Sub => {
                        println!("Sub found at `{i}`");
                        match tokens[i+1].clone() {
                            Token::Num(n) => {
                                tokens[i] = Token::Op(Oper::Add);
                                tokens[i+1] = Token::Num(-n);
                            },
                            _ => return Err(CalcErr(format!("token at index `{}` (after `-`) is not a `Token::Num`", i + 1)))
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
    fn parse_functions(mut tokens: Vec<Token>) -> Result<Vec<Token>, CalcErr> {
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
    fn calculate_operator(tokens: Vec<Token>, operators: &[Oper]) -> Result<Vec<Token>, CalcErr> {
        if tokens.is_empty() { return Err(CalcErr::from("length cannot be `0`")); }

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
    pub fn calculate(tokens: &Vec<Token>, context: &mut HashMap<String, Vec<Token>>) -> Result<Token, CalcErr> {
        if tokens.is_empty() { return Err(CalcErr::from("length cannot be `0`")); }
        let mut tokens = tokens.clone();

        println!("Started `calculate`");

        // Delete trailing `Op(LnBr)`
        if tokens.ends_with(&[Token::Op(Oper::LnBr)]) {
            tokens.pop();
            println!("Deleted trailing `Op(LnBr)`");
        }

        // Check syntax, return `Err` if any
        simple_syntax_check(&tokens)?;

        println!("`context` before assignments: `{context:?}`");
        let tokens = parse_assignment(tokens, context)?;
        println!("`context` after assignments: `{context:?}`");
        
        // Make substitutions
        let mut tokens = substitute_assignments(tokens, context)?;
        
        if tokens.is_empty() { return Ok(Token::Empty); }

        // Pass 1: Parse each function, calculate each function
        tokens = parse_functions(tokens)?.into_iter().map(|t| {
            if let Token::Fn(f) = t {
                f.calc(context)
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
                let res = calculate(subsection, context)?;

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

        Ok(tokens[0].clone())
    } 

    /// Takes a `Vec<Token>` and a `Hashmap<String, Vec<Token>>`.
    /// Takes all the assignment expressions in the `Vec<Token>`,
    /// deletes them and converts it into a key value pair in the `Hashmap<String, Vec<Token>>`
    // FIXME: If a variable is assigned to itself (e.g. `a = a`), it will break the substitution
    fn parse_assignment(tokens: Vec<Token>, context: &mut HashMap<String, Vec<Token>>) -> Result<Vec<Token>, CalcErr> {
        let exprs = tokens.split(|t| t == &Token::Op(Oper::LnBr));
        let mut new_tokens: Vec<Token> = Vec::new();

        println!("Starting `parse_assignments`");

        for expr in exprs {
            println!("Expression: `{expr:?}`");
            // Validate syntax
            // Check if empty
            if expr.is_empty() { continue; }

            match expr.iter().filter(|&t| *t == Token::Op(Oper::Eql)).count().cmp(&1) {
                // If there is more than 1 `Op(Eql)`, return an error
                Ordering::Greater => return Err(CalcErr(format!("multiple `Op(Eql)` in expression `{}`", "UNIMPLEMENTED"))),
                // If it is not an assignment, add the expression to `new_tokens` (it will be calculated later)
                Ordering::Less => {
                    new_tokens.append(&mut expr.to_vec());
                    new_tokens.push(Token::Op(Oper::LnBr))
                },
                // If there is 1 `Op(Eql)`, parse the assignment expression
                Ordering::Equal => {
                    println!("There is 1 assignment in `expr`");
                    let id = if let Token::Id(i) = &expr[0] {
                        println!("The `id` is `{i}`");
                        i
                    } else {
                        return Err(CalcErr::from("first token of assignment expression must be an `Id`"));
                    };
                    if expr[1] != Token::Op(Oper::Eql) {
                        println!("Second token must be `Op(Eql)` not `{:?}`", expr[1]);
                        return Err(CalcErr(format!("second token of assignment expression must be an `Op(Eql)`, not `{}`", expr[1])));
                    }

                    let insert_value = if expr[2..expr.len()].iter().any(|t| matches!(t, Token::Id(_))) {
                        println!("contains an `Id`, not calculating");
                        expr[2..expr.len()].to_vec()
                    } else {
                        println!("Does not contain an `Id`, calculating");
                        [calculate(&expr[2..expr.len()].to_vec(), context)?].to_vec()
                    };

                    println!("The value assigned to `{id}` is `{insert_value:?}`");

                    // Add the calculated value to the `context`
                    context.insert(
                        id.to_string(), 
                        insert_value,
                    );
                    println!("Context: `{context:?}`");
                }
            }
        }

        println!("New tokens: `{new_tokens:?}`");
        Ok(new_tokens)
    }

    /// Takes a `Vec<Token>` and replaces all `Id`s with their values defined in the given `context`
    fn substitute_assignments(tokens: Vec<Token>, context: &HashMap<String, Vec<Token>>) -> Result<Vec<Token>, CalcErr> {
        let mut tokens = tokens;

        if tokens.contains(&Token::Op(Oper::Eql)) {
            return Err(CalcErr::from("cannot substitute assignments for an expression containing `Op(Eql)`"));
        }

        let mut i = 0;
        // Loop through all tokens
        loop {
            println!("Token at `{i}`: `{:?}`", tokens.get(i));
            match tokens.get(i) {
                None => break,
                Some(t) => if let Token::Id(s) = t {
                    // If the token is an `Id`, get its associated value in `context`
                    println!("The token is `Some`");
                    let value = match context.get(s) {
                        None => {
                            i += 1;
                            continue;
                        },
                        Some(v) => {println!("Token is: `{v:?}`"); v},
                    };

                    // Enclose the value in parentheses
                    let mut enclosed = vec![Token::Op(Oper::LPar)];
                    enclosed.extend(value.clone());
                    enclosed.push(Token::Op(Oper::RPar));

                    // Substitute the `Id` for the `Id`s value
                    tokens.splice(i..i+1, enclosed.clone());
                    println!("Substituted:\n`{tokens:?}`");

                    i = 0;
                    continue;
                } else {
                    println!("Token is not an `Id`");
                }
            }

            i += 1;
        };

        Ok(tokens)
    }

    /// Generates a `(f32, Oper, f32)` containing the same values as the given `&Vec<Token>`
    fn extract_token_values(tokens: &Vec<Token>) -> Result<(f32, Oper, f32), CalcErr> {
        if tokens.is_empty() { return Err(CalcErr::from("length cannot be `0`")); }

        let token0;
        let token1;
        let token2;

        // Verify length
        if tokens.len() != 3 {
            return Err(CalcErr::from("length must be 3"));
        }

        // Verify tokens and extract values
        if let Token::Num(t) = &tokens[0] {
            token0 = t;
        } else {
            return Err(CalcErr::from("first token must be a `Token::Num`"));
        }
        if let Token::Op(t) = &tokens[1] {
            token1 = t;
        } else {
            return Err(CalcErr::from("middle token must be a `Token::Op`"));
        }
        if let Token::Num(t) = &tokens[2] {
            token2 = t;
        } else {
            return Err(CalcErr::from("last token must be a `Token::Num`"));
        }

        Ok((*token0, token1.clone(), *token2))
    }

    fn convert_implicit_mul(tokens: Vec<Token>) -> Result<Vec<Token>, CalcErr> {
        // Return error if `tokens` contains parentheses
        if tokens.contains(&Token::Op(Oper::LPar)) || tokens.contains(&Token::Op(Oper::RPar)) {
            return Err(CalcErr::from("cannot call `convert_implicit_mul` on a `Vec<Token>` that contains parentheses"));
        }

        // Delete `Empty` variants
        let mut tokens: Vec<Token> = tokens.into_iter().filter(|t| !matches!(t, Token::Empty)).collect();

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
                        Token::Op(_) | Token::Empty => (),
                    },
                    None => break,
                },
                Token::Op(_) | Token::Empty => match tokens.get(i+1) {
                    Some(_) => (),
                    None => break,
                },
            }

            i += 1;
        }

        Ok(tokens)
    }

    /// Does some simple syntax checks on a `Vec<Token>` and returns an error if there are any mistakes
    /// 
    fn simple_syntax_check(tokens: &[Token]) -> Result<(), CalcErr> {
        if tokens.is_empty() { return Err(CalcErr::from("length cannot be `0`")); }

        println!("starting syntax check");

        // Check dangling operators
        // Beginning
        if let Token::Op(o) = &tokens[0] {
            match o {
                Oper::RPar | Oper::Exp | Oper::Mul | Oper::Div | Oper::Mod | Oper::Add | Oper::Eql | Oper::LnBr => {
                    return Err(CalcErr::from("first token is invalid"));
                },
                Oper::FnStart | Oper::LPar | Oper::Sub => (),
            }
        }
        // End
        if let Token::Op(o) = &tokens.last().unwrap() {
            match o {
                Oper::LPar | Oper::Exp | Oper::Mul | Oper::Div | Oper::Mod | Oper::Add | Oper::Sub | Oper::Eql => {
                    return Err(CalcErr::from("last token is invalid"));
                },
                Oper::FnStart | Oper::RPar | Oper::LnBr => (),
            }
        }

        // Check number of parentheses (and empty parentheses)
        let mut open_parens = 0;
        let mut last_paren = 0;
        for (i, token) in tokens.iter().enumerate() {
            if let Token::Op(o) = token {
                match o {
                    Oper::LPar => {
                        // Check for empty parens
                        if tokens[i+1] == Token::Op(Oper::RPar) { return Err(CalcErr(format!("empty parentheses at `{i}`"))); }
                        open_parens += 1;
                        last_paren = i
                    },
                    Oper::RPar => open_parens -= 1,
                    _ => (),
                }
            }
            if open_parens < 0 {
                return Err(CalcErr(format!("unexpected closing parentheses at `{i}`")));
            }
        }
        if open_parens != 0 {
            return Err(CalcErr(format!("unclosed parentheses at `{last_paren}`")));
        }
        
        println!("tokens: {tokens:?}");
        // Check for 2 `Op` in a row
        for (i, token) in tokens.iter().enumerate() {
            println!("`{token}` at `{i}`");
            if let Token::Op(op1) = token {
                match op1 {
                    Oper::RPar | Oper::LnBr => continue,
                    _ => {
                        if let Token::Op(op2) = &tokens[i+1] {
                            match op2 {
                                Oper::LPar => continue,
                                _ => return Err(CalcErr(format!("2 `Op` in a row at {i}"))),
                            }
                        }
                    }
                }
            }
        }

        Ok(())
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
        Eql, // Assign (equal sign)
        LnBr, // Line break
        FnStart, // Signifies that the next token (an Id), will be the name of a function
    }
    impl Oper {
        pub fn display(&self) -> String {
            match self {
                Self::LPar => "(",
                Self::RPar => ")",
                Self::Exp => "^",
                Self::Mul => "*",
                Self::Div => "/",
                Self::Mod => "%",
                Self::Add => "+",
                Self::Sub => "-",
                Self::Eql => "=",
                Self::LnBr => ";",
                Self::FnStart => "\\",
            }.to_string()
        }
    }
    impl From<&str> for Oper {
        fn from(value: &str) -> Self {
            match value {
                "(" => Ok(Self::LPar),
                ")" => Ok(Self::RPar),
                "^" => Ok(Self::Exp),
                "*" => Ok(Self::Mul),
                "/" => Ok(Self::Div),
                "%" => Ok(Self::Mod),
                "+" => Ok(Self::Add),
                "-" => Ok(Self::Sub),
                "=" => Ok(Self::Eql),
                ";" => Ok(Self::LnBr),
                "\\" => Ok(Self::FnStart),
                o => Err(CalcErr(format!("invalid character for an `Oper`: `{o}`"))),
            }
        }
    }
    impl std::fmt::Display for Oper {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{self:?}")
        }
    }

    /// Represents a mathematical function
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
        pub fn calc(&self, context: &mut HashMap<String, Vec<Token>>) -> Result<Token, CalcErr> {
            let error = Err(CalcErr::from("`calculate` did not return a `Token::Num`"));
            match self {
                Self::Sqrt(v) => if let Token::Num(n) = calculate(v, context)? {
                    Ok(Token::Num(n.sqrt()))
                } else { error },
                Self::Cbrt(v) => if let Token::Num(n) = calculate(v, context)? {
                    Ok(Token::Num(n.cbrt()))
                } else { error },
                Self::Log(v) => if let Token::Num(n) = calculate(v, context)? {
                    Ok(Token::Num(n.log10()))
                } else { error },
                Self::Floor(v) => if let Token::Num(n) = calculate(v, context)? {
                    Ok(Token::Num(n.floor()))
                } else { error },
                Self::Ceil(v) => if let Token::Num(n) = calculate(v, context)? {
                    Ok(Token::Num(n.ceil()))
                } else { error },
                Self::Abs(v) => if let Token::Num(n) = calculate(v, context)? {
                    Ok(Token::Num(n.abs()))
                } else { error },
                Self::Round(v) => if let Token::Num(n) = calculate(v, context)? {
                    Ok(Token::Num(n.round()))
                } else { error },
                Self::Sin(v) => if let Token::Num(n) = calculate(v, context)? {
                    Ok(Token::Num(n.sin()))
                } else { error },
                Self::Cos(v) => if let Token::Num(n) = calculate(v, context)? {
                    Ok(Token::Num(n.cos()))
                } else { error },
                Self::Tan(v) => if let Token::Num(n) = calculate(v, context)? {
                    Ok(Token::Num(n.tan()))
                } else { error },
            }
        }
    
        pub fn display(&self) -> String {
            let (fn_id, tokens) = match self {
                Self::Sqrt(v) => ("sqrt", v.clone()),
                Self::Cbrt(v) => ("cbrt", v.clone()),
                Self::Log(v) => ("log", v.clone()),
                Self::Floor(v) => ("floor", v.clone()),
                Self::Ceil(v) => ("ceil", v.clone()),
                Self::Abs(v) => ("abs", v.clone()),
                Self::Round(v) => ("round", v.clone()),
                Self::Sin(v) => ("sin", v.clone()),
                Self::Cos(v) => ("cos", v.clone()),
                Self::Tan(v) => ("tan", v.clone()),
            };

            format!("\\{}({})", fn_id, display_expr(&tokens))
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
        Empty,
    }
    impl Token {
        pub fn display(&self) -> String {
            match self {
                Self::Num(n) => n.to_string(),
                Self::Op(o) => o.display(),
                Self::Id(i) => i.to_string(),
                Self::Fn(f) => f.display(),
                Self::Empty => "".to_string(),
            }
        }
    }

    impl std::fmt::Display for Token {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Num(v) => write!(f, "Num({v})"),
                Self::Op(v) => write!(f, "Op({v})"),
                Self::Id(v) => write!(f, "Id(\"{v}\")"),
                Self::Fn(v) => write!(f, "Fn({v:?})"),
                Self::Empty => write!(f, "Empty"),
            }
        }
    }

    /// Represents possible errors when calculating
    #[derive(Debug)]
    pub struct CalcErr(pub String);
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
        use super::Token::{Num, Op, Id, Fn, Empty};
        use super::Oper::{LPar, RPar, Exp, Mul, Div, Mod, Add, Sub, Eql, LnBr, FnStart};
        use super::Func::{Sqrt, Cbrt, Log, Floor, Ceil, Abs, Round, Sin, Cos, Tan};

        #[test]
        fn test_token_display() {
            let t1 = Num(2.0);
            let t2 = Num(3.15);
            let t3 = Op(LPar);
            let t4 = Id(String::from("time"));
            let t5 = Fn(Sqrt(vec![]));
            let t6 = Fn(Cos(vec![Num(5.0)]));
            let t7 = Fn(Tan(vec![Num(1.03), Op(Add), Num(2.11)]));
            let t8 = Empty;

            assert_eq!("2", t1.display());
            assert_eq!("3.15", t2.display());
            assert_eq!("(", t3.display());
            assert_eq!("time", t4.display());
            assert_eq!("\\sqrt()", t5.display());
            assert_eq!("\\cos(5)", t6.display());
            assert_eq!("\\tan(1.03+2.11)", t7.display());
            assert_eq!("", t8.display());
        }

        #[test]
        fn test_oper_display() {
            // TODO:
        }

        #[test]
        fn test_func_display() {
            // TODO:
        }

        #[test]
        fn test_display_expr() {
            let tokens1 = vec![Num(5.0), Op(Add), Fn(Sin(vec![Num(3.15)])), Op(LPar), Num(3.0), Op(Sub), Id(String::from("a")), Op(RPar)];

            assert_eq!("5+\\sin(3.15)(3-a)".to_string(), display_expr(&tokens1));
        }

        #[test]
        fn test_tokenize() {
            let expr1 = "6^8*(4-1)*(3%2+(4*2))";
            let expr2 = "abc123%^*";
            let expr3 = "1";

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
            );

            assert_eq!(
                tokenize(expr3).unwrap(),
                vec![Num(1.0)],
            );
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
            let mut context: HashMap<String, Vec<Token>> = HashMap::new();

            let tokens1  = vec![Num(4.0), Op(Add), Num(2.0), Op(Mul), Num(2.0)]; // Order of operations
            let tokens2  = vec![Num(2.0), Op(Sub), Num(3.0), Op(Add), Num(4.0), Op(Mod), Num(5.0), Op(Div), Num(6.0), Op(Mul), Num(7.0), Op(Exp), Num(8.0)]; // Very long and complicated
            let tokens3  = vec![Op(LPar), Num(4.0), Op(Add), Num(2.0), Op(RPar), Op(Mul), Num(2.0)]; // Parentheses
            let tokens4  = vec![Op(LPar), Num(2.0), Op(RPar)]; // Unnecessary parentheses
            let tokens5  = vec![Num(2.0), Op(LPar), Num(3.0), Op(RPar)]; // Implicit multiplication
            let tokens6  = vec![Op(FnStart), Id(String::from("sqrt")), Op(LPar), Num(4.0), Op(RPar), Op(FnStart), Id(String::from("log")), Op(LPar), Num(100.0), Op(RPar)]; // Functions
            let tokens7  = vec![Num(1.0), Op(Add), Num(2.0), Op(LnBr)]; // Trailing `Op(LnBr)`
            let tokens8  = vec![Id(String::from("a")), Op(Eql), Num(20.0), Op(LnBr), Num(1.0), Op(Add), Num(2.0)]; // Assignment and calculate
            let tokens9  = vec![Num(5.0), Op(Mul), Id(String::from("a"))]; // Assignment substitution
            let tokens10 = vec![Id(String::from("b")), Op(Eql), Num(1.0), Op(Add), Num(2.0), Op(LnBr), Num(4.0), Op(Mul), Id(String::from("b"))]; // Order of operations
            let tokens11 = vec![Id(String::from("a")), Op(Eql), Num(10.0)]; // Reassignment and empty calculation

            assert_eq!(Token::Num(8.0), calculate(&tokens1, &mut context).unwrap()); println!("----- 1 -----"); // Order of operations
            assert_eq!(Token::Num(3_843_199.8), calculate(&tokens2, &mut context).unwrap()); println!("----- 2 -----"); // Very long and complicated
            assert_eq!(Token::Num(12.0), calculate(&tokens3, &mut context).unwrap()); println!("----- 3 -----"); // Parentheses
            assert_eq!(Token::Num(2.0), calculate(&tokens4, &mut context).unwrap()); println!("----- 4 -----"); // Unnecessary parentheses
            assert_eq!(Token::Num(6.0), calculate(&tokens5, &mut context).unwrap()); println!("----- 5 -----"); // Implicit multiplication
            assert_eq!(Token::Num(4.0), calculate(&tokens6, &mut context).unwrap()); println!("----- 6 -----"); // Functions
            assert_eq!(Token::Num(3.0), calculate(&tokens7, &mut context).unwrap()); println!("----- 7 -----"); // Trailing `Op(LnBr)`
            assert_eq!(Token::Num(3.0), calculate(&tokens8, &mut context).unwrap()); println!("----- 8 -----"); // Assign and calculate
            assert_eq!(&vec![Num(20.0)], context.get("a").expect(""));
            assert_eq!(Token::Num(100.0), calculate(&tokens9, &mut context).unwrap()); println!("----- 9 -----"); // Assignment substitutions
            assert_eq!(Token::Num(12.0), calculate(&tokens10, &mut context).unwrap()); println!("----- 10 -----"); // Order of operations
            assert_eq!(&vec![Num(3.0)], context.get("b").expect(""));
            assert_eq!(Token::Empty, calculate(&tokens11, &mut context).unwrap()); println!("----- 11 -----"); // Reassignment and empty calculation
            assert_eq!(&vec![Num(10.0)], context.get("a").expect(""));
        }

        #[test]
        fn test_parse_assignment() {
            let mut context: HashMap<String, Vec<Token>> = HashMap::new();
            
            let tokens1 = vec![Id(String::from("x")), Op(Eql), Num(2.0), Op(LnBr), Num(2.0)]; // Assign to `x`, trailing expression (`Num(2.0)`) should be returned

            let res = parse_assignment(tokens1, &mut context)
                .unwrap();

            assert_eq!(res, vec![Num(2.0), Op(LnBr)], "`parse_assignment` should return `[Num(2.0), Op(LnBr)]`");

            let x = context.get("x")
                .expect("`context[\"x\"]` should not be `None`");

            assert_eq!(x, &vec![Num(2.0)], "`context[\"x\"]` should be `vec![Num(2.0)]` not `{x:?}`");
        

            let tokens2 = vec![Id(String::from("y")), Op(Eql), Id(String::from("x"))];

            let res = parse_assignment(tokens2, &mut context)
                .unwrap();

            assert_eq!(res, vec![], "`parse_assignment` should return `[]`");

            let y = context.get("y")
                .expect("`context[\"y\"]` should not be `None`");

            assert_eq!(y, &vec![Id(String::from("x"))], "`context[\"y\"]` should be `[Id(\"x\")]` not `{y:?}`"); // `y` should be `x` (a "pointer" to a "pointer"), if `x` changes, then so will `y`

            // Multiple assignments
            let tokens3 = vec![
                Id(String::from("a")), Op(Eql), Num(10.0), Op(LnBr),
                Id(String::from("b")), Op(Eql), Id(String::from("a")), Op(LnBr),
                Id(String::from("c")), Op(Eql), Id(String::from("b"))
            ];

            let _ = parse_assignment(tokens3, &mut context);

            let a = context.get("a")
                .expect("`a` should not be `None`");
            let b = context.get("b")
                .expect("`b` should not be `None`");
            let c = context.get("c")
                .expect("`c` should not be `None`");

            assert_eq!(a, &vec![Num(10.0)]);
            assert_eq!(b, &vec![Id(String::from("a"))]);
            assert_eq!(c, &vec![Id(String::from("b"))]);
        }

        #[test]
        fn test_substitute_assignment() {
            let context = HashMap::from([
                (String::from("x"), vec![Num(2.0)]),
                (String::from("y"), vec![Num(4.0), Op(Add), Num(3.0)]),
                (String::from("z"), vec![Id(String::from("x"))])
            ]);

            let tokens1 = vec![Num(2.0), Op(Mul), Id(String::from("x"))];
            let tokens2 = vec![Id(String::from("z"))];
            let tokens3 = vec![Id(String::from("x")), Op(Eql), Num(100.0)];

            assert_eq!(vec![Num(2.0), Op(Mul), Op(LPar), Num(2.0), Op(RPar)], substitute_assignments(tokens1, &context).unwrap());
            assert_eq!(vec![Op(LPar), Op(LPar), Num(2.0), Op(RPar), Op(RPar)], substitute_assignments(tokens2, &context).unwrap());
            assert!(substitute_assignments(tokens3, &context).is_err(), "should return error if it contains an `Op(Eql)`")
        }

        #[test]
        fn test_convert_implicit_mul() {
            let tokens1 = vec![Num(4.0), Num(4.0)]; // `Num`
            let tokens2 = vec![Id(String::from("a")), Id(String::from("b"))]; // `Id`
            let tokens3 = vec![Num(4.0), Id(String::from("a"))]; // `Num` `Id`
            let tokens4 = vec![Fn(Sqrt(vec![Num(2.0)])), Fn(Sqrt(vec![Num(2.0)]))]; // `Fn`
            let tokens5 = vec![Fn(Sqrt(vec![Num(2.0)])), Num(2.0)]; // `Fn` `Num`
            let tokens6 = vec![Num(5.0), Empty, Num(5.0)]; // `Num` `Empty` `Num`

            assert_eq!(vec![Num(4.0), Op(Mul), Num(4.0)], convert_implicit_mul(tokens1).unwrap());
            assert_eq!(vec![Id(String::from("a")), Op(Mul), Id(String::from("b"))], convert_implicit_mul(tokens2).unwrap());
            assert_eq!(vec![Num(4.0), Op(Mul), Id(String::from("a"))], convert_implicit_mul(tokens3).unwrap());
            assert_eq!(vec![Fn(Sqrt(vec![Num(2.0)])), Op(Mul), Fn(Sqrt(vec![Num(2.0)]))], convert_implicit_mul(tokens4).unwrap());
            assert_eq!(vec![Fn(Sqrt(vec![Num(2.0)])), Op(Mul), Num(2.0)], convert_implicit_mul(tokens5).unwrap());
            assert_eq!(vec![Num(5.0), Op(Mul), Num(5.0)], convert_implicit_mul(tokens6).unwrap());
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
            let mut context: HashMap<String, Vec<Token>> = HashMap::new();
            

            let f01 = Sqrt(vec![Num(4.0), Op(Mul), Num(8.0)]);
            assert_eq!(Token::Num(32.0_f32.sqrt()), f01.calc(&mut context).unwrap());

            let f02 = Cbrt(vec![Num(27.0)]);
            assert_eq!(Token::Num(3.0), f02.calc(&mut context).unwrap());

            let f03 = Log(vec![Num(1000.0)]);
            assert_eq!(Token::Num(3.0), f03.calc(&mut context).unwrap());

            let f04 = Floor(vec![Num(3.15)]);
            assert_eq!(Token::Num(3.0), f04.calc(&mut context).unwrap());

            let f05 = Ceil(vec![Num(2.15)]);
            assert_eq!(Token::Num(3.0), f05.calc(&mut context).unwrap());

            let f06 = Abs(vec![Num(-10.0)]);
            assert_eq!(Token::Num(10.0), f06.calc(&mut context).unwrap());

            let f07 = Round(vec![Num(3.1)]);
            assert_eq!(Token::Num(3.0), f07.calc(&mut context).unwrap());

            let f08 = Sin(vec![Num(2.0)]);
            assert_eq!(Token::Num(2.0_f32.sin()), f08.calc(&mut context).unwrap());

            let f09 = Cos(vec![Num(2.0)]);
            assert_eq!(Token::Num(2.0_f32.cos()), f09.calc(&mut context).unwrap());

            let f10 = Tan(vec![Num(2.0)]);
            assert_eq!(Token::Num(2.0_f32.tan()), f10.calc(&mut context).unwrap());
        }
    
        #[test]
        fn test_simple_syntax_check() {
            let tokens = &vec![Num(2.0)]; // Base passing case
            assert!(simple_syntax_check(tokens).is_ok(), "single `Num`, should pass");

            let tokens = &vec![Num(2.0), Op(RPar)]; // Unexpected `RPar`
            assert!(simple_syntax_check(tokens).is_err(), "unexpected `RPar`, should err");

            let tokens = &vec![Num(2.0), Op(LPar)]; // Unclosed `LPar`
            assert!(simple_syntax_check(tokens).is_err(), "unclosed `LPar`, should err");

            let tokens = &vec![Num(2.0), Op(RPar), Op(LPar)]; // Correct number of parens, wrong placement
            assert!(simple_syntax_check(tokens).is_err(), "correct number of parens, wrong placement, should err");

            let tokens = &vec![Op(LPar), Op(RPar)]; // Empty parens
            assert!(simple_syntax_check(tokens).is_err(), "empty parens, should err");

            let tokens = &vec![Num(2.0), Op(Mul), Op(Mul), Num(2.0)]; // 2 `Op` in a row
            assert!(simple_syntax_check(tokens).is_err(), "2 `Op` in a row, should err");

            let tokens = &vec![Num(2.0), Op(Mul), Op(LPar), Num(2.0), Op(RPar)]; // Good case of `LPar` 2 `Op` in a row
            assert!(simple_syntax_check(tokens).is_ok(), "2 `Op` in a row, `Op` `LPar`, should pass");

            let tokens = &vec![Op(LPar), Num(2.0), Op(RPar), Op(Mul), Num(2.0)]; // Good case of `RPar` 2 `Op` in a row
            assert!(simple_syntax_check(tokens).is_ok(), "2 `Op` in a row,`RPar` `Op`, should pass");

            let tokens = &vec![Op(LPar), Num(2.0), Op(Mul), Op(RPar)]; // Bad case of `RPar` 2 `Op` in a row
            assert!(simple_syntax_check(tokens).is_err(), "2 `Op` in a row, `Op` `RPar`, should err");

            let tokens = &vec![Op(LPar), Op(Mul), Num(2.0), Op(RPar)]; // Bad case of `LPar` 2 `Op` in a row
            assert!(simple_syntax_check(tokens).is_err(), "2 `Op` in a row, `LPar` `Op`, should err");

            let tokens = &vec![Op(Mul), Num(2.0)]; // Left dangling `Op`
            assert!(simple_syntax_check(tokens).is_err(), "left dangling `Op`, should err");

            let tokens = &vec![Num(2.0), Op(Mul)]; // Right dangling 'Op'
            assert!(simple_syntax_check(tokens).is_err(), "right dangling `Op`, should err");

            let tokens = &vec![Num(2.0), Op(LnBr)]; // Right dangling 'LnBr'
            assert!(simple_syntax_check(tokens).is_ok(), "right dangling `LnBr`, should pass");

            let tokens = &vec![Op(LPar), Num(2.0), Op(RPar)]; // Left dangling 'LPar', right dangling `RPar`
            assert!(simple_syntax_check(tokens).is_ok(), "left dangling `LPar`, right dangling `RPar`, should pass");

            let tokens = &vec![Num(2.0), Op(LPar)]; // Right dangling 'LPar'
            assert!(simple_syntax_check(tokens).is_err(), "right dangling `LPar`, should err");

            let tokens = &vec![Op(RPar), Num(2.0)]; // Left dangling 'RPar'
            assert!(simple_syntax_check(tokens).is_err(), "left dangling `RPar`, should err");

            let tokens = &vec![Op(LPar), Op(LPar), Op(LPar), Num(2.0), Op(RPar), Op(RPar), Op(RPar), ]; // Multiple parens in a row
            assert!(simple_syntax_check(tokens).is_ok(), "multiple parens in a row, should pass");
        }
    }
}

#[doc = include_str!("../README.md")]
#[cfg(doctest)]
pub struct ReadmeDocTests;
