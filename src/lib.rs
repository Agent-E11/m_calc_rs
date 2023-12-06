// TODO: Refactor code to use modules
// TODO: Ensure test coverage after refactor
// TODO: Allow cases like `1+(-1)` in `calculate`
// TODO: Create doc tests for all pub functions

pub use calc::calculate;
pub use calc::display_expr;
pub use calc::{Token, Oper, Func};
pub use calc::CalcErr;

pub mod logging;
pub mod parse;

pub mod calc {
    use std::num::ParseFloatError;
    use std::collections::HashMap;
    use std::convert::TryFrom;
    use log::debug;

    use crate::parse::{
        parse_functions,
        parse_assignment,
        convert_implicit_mul,
    };

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

    /// Generate a `Token` from a given `&Vec<Token>`
    // TODO: Create doc tests
    pub fn simple_calc(tokens: &Vec<Token>) -> Result<Token, CalcErr> {
        if tokens.is_empty() { return Err(CalcErr::from("length cannot be `0`")); }
        debug!("starting `simple_calc` with tokens: `{tokens:?}`");

        let token_values = extract_token_values(tokens)?;
        debug!("values extracted from tokens: {token_values:?}");

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

        debug!("finished `simple_calc` with value: `{res:?}`");
        res
    }

    /// Takes a `Vec<Token>` and calculates all occurrences of a `&str` operator
    fn calculate_operator(tokens: Vec<Token>, operators: &[Oper]) -> Result<Vec<Token>, CalcErr> {
        if tokens.is_empty() { return Err(CalcErr::from("length cannot be `0`")); }
        debug!("starting `calculate_operator` with tokens: `{:?}`, and operators: `{:?}`", tokens, operators);

        let mut tokens = tokens;
        let mut i = 0;
        loop {
            if tokens.get(i).is_none() {
                debug!("reached end of tokens at `{}`", i);
                break;
            }
            if let Token::Op(o) = &tokens[i] {
                debug!("operator `{o}` at `{i}`");
                if operators.contains(o) {
                    debug!("operator matches");
                    debug!("calculating: {:?}", &tokens[i-1..i+2]);
                    let res = simple_calc(&Vec::from(&tokens[i-1..i+2]))?;
                    debug!("result: {res:?}");
                    debug!("deleting: {:?}", &tokens[i-1..i+2]);

                    tokens.drain(i-1..i+2);
                    tokens.insert(i-1, res);

                    i = 0;
                    continue;
                }
                debug!("operator doesn't match, continuing");
            }
            i += 1;
        }
        debug!("finished `calculate_operator` with tokens: `{:?}`", tokens);
        Ok(tokens)
    }

    /// Takes a `&Vec<Token>` and computes the mathematical expression and returns the resulting number wrapped in a `Token::Num()`
    // TODO: Create doc tests
    pub fn calculate(tokens: &Vec<Token>, context: &mut HashMap<String, Vec<Token>>) -> Result<Token, CalcErr> {
        if tokens.is_empty() { return Err(CalcErr::from("length cannot be `0`")); }
        let mut tokens = tokens.clone();

        debug!("starting `calculate` with tokens `{:?}` and context `{:?}`", tokens, context);

        // Delete trailing `Op(LnBr)`
        if tokens.ends_with(&[Token::Op(Oper::LnBr)]) {
            tokens.pop();
            debug!("deleted trailing `Op(LnBr)`");
        }
        
        tokens = parse_assignment(tokens, context)?;
        
        // Make substitutions
        tokens = substitute_assignments(tokens, context)?;
        
        if tokens.is_empty() {
            debug!("empty tokens, returning");
            return Ok(Token::Empty);
        }

        // Pass 1: Parse the functions, calculate each function
        tokens = parse_functions(tokens)?;
        debug!("calculating each function");
        tokens = tokens.into_iter().map(|t| {
            if let Token::Fn(f) = t {
                debug!("found function: `{f:?}`, calculating");
                f.calc(context)
            } else {
                Ok(t)
            }
        }).collect::<Result<Vec<Token>, CalcErr>>()?;
        debug!("finished calculating functions with tokens: `{:?}`", tokens);

        // Pass 2: Calculate and substitute parentheses
        debug!("started parsing and calculating parentheses");
        let mut i = 0;
        loop {

            // Check if index is out of bounds, get token
            let token = match tokens.get(i) {
                None => {
                    break;
                },
                Some(t) => t.clone(),
            };

            debug!("token at index `{}`: {:?}", i, token);

            // Find an open parentheses
            if token == Token::Op(Oper::LPar) {
                debug!("open parentheses at `{i}`");
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

                debug!("closing parentheses at {j}");

                let subsection = &Vec::from(&tokens[i+1..j]);
                debug!("calculating subsection: `{subsection:?}`");

                // Calculate subsection
                let res = calculate(subsection, context)?;

                debug!("range to be deleted: {:?}", i..j+1);


                // Replace subsection with calculation
                tokens.drain(i..j+1);
                tokens.insert(i, res);

                // Go back to beginning of `tokens` and keep searching
                i = 0;
                continue;
            }

            i += 1;
        }
        debug!("finished calculating parentheses with tokens: `{:?}`", tokens);

        tokens = convert_implicit_mul(tokens)?;

        // Check syntax, return `Err` if any
        simple_syntax_check(&tokens)?;

        // Pass 3: operators
        debug!("parsing operators");

        debug!("tokens before pass: {tokens:?}");
        tokens = calculate_operator(tokens, &[Oper::Exp])?;
        debug!("after `^`: {tokens:?}");
        tokens = calculate_operator(tokens, &[Oper::Mul, Oper::Div, Oper::Mod])?;
        debug!("after `*, /, %`: {tokens:?}");
        tokens = calculate_operator(tokens, &[Oper::Add, Oper::Sub])?;
        debug!("after `+, -`: {tokens:?}");


        let res = tokens[0].clone();
        debug!("finished `calculate` with result: `{}`", res);
        Ok(res)
    } 

    /// Takes a `Vec<Token>` and replaces all `Id`s with their values defined in the given `context`
    // FIXME: If a variable is assigned to itself (e.g. `a = a`), it will break the substitution
    // TODO: Add logs
    fn substitute_assignments(tokens: Vec<Token>, context: &HashMap<String, Vec<Token>>) -> Result<Vec<Token>, CalcErr> {
        let mut tokens = tokens;

        if tokens.contains(&Token::Op(Oper::Eql)) {
            return Err(CalcErr::from("cannot substitute assignments for an expression containing `Op(Eql)`"));
        }
        debug!("started `substitute_assignments` with tokens `{:?}` and context `{:?}`", tokens, context);

        let mut i = 0;
        // Loop through all tokens
        loop {
            debug!("token: `{:?}`, at index: `{:?}`", tokens.get(i), i);
            match tokens.get(i) {
                None => break,
                Some(t) => if let Token::Id(s) = t {
                    // If the token is an `Id`, get its associated value in `context`
                    let value = match context.get(s) {
                        None => {
                            debug!("variable `{}` is not assigned, continuing", s);
                            i += 1;
                            continue;
                        },
                        Some(v) => {debug!("value assigned to `{}` is: `{:?}`", s, v); v},
                    };

                    // Enclose the value in parentheses
                    let mut enclosed = vec![Token::Op(Oper::LPar)];
                    enclosed.extend(value.clone());
                    enclosed.push(Token::Op(Oper::RPar));

                    // Substitute the `Id` for the `Id`s value
                    tokens.splice(i..i+1, enclosed.clone());
                    debug!("substituted:\n`{tokens:?}`");

                    i = 0;
                    continue;
                }
            }

            i += 1;
        };

        Ok(tokens)
    }

    /// Generates a `(f32, Oper, f32)` containing the same values as the given `&Vec<Token>`
    // TODO: Add logs
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

    /// Does some simple syntax checks on a `Vec<Token>` and returns an error if there are any mistakes
    /// 
    // TODO: Add logs
    fn simple_syntax_check(tokens: &[Token]) -> Result<(), CalcErr> {
        if tokens.is_empty() { return Err(CalcErr::from("length cannot be `0`")); }

        debug!("starting syntax check on tokens: `{:?}`", tokens);

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
        
        debug!("checking tokens `{tokens:?}` for multiple operators in a row");
        // Check for 2 `Op` in a row
        for (i, token) in tokens.iter().enumerate() {
            debug!("`{token}` at `{i}`");
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
    impl TryFrom<&str> for Oper {
        type Error = CalcErr;

        fn try_from(value: &str) -> Result<Self, Self::Error> {
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
        // TODO: Add logs
        pub fn calc(&self, context: &mut HashMap<String, Vec<Token>>) -> Result<Token, CalcErr> {
            let error = Err(CalcErr::from("`calculate` did not return a `Token::Num`")); // TODO: This might break things in the future when `calculate` can return a `Vac<Token>`
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

            assert_eq!(Token::Num(8.0), calculate(&tokens1, &mut context).unwrap()); // Order of operations
            assert_eq!(Token::Num(3_843_199.8), calculate(&tokens2, &mut context).unwrap()); // Very long and complicated
            assert_eq!(Token::Num(12.0), calculate(&tokens3, &mut context).unwrap()); // Parentheses
            assert_eq!(Token::Num(2.0), calculate(&tokens4, &mut context).unwrap()); // Unnecessary parentheses
            assert_eq!(Token::Num(6.0), calculate(&tokens5, &mut context).unwrap()); // Implicit multiplication
            assert_eq!(Token::Num(4.0), calculate(&tokens6, &mut context).unwrap()); // Functions
            assert_eq!(Token::Num(3.0), calculate(&tokens7, &mut context).unwrap()); // Trailing `Op(LnBr)`
            assert_eq!(Token::Num(3.0), calculate(&tokens8, &mut context).unwrap()); // Assign and calculate
            assert_eq!(&vec![Num(20.0)], context.get("a").expect(""));
            assert_eq!(Token::Num(100.0), calculate(&tokens9, &mut context).unwrap()); // Assignment substitutions
            assert_eq!(Token::Num(12.0), calculate(&tokens10, &mut context).unwrap()); // Order of operations
            assert_eq!(&vec![Num(3.0)], context.get("b").expect(""));
            assert_eq!(Token::Empty, calculate(&tokens11, &mut context).unwrap()); // Reassignment and empty calculation
            assert_eq!(&vec![Num(10.0)], context.get("a").expect(""));
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
