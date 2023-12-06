use std::cmp::Ordering;
use std::collections::HashMap;

use crate::logging::{error, debug};
use crate::{Token, Oper, Func, calculate};
use crate::CalcErr;

/// Constructs a `Vec<Token>` from a given `&str` representation of a mathematical expression
/// 
/// # Examples
/// 
/// ```
/// use m_calc::{Token, Func, Oper};
/// use m_calc::parse::tokenize;
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

/// Takes a `Vec<Token>` and converts all occurrences of division and subtraction to their equivalents in multiplication and addition
// TODO: Create doc tests
pub fn convert_div_sub(tokens: Vec<Token>) -> Result<Vec<Token>, CalcErr> {
    if tokens.is_empty() { return Err(CalcErr::from("length cannot be `0`")); }
    let mut tokens = tokens;

    debug!("starting `convert_div_sub` with tokens: `{tokens:?}`");
    
    let mut i = 0;
    loop {
        let token = match tokens.get(i) {
            None => break,
            Some(t) => t,
        };

        if let Token::Op(o) = token {
            match o {
                Oper::Div => {
                    debug!("found division at index: `{i}`");
                    match tokens[i+1].clone() {
                        Token::Num(n) => {
                            debug!("changing `{}` at `{}` to `Op(Mul)`", tokens[i], i);
                            debug!("inverting `{}` at `{}` to `{}`", n, i+1, 1.0/n);
                            tokens[i] = Token::Op(Oper::Mul);
                            tokens[i+1] = Token::Num(1.0/n);
                        },
                        _ => {
                            debug!("token at index `{}` (after `/`) is not a `Token::Num`", i+1);
                            return Err(CalcErr(format!("token at index `{}` (after `/`) is not a `Token::Num`", i+1)));
                        },
                    }
                },
                Oper::Sub => {
                    debug!("found subtraction at index: `{i}`");
                    match tokens[i+1].clone() {
                        Token::Num(n) => {
                            debug!("changing `{}` at `{}` to `Op(Add)`", tokens[i], i);
                            debug!("inverting `{}` at `{}` to `{}`", n, i+1, Token::Num(-n));
                            tokens[i] = Token::Op(Oper::Add);
                            tokens[i+1] = Token::Num(-n);
                        },
                        _ => {
                            debug!("token at index `{}` (after `-`) is not a `Token::Num`", i+1);
                            return Err(CalcErr(format!("token at index `{}` (after `-`) is not a `Token::Num`", i+1)))
                        },
                    }
                },
                _ => (),
            }
        }

        i += 1;
    }

    debug!("finished `convert_div_sub` with tokens: `{:?}`", tokens);
    Ok(tokens)
}

/// Takes a `Vec<Token>` and replaces all occurrences of `Op(FnStart), Id(), Op(LPar), ... , Op(RPar)` with `Fn(Func(...))`
pub fn parse_functions(mut tokens: Vec<Token>) -> Result<Vec<Token>, CalcErr> {
    let mut i = 0;
    debug!("starting `parse_functions` with tokens: `{tokens:?}`");

    loop {
        // Get current token, break if end of `Vec`
        match tokens.get(i) {
            None => break,
            Some(token) => {
                debug!("token `{token}` at index `{i}`");
                // Check to see if the current token is a function start operator, if it isn't, do nothing
                if token == &Token::Op(Oper::FnStart) {
                    debug!("found function start at `{i}`, starting parse");
                    // `id_str` is the string contained in the next token. If the next token is not a `Token::Id` return an error
                    let id_str = match tokens.get(i+1).cloned() {
                        None => { 
                            error!("unexpected end of tokens after function start");
                            return Err(CalcErr::from("unexpected end of `Vec`"));
                        },
                        Some(t) => if let Token::Id(id) = t {
                            debug!("function's id is `{id}`");
                            id
                        } else {
                            return Err(CalcErr(format!("`{}` is an invalid function id", tokens.get(i+1).unwrap())));
                        }
                    };
                    // `func_tokens` is the `Vec<Token>` to be passed into the `Func`
                    debug!("getting tokens to be passed to the function");
                    let func_tokens: Vec<Token> = if let Some(t) = tokens.get(i+2) {
                        if t == &Token::Op(Oper::LPar) {
                            let start = i + 3;
                            let mut end = i + 4;
                            let mut open_parens = 1;
                            // Loop through elements inside the parentheses
                            loop {
                                match tokens.get(end) {
                                    None => { 
                                        error!("expected closing parentheses before end of tokens");
                                        return Err(CalcErr::from("unexpected end of `Vec`"));
                                    },
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
                                    debug!("all parentheses have been closed, deleting index `{}` to `{}`", start-1, end+1);
                                    break tokens.drain(start-1..end+1).collect();
                                }
                                end += 1;
                            }
                        } else {
                            // If the next token is not an open parentheses, return an error
                            return Err(CalcErr::from("missing open parentheses after function id"));
                        }
                    } else {
                        return Err(CalcErr::from("unexpected end of `Vec`"));
                    };

                    debug!("tokens passed into the function are `{func_tokens:?}`");
                    // Get the index of the first `Op(FnStart)` (this is the one currently being parsed)
                    let func_index = tokens.iter().position(|t| t == &Token::Op(Oper::FnStart)).unwrap();
                    // Delete the `Op(FnStart)` and `Id()`
                    debug!("deleting index `{}` to `{}`", func_index, func_index+2);
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
    debug!("finished `parse_functions` with tokens: `{tokens:?}`");
    Ok(tokens)
}

/// Takes a `Vec<Token>` and a `Hashmap<String, Vec<Token>>`.
/// Takes all the assignment expressions in the `Vec<Token>`,
/// deletes them and converts it into a key value pair in the `Hashmap<String, Vec<Token>>`
pub(crate) fn parse_assignment(tokens: Vec<Token>, context: &mut HashMap<String, Vec<Token>>) -> Result<Vec<Token>, CalcErr> {
    let exprs = tokens.split(|t| t == &Token::Op(Oper::LnBr));
    let mut new_tokens: Vec<Token> = Vec::new();

    debug!("starting `parse_assignments` with tokens `{:?}` and context `{:?}`", tokens, context);
    debug!("the tokens are broken into `{:?}` expressions: `{:?}`", exprs.clone().count(), exprs);

    for expr in exprs {
        debug!("expression: `{expr:?}`");
        // Validate syntax
        // Check if empty
        if expr.is_empty() { continue; }

        match expr.iter().filter(|&t| *t == Token::Op(Oper::Eql)).count().cmp(&1) {
            // If there is more than 1 `Op(Eql)`, return an error
            Ordering::Greater => {
                debug!("multiple `Op(Eql)` in expression: `{:?}`", expr);
                return Err(CalcErr(format!("multiple assignments in expression: `{:?}`", expr)));
            },
            // If it is not an assignment, add the expression to `new_tokens` (it will be calculated later)
            Ordering::Less => {
                debug!("no assignments in expression: `{:?}`, appending to tokens", expr);
                new_tokens.append(&mut expr.to_vec());
                new_tokens.push(Token::Op(Oper::LnBr))
            },
            // If there is 1 `Op(Eql)`, parse the assignment expression
            Ordering::Equal => {
                debug!("1 assignment in expression: `{:?}`, parsing", expr);
                let id = if let Token::Id(i) = &expr[0] {
                    debug!("the id is `{i}`");
                    i
                } else {
                    return Err(CalcErr::from("first token of assignment expression must be an `Id`"));
                };
                if expr[1] != Token::Op(Oper::Eql) {
                    debug!("second token must be `Op(Eql)` not `{:?}`", expr[1]);
                    return Err(CalcErr(format!("second token of assignment expression must be `=`, not `{}`", expr[1])));
                }


                let value = &expr[2..expr.len()];
                debug!("value is `{:?}`", value);
                let insert_value = if value.iter().any(|t| matches!(t, Token::Id(_))) {
                    debug!("contains an `Id`, not calculating");
                    expr[2..expr.len()].to_vec()
                } else {
                    debug!("does not contain an `Id`, calculating");
                    [calculate(&expr[2..expr.len()].to_vec(), context)?].to_vec()
                };

                debug!("the value assigned to `{id}` is `{insert_value:?}`");

                // Add the calculated value to the `context`
                context.insert(
                    id.to_string(), 
                    insert_value,
                );
                debug!("context after assignment: `{context:?}`");
            }
        }
    }

    debug!("finished `parse_assignment` with tokens: `{new_tokens:?}`, and context: `{context:?}`");
    Ok(new_tokens)
}

// TODO: Add logs
pub(crate) fn convert_implicit_mul(tokens: Vec<Token>) -> Result<Vec<Token>, CalcErr> {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Token::*, Oper::*, Func::*};

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
    fn test_convert_div_sub() {
        // TODO:
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
}
