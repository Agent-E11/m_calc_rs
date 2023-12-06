use crate::CalcErr;
use crate::{Token, Oper};
use crate::logging::debug;

/// Does some simple syntax checks on a `Vec<Token>` and returns an error if there are any mistakes
/// 
// TODO: Add logs
pub(crate) fn simple_syntax_check(tokens: &[Token]) -> Result<(), CalcErr> {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Token::*, Oper::*};

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
 