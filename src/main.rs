use std::io;
use std::collections::HashMap;
use log::{error, warn, info, debug, trace};

use m_calc::{calculate, tokenize, init_logger, CalcErr};

fn main() {
    init_logger("RUST_LOG", "");
    let mut buf = String::new();
    let mut context = HashMap::new();

    loop {
        println!("type `/q` to quit");
        io::stdin().read_line(&mut buf).unwrap();

        buf = buf.trim().to_string();

        if buf == "/q" {
            break;
        }

        let tokens = match tokenize(&buf) {
            Ok(t) => {
                info!("successful tokenization: `{t:?}`");
                t
            },
            Err(CalcErr(e)) => {
                error!("{e:?}");
                buf.clear();
                continue;
            },
        };

        let res = match calculate(&tokens, &mut context) {
            Ok(t) => {
                info!("successful calculation: `{t}`");
                t
            },
            Err(CalcErr(e)) => {
                error!("{e:?}");
                buf.clear();
                continue;
            },
        };

        println!("\n--- Calculation ---\nResult:  {:?}\nContext: {:?}", res, context);

        buf.clear();
    }
}
