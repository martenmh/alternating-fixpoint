use std::env;

use crate::cli::{repl, run};

mod cli;
mod error;
mod language;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        return run::run(args);
    } else {
        repl::repl();
    }
}
