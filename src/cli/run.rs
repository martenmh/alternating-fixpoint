use std::collections::HashMap;

use crate::cli::command::COMMAND_MAP;

// No dealing with commands anymore
pub fn run_line(line: &str) {}

/// run_command?
///
pub fn run(args: Vec<String>) {
    // Attempt command interpretation
    let arg = &args[0];
    println!("{arg}");
    match COMMAND_MAP.iter().find(|cmd| cmd.name == arg) {
        Some(cmd) => (cmd.evaluator)(args[1..].join(" ").as_str()),
        None => run_line(args.join(" ").as_str()),
    }
}
