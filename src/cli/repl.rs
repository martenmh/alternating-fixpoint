use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;

use crate::error::{Error, Result};

use crate::cli::command::{COMMAND_MAP, Evaluator};
use crate::cli::run::run;

fn header_msg() {
    println!("Alternating fixpoint implementation.");
    println!("Type \"help\" for more information, or \"exit\" to quit");
}

/// Start an interactive REPL for a certain command,
/// where every line will be interpreted as that command
fn interactive_command_repl(command_name: &str) -> () {
    let command = COMMAND_MAP.iter().find(|cmd| cmd.name == command_name);
    match command {
        // Start a nested REPL that runs every line through the command evaluator
        Some(cmd) => {
            if let Err(error) = start_repl(cmd.name, cmd.evaluator) {
                println!("{error:?}");
            }
        }
        None => println!("{}", Error::UnknownCommand(command_name.to_owned())),
    }
}

fn evaluate_command(line: &str) -> () {
    let split_line: Vec<&str> = line.trim().split_whitespace().collect();
    if !line.starts_with(':') {
        run(split_line.iter().map(|x| x.to_string()).collect());
        return ();
    }

    interactive_command_repl(line.strip_prefix(':').unwrap())
}

pub fn repl() {
    header_msg();
    if let Err(error) = start_repl("", evaluate_command) {
        println!("{error:?}");
    }
}

/// Run the Read Eval Print Loop
///
/// Lines starting with ':' will be interpreted as control commands,
/// see the COMMAND_MAP for all possible commands.
pub fn start_repl(mode: &str, eval: Evaluator) -> Result<()> {
    let mut rl = DefaultEditor::new()?;
    //.map_err(|e| -> Error { e.into() })?;
    loop {
        let readline = rl.readline(format!("{mode}>> ").as_str());
        match readline {
            Ok(line) => {
                if line.trim() == "exit" {
                    break;
                }

                if line.trim().is_empty() {
                    continue;
                }

                rl.add_history_entry(line.as_str())?;
                let stripped_line = line.trim().strip_prefix(mode).unwrap_or(&line);
                eval(stripped_line);
            }

            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    Ok(())
}
