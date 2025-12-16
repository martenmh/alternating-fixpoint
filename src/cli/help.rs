use crate::cli::command::COMMAND_MAP;
use crate::error::Error;

fn print_command_help(command_name: &str) -> () {
    //let glossary_descriptions = HashMap<&str, &str>::new();
    let command = COMMAND_MAP.iter().find(|cmd| cmd.matches(command_name));

    match command {
        Some(cmd) => println!("{}", cmd),
        None => println!("{}", Error::UnknownCommand(command_name.to_owned())),
    }
}

pub fn print_help(command: &str) -> () {
    if !command.trim().is_empty() {
        return print_command_help(command);
    }
    println!("Type \"help [command]\" for help on any keyword, command");

    ()
}
