use std::fmt;

use crate::cli::help::print_help;
use crate::error::{Error, Result};
use crate::language::glossary::Rule;
use crate::language::parse::parse_rule;

/// Evaluate the given arguments according to the Command
pub type Evaluator = fn(&str) -> ();

pub type StaticSlice = &'static [&'static str];

/// Front-end operations that perform certain actions on the given input
/// Can be invoked using the CLI args or the REPL as either a control mode or input
#[derive(Debug, Clone, Copy)]
pub struct Command {
    pub name: &'static str,
    pub description: &'static str,
    pub evaluator: Evaluator,
    // Optional fields
    usage: Option<&'static str>,
    examples: StaticSlice,
    aliases: StaticSlice,
    hidden: bool, // Show command in general help
}

impl Command {
    const fn new(name: &'static str, description: &'static str, evaluator: Evaluator) -> Self {
        Command {
            name,
            description,
            evaluator,

            usage: None,
            examples: &[],
            aliases: &[],
            hidden: false,
        }
    }

    pub fn matches(self, name: &str) -> bool {
        self.name == name || self.aliases.contains(&name)
    }

    const fn aliases(mut self, aliases: StaticSlice) -> Self {
        self.aliases = aliases;
        self
    }

    const fn usage(mut self, usage: &'static str) -> Self {
        self.usage = Some(usage);
        self
    }

    const fn examples(mut self, examples: StaticSlice) -> Self {
        self.examples = examples;
        self
    }

    const fn hidden(mut self) -> Self {
        self.hidden = true;
        self
    }
}

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;

        if !self.aliases.is_empty() {
            write!(f, "[{}]", self.aliases.join(", "))?;
        }
        writeln!(f, " - {}", self.description)?;

        if self.usage.is_some() {
            writeln!(f, "Usage: {}", self.usage.unwrap())?;
        }

        if !self.examples.is_empty() {
            writeln!(f, "Examples:")?;
            for example in self.examples {
                writeln!(f, "   $ {}", example)?;
            }
        }
        Ok(())
    }
}

pub static COMMAND_MAP: &[Command] = &[
    Command::new("help", "Print this help message", print_help),
    Command::new(
        "ast",
        "Pretty print the abstract syntax tree of a given rule",
        pretty_print_ast,
    ),
    Command::new(
        "astdbg",
        "Debug print the abstract syntax tree fully",
        debug_print_ast,
    ),
    Command::new("load", "Print the abstract syntax tree", print_help),
    Command::new("set", "Print the abstract syntax tree", print_help)
        .examples(&["set max_iterations 1000", "set subsonsequents true"]),
    Command::new("query", "Print the abstract syntax tree", print_help).aliases(&["?"]),
    Command::new("trace", "Print the abstract syntax tree", print_help).aliases(&["debug"]),
    Command::new("bench", "Print the abstract syntax tree", print_help),
    // ("enable", "feature", print_ast),
];

pub fn get_command_evaluator(name: &str) -> Option<Evaluator> {
    COMMAND_MAP
        .iter()
        .find(|cmd| cmd.matches(name))
        .map(|cmd| cmd.evaluator)
}

fn set_config(str: &str) -> () {
    ()
}

fn debug_print_ast(str: &str) -> () {
    let rule = parse_rule(str);
    println!("{:?}", rule);
}

fn pretty_print_ast(str: &str) -> () {
    println!("{str}");
    let rule = parse_rule(str);
    rule.inspect(|v| println!("{}", v.clone().either_into::<Rule>()))
        .inspect_err(|e| println!("{}", e));
}
