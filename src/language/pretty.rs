use core::fmt;

use itertools::Itertools;

use crate::language::glossary::*;

pub fn num_to_subscript(num: usize) -> &'static str {
    let subscripts = vec!["₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉"];
    match num {
        0..9 => subscripts[num],
        _ => "",
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Variable(v) => write!(f, "{}", v),
            Term::Constant(c) => write!(f, "{}", c),
            Term::Compound(name, terms) => write!(f, "{}({})", name, terms.iter().format(", ")),
        }
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.terms.len() > 0 {
            write!(f, "{}({})", self.predicate, self.terms.iter().format(", "))
        } else {
            write!(f, "{}", self.predicate)
        }
    }
}
impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Positive(atom) => write!(f, "{}", atom),
            Literal::Negative(atom) => write!(f, "not {}", atom), // ¬
        }
    }
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.head)?;
        if !self.is_fact() {
            write!(f, " :- {}", self.body.iter().format(", "))?;
        }
        write!(f, ".")
    }
}

impl Rule {
    pub fn pretty(&self) -> String {
        use std::fmt::Write;
        let mut output = String::new();
        self.pretty_write(&mut output).unwrap_or_default();
        output
    }
    // Depth-first visitation & printing
    /// pretty print a tree
    fn pretty_write(&self, f: &mut dyn fmt::Write) -> fmt::Result {
        const CONNECTOR: &str = " │ "; // prefix: pipe
        const ENTRY: &str = " ├─"; // connector: tee
        const PADDING: &str = "   "; // prefix: no more siblings
        const LAST_ENTRY: &str = " └─"; // connector: elbow

        fn get_connective_prefix(last: bool) -> &'static str {
            if last { PADDING } else { CONNECTOR }
        }
        fn get_entry_prefix(last: bool) -> &'static str {
            if last { LAST_ENTRY } else { ENTRY }
        }

        fn visit_literal(
            f: &mut dyn fmt::Write,
            literal: &Literal,
            prefix: String,
            last: bool,
        ) -> fmt::Result {
            let terms_prefix = prefix.clone() + get_connective_prefix(last);

            match literal {
                Literal::Positive(atom) => {
                    writeln!(
                        f,
                        "{}{}[Positive] {}",
                        prefix,
                        get_entry_prefix(last),
                        atom.predicate
                    )?;
                    visit_terms(f, atom.terms.clone(), terms_prefix, true)
                }
                Literal::Negative(atom) => {
                    writeln!(
                        f,
                        "{}{}[Negative] {}",
                        prefix,
                        get_entry_prefix(last),
                        atom.predicate
                    )?;
                    visit_terms(f, atom.terms.clone(), terms_prefix, true)
                }
            }
        }

        fn visit_atom(
            f: &mut dyn fmt::Write,
            atom: &Atom,
            prefix: String,
            last: bool,
        ) -> fmt::Result {
            writeln!(
                f,
                "{}{}[Atom] {}",
                prefix,
                get_entry_prefix(last),
                atom.predicate
            )?;
            let terms_prefix = prefix.clone() + get_connective_prefix(last);
            visit_terms(f, atom.terms.clone(), terms_prefix, true)
        }

        fn visit_literals(
            f: &mut dyn fmt::Write,
            literals: Vec<Literal>,
            prefix: String,
        ) -> fmt::Result {
            for (i, literal) in literals.iter().enumerate() {
                let last_literal = (i + 1) >= literals.len();
                visit_literal(f, literal, prefix.clone(), last_literal)?;
                if !last_literal {
                    writeln!(f, "{}{}", prefix, get_connective_prefix(false))?;
                }
            }
            Ok(())
        }
        fn visit_terms(
            f: &mut dyn fmt::Write,
            terms: Vec<Term>,
            prefix: String,
            newline_terms: bool,
        ) -> fmt::Result {
            for (i, term) in terms.iter().enumerate() {
                let last_term = (i + 1) >= terms.len();
                visit_term(f, term, prefix.clone(), last_term)?;
                if newline_terms && !last_term && matches!(term, Term::Compound(_, _)) {
                    writeln!(f, "{}{}", prefix, get_connective_prefix(false))?;
                }
            }
            Ok(())
        }

        fn visit_term(
            f: &mut dyn fmt::Write,
            term: &Term,
            prefix: String,
            last: bool,
        ) -> fmt::Result {
            match term {
                Term::Variable(str) => {
                    writeln!(f, "{}{}[Variable] {}", prefix, get_entry_prefix(last), str)
                }
                Term::Constant(str) => {
                    writeln!(f, "{}{}[Constant] {}", prefix, get_entry_prefix(last), str)
                }
                Term::Compound(str, terms) => {
                    writeln!(f, "{}{}[Compound] {}", prefix, get_entry_prefix(last), str)?;
                    let terms_prefix = prefix.clone() + get_connective_prefix(last);
                    visit_terms(f, terms.clone(), terms_prefix, false)
                }
            }
        }

        if self.is_fact() {
            writeln!(f, "[Fact] :-")?;
        } else {
            writeln!(f, "[Rule] :-")?;
        }
        let is_last_entry = self.is_fact();
        visit_atom(f, &self.head, String::from(""), is_last_entry)?;

        // only print out the head for facts
        if self.is_fact() {
            return Ok(());
        }

        writeln!(f, "{}", get_connective_prefix(false))?;
        writeln!(f, "{}[Terms]", get_entry_prefix(true))?;
        visit_literals(f, self.body.clone(), PADDING.to_owned())?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::language::{glossary::*, parse::parse_rule};

    #[test]
    fn test_rule_display() {
        let input = "fly(X) :- bird(ancestor(X, larry), Z), not -fly(X).";
        let parsed_rule = parse_rule(input).unwrap().unwrap_left();
        let output = format!("{}", parsed_rule);

        assert_eq!(input, output);
    }

    #[test]
    fn test_rule_pretty_print() {
        // fly(X) :- bird(ancestor(X, larry), Z), not -fly(X).
        let rule = Rule::from(
            Atom::from("fly", vec![Term::variable("X")]),
            vec![
                Literal::positive(
                    "bird",
                    vec![
                        Term::compound(
                            "ancestor",
                            vec![Term::variable("X"), Term::constant("larry")],
                        ),
                        Term::variable("Z"),
                    ],
                ),
                Literal::negative("-fly", vec![Term::variable("X")]),
            ],
        );
        let display = r#"[Rule] :-
 ├─[Atom] fly
 │  └─[Variable] X
 │.
 └─[Terms]
    ├─[Positive] bird
    │  ├─[Compound] ancestor
    │  │  ├─[Variable] X
    │  │  └─[Constant] larry
    │  │.
    │  └─[Variable] Z
    │.
    └─[Negative] -fly
       └─[Variable] X
    "#
        .replace('.', " "); // keep in trailing spaces
        assert_eq!(rule.pretty().trim(), display.trim());
    }
}
