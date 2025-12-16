use std::{
    collections::{HashMap, HashSet},
    result,
};

use itertools::Itertools;

use crate::language::glossary::*;

/// Current view of
struct Interpretation {
    positives: Vec<Atom>,
    negatives: Vec<Atom>,
}

struct Denotation {}

/// Things we know to be true,
/// either inferred or stated as facts (from the "EDB")
struct Assignments {
    assign: Vec<Atom>,
}

#[derive(Debug)]
pub enum SemanticError {
    UngroundedFact,
}

type Result<T> = result::Result<T, SemanticError>;

impl Rule {
    /// Replaces all variable terms with constants given by the assignments map
    /// assignments
    /// # Example
    /// ```
    /// rule = parse_rule("atom(X, Y) :- term(X), term(Y).").unwrap();
    /// rule.substitute_variables(vec)
    /// ```
    pub fn substitute_variables(&mut self, assignments: &HashMap<String, Term>) {
        fn substitute_terms(terms: &mut [Term], assignments: &HashMap<String, Term>) {
            for term in terms {
                match term {
                    Term::Variable(var) => {
                        if let Some(assign) = assignments.get(var) {
                            *term = assign.clone();
                        }
                    }
                    Term::Constant(_) => {}
                    Term::Compound(_, terms) => substitute_terms(terms, assignments),
                }
            }
        }

        substitute_terms(&mut self.head.terms, assignments);
        for literal in &mut self.body {
            let atom = literal.atom_mut();
            substitute_terms(&mut atom.terms, assignments);
        }
    }

    //pub fn get_unique_variables(&self)
    pub fn naive_ground_rule(&self, constants: Vec<Term>) -> Result<Rule> {
        Err(SemanticError::UngroundedFact)
    }
}

impl Term {
    fn get_base_terms(&self) -> Vec<Term> {
        match self {
            Term::Variable(_) => vec![self.clone()],
            Term::Constant(_) => vec![self.clone()],
            Term::Compound(_, terms) => terms
                .iter()
                .flat_map(|term| term.get_base_terms())
                .collect(),
        }
    }
}

impl Program {
    pub fn herbrand_universe(&self) -> Result<Vec<Term>> {
        // all constants from all facts
        let domain: Vec<Term> = self
            .facts
            .iter()
            .flat_map(|fact| fact.terms.iter())
            .flat_map(|term| term.get_base_terms())
            .collect();

        // TODO: is the assumption that facts should be automatically ground valid?
        // => if so, check if this assumption is true and otherwise return inference error
        if domain.iter().any(|term| !matches!(term, Term::Constant(_))) {
            return Err(SemanticError::UngroundedFact);
        }
        Ok(domain)
    }

    // Naive grounding algorithm, generate all ground instances of rules
    // O(|constants|^|variables|) in regards to the amount of variables in a rule
    pub fn naive_ground(&self) -> Result<Vec<Rule>> {
        let constants = self.herbrand_universe()?;
        // Know we know all ground constants
        // Collect all variables
        let mut ground_rule_permutations = Vec::new();

        //self.rules.iter().map(|rule| rule.naive_ground_rule(constants)).collect();

        for rule in self.rules.iter() {
            let unique_variables: HashSet<&Term> = rule
                .iter_terms()
                .filter(|term| matches!(term, Term::Variable(_)))
                .collect();
            let n = unique_variables.len();

            let assignment_permutations = std::iter::repeat(constants.iter())
                .take(n)
                .multi_cartesian_product();

            for constants in assignment_permutations {
                let mut grounded_rule = rule.clone(); // TODO: obviously very slow!!
                let ungrounded_terms = grounded_rule
                    .iter_terms()
                    .filter(|term| matches!(term, Term::Variable(_)));

                let mut variable_assignments = HashMap::<String, Term>::new();

                for term in ungrounded_terms {
                    let index = unique_variables.iter().position(|x| *x == term).unwrap();

                    let a = constants.get(index).unwrap();
                    if let Term::Variable(var) = term {
                        variable_assignments.insert(var.clone(), (*a).clone());
                    }
                }
                grounded_rule.substitute_variables(&variable_assignments);
                ground_rule_permutations.push(grounded_rule);
            }
        }

        Ok(ground_rule_permutations)
    }
}

fn S_A(I: Interpretation) {}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::language::parse::{parse_program, parse_rule};

    #[test]
    fn test_rule_grounding() {
        let program = parse_program(
            "
            eagly(eddy).
            penguin(tux).
            fly(X) :- bird(X), not -fly(X).
            comrad_of(X, Y) :- penguin(X), penguin(Y).     % all penguins are comrads
            friend_of(X) :- penguin(X), eagly(Y).          % all penguins are friend of eddy the eagle
        ",
        )
        .unwrap();

        let grounded_rules = vec![
            "fly(eddy) :- bird(eddy), not -fly(eddy).",
            "fly(tux) :- bird(tux), not -fly(tux).",
            // test out cartesian explosion
            "comrad_of(tux, tux) :- penguin(tux), penguin(tux).",
            "comrad_of(tux, eddy) :- penguin(tux), penguin(eddy).",
            "comrad_of(eddy, tux) :- penguin(eddy), penguin(tux).",
            "comrad_of(eddy, eddy) :- penguin(eddy), penguin(eddy).",
            // test out variables only appearing in the body
            "friend_of(eddy) :- penguin(eddy), eagly(eddy).",
            "friend_of(eddy) :- penguin(eddy), eagly(tux).",
            "friend_of(tux) :- penguin(tux), eagly(eddy).",
            "friend_of(tux) :- penguin(tux), eagly(tux).",
        ];
        let result = program.naive_ground().unwrap();

        println!("{}", result.iter().format("\n"));
        assert_eq!(grounded_rules.len(), result.len());
        for rule in grounded_rules {
            println!("parsing.. {}", rule);
            let parsed_grounded_rule = parse_rule(rule).unwrap().unwrap_left();
            assert!(result.contains(&parsed_grounded_rule));
        }
    }
}
