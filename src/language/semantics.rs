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
enum SemanticError {
    UngroundedFact,
}

type Result<T> = result::Result<T, SemanticError>;

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
    // concretize
    // naive grounding algorithm
    // O(n^n) in regards to the amount of variables in a rule
    //
    pub fn naive_ground(&self) -> Result<Vec<Rule>> {
        // all constants from all facts
        let domain: Vec<Term> = self
            .facts
            .iter()
            .flat_map(|fact| fact.terms.iter().flat_map(|fact| fact.get_base_terms()))
            .collect();

        // TODO: is the assumption that facts should be automatically ground valid?
        // => if so, check if this assumption is true and otherwise return inference error
        let any_ungrounded_term = domain.iter().any(|term| !matches!(term, Term::Constant(_)));
        if any_ungrounded_term {
            return Err(SemanticError::UngroundedFact);
        }

        // Know we know all ground constants
        // Collect all variables
        let mut ground_rule_permutations = Vec::new();
        for rule in self.rules.iter() {
            let unique_variables: Vec<&Term> = rule
                .head
                .iter_terms()
                .filter(|term| matches!(term, Term::Variable(_)))
                .collect();
            let n = unique_variables.len();

            let assignment_permutations = std::iter::repeat(domain.iter())
                .take(n)
                .multi_cartesian_product();

            for constants in assignment_permutations {
                let mut grounded_rule = rule.clone(); // TODO: obviously very slow!!
                let ungrounded_terms = grounded_rule
                    .iter_terms()
                    .filter(|term| matches!(term, Term::Variable(_)));

                for mut term in ungrounded_terms {
                    let index = unique_variables
                        .iter()
                        .position(|x: &&Term| **x == *term)
                        .unwrap();
                    let a = unique_variables.get(index).unwrap();
                    term = a;
                }
                ground_rule_permutations.push(grounded_rule);
            }
        }

        Ok(ground_rule_permutations)
    }
}

fn S_A(I: Interpretation) {}

#[cfg(test)]
mod tests {
    use crate::language::parse::{parse_program, parse_rule};

    #[test]
    fn test_rule_grounding() {
        let program = parse_program(
            "
            eagly(eddy).
            penguin(tux).
            fly(X) :- bird(X), not -fly(X).
        ",
        )
        .unwrap();

        let grounded_rules = vec![
            "fly(eddy) :- bird(eddy), not -fly(eddy).",
            "fly(tux) :- bird(tux), not -fly(tux).",
        ];
        let result = program.naive_ground().unwrap();
        for rule in grounded_rules {
            let parsed_grounded_rule = parse_rule(rule).unwrap().unwrap_left();
            //assert!(result.contains(&parsed_grounded_rule));
            println!("{:?}", parsed_grounded_rule);
        }
    }
}
