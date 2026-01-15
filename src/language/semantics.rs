use std::{
    collections::{HashMap, HashSet},
    iter, result,
};

use either::Either;
use itertools::Itertools;

use crate::language::{glossary::*, ground::*, pretty};

/// The Herbrand Universe (all constants)
pub type HerbrandUniverse = HashSet<Term>;

/// Three valued logical value
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TruthValue {
    True,
    False,
    Unknown,
}

struct Interpretation {
    true_atoms: HashSet<GroundAtom>,
    false_atoms: HashSet<GroundAtom>,
}

impl Interpretation {
    pub fn new() -> Self {
        Self {
            true_atoms: HashSet::new(),
            false_atoms: HashSet::new(),
        }
    }
    /// Returns if we know an atom to be true or false, or unknown otherwise
    pub fn truth_value(&self, atom: &Atom) -> TruthValue {
        if self.true_atoms.contains(atom) {
            TruthValue::True
        } else if self.false_atoms.contains(atom) {
            TruthValue::False
        } else {
            TruthValue::Unknown
        }
    }
}

#[derive(Debug)]
pub enum SemanticError {
    UngroundedFact(Fact),
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
}

impl Program {
    /// Returns all unique constants of a program
    pub fn herbrand_universe(&self, only_facts: bool) -> HerbrandUniverse {
        let atom_iter = if only_facts {
            Either::Left(self.facts.iter())
        } else {
            Either::Right(
                self.facts
                    .iter()
                    .chain(self.rules.iter().flat_map(|rule| rule.iter_atoms())),
            )
        };
        // all constants from all facts (& rules if only_facts = true)
        atom_iter
            .flat_map(|fact| fact.terms.iter())
            .flat_map(|term| term.get_base_terms())
            .filter(|term| term.is_constant())
            .collect::<HerbrandUniverse>()
    }
}

/// An interpretation containing negative literals
type LiteralSet = HashSet<GroundLiteral>;

impl GroundProgram {
    pub fn get_unknowns<'a>(
        &'a self,
        interpretation: &'a Interpretation,
    ) -> HashSet<Ground<&'a Atom>> {
        // H di interpretation
        let H = self
            .herbrand_base()
            .into_iter()
            .collect::<HashSet<Ground<&Atom>>>();
        let interpretation_atoms = interpretation
            .true_atoms
            .union(&interpretation.false_atoms)
            .map(|a| a.inner())
            .map(Ground::new_ref)
            .collect::<HashSet<Ground<&Atom>>>();

        H.difference(&interpretation_atoms).cloned().collect()
    }

    /// Returns all unique grounded atoms
    pub fn herbrand_base(&self) -> HashSet<Ground<&Atom>> {
        self.rules
            .iter()
            .flat_map(|rule| rule.iter_atoms())
            .map(Ground::new_ref)
            .collect::<HashSet<Ground<&Atom>>>()
    }

    pub fn immediate_consequence_operator(&self, i: &LiteralSet) {
        todo!();
    }
    // https://dl.acm.org/doi/pdf/10.1145/73721.73722

    /// Returns the eventual consequences, only positive literals, standard bottom-up rule firing
    /// lf_P(I)
    /// Apply the immediate consequence operator until convergence
    pub fn horn_least_fixpoint(&self, I: &LiteralSet) -> Result<LiteralSet> {
        // what can we know, when we assume "i" is false?
        // assume :- T_P (program facts) ∪ I
        let mut assumptions = self
            .facts
            .iter()
            .map(|fact| fact.into_positive())
            .collect::<LiteralSet>();
        // we assume that I is positive
        let mut result = assumptions.clone(); // all facts are included
        assumptions.extend(I.iter().cloned());

        loop {
            let prev_size = result.len();
            for rule in &self.rules {
                let body_satisfied = rule
                    .body()
                    .iter()
                    //.filter(|&lit| {result.contains(&Ground::new(lit.to_positive())) && lit.is_positive()}) // Make the rule into a horn clause
                    .all(|lit| assumptions.contains(lit));
                if body_satisfied {
                    let head_lit = rule.head().into_positive();
                    result.insert(head_lit.clone());
                    assumptions.insert(head_lit);
                }
            }
            if result.len() == prev_size {
                break;
            }
        }
        println!(
            "Sₚ({{{}}}) = {{{}}}",
            I.iter().format(", "),
            result.iter().format(", ")
        );
        Ok(result)
    }

    /// Negative consequences
    // s_P(I) = ¬(H - S_P(I))
    fn negative_complement(&self, I: &LiteralSet) -> Result<LiteralSet> {
        // create all the negatives of atoms that are in H but not in the horn least fixpoint
        let H = self.herbrand_base();

        let S_P = self.horn_least_fixpoint(I)?;
        println!("H: {{{}}}", H.iter().format(", "));
        let s_P = H
            .iter()
            .filter(|&atom| !S_P.contains(&atom.into_positive())) // H - S_P(I)
            .map(|atom| atom.into_negative()) // ¬(...)
            .collect::<LiteralSet>();
        println!(
            "Śₚ({{{}}}): {}",
            I.iter().format(", "),
            s_P.iter().format(", ")
        );
        Ok(s_P)
    }

    /// Alternating operator
    // A_P = s_P(s_P(I))
    pub fn A_P(&self, I: &LiteralSet) -> Result<LiteralSet> {
        self.negative_complement(&self.negative_complement(I)?)
    }

    // Solver
    pub fn alternating_fixpoint(&self) -> Result<Interpretation> {
        let mut loops = 0;
        let mut I = LiteralSet::new(); // Assume nothing is false
        loop {
            println!(
                "I{} = {{{}}}",
                pretty::num_to_subscript(loops),
                I.iter().format(", ")
            );
            let i_next = self.A_P(&I)?;
            loops += 2;
            if I.len() == i_next.len() && i_next.iter().all(|i| I.contains(i)) {
                break; // convergence!
            }
            I = i_next;
        }

        let positives = self
            .horn_least_fixpoint(&I)?
            .iter()
            .map(|lit| Ground::new(lit.atom().clone()))
            .collect::<HashSet<GroundAtom>>();

        let interpretation = Interpretation {
            true_atoms: positives,
            false_atoms: I
                .iter()
                .map(|lit| Ground::new(lit.atom().clone()))
                .collect::<HashSet<GroundAtom>>(),
        };
        Ok(interpretation)
    }
}

/// Semi naive grounding algorithm, don't generate ground instances that are not possible
pub fn semi_naive_ground(rule: &Program) -> Result<GroundProgram> {
    todo!()
}
pub fn naive_ground_rule(rule: &Rule, constants: &HashSet<Term>) -> HashSet<Ground<Rule>> {
    // Collect all variables from the current rule
    let unique_variables: HashSet<&Term> = rule
        .iter_terms()
        .filter(|term| term.is_variable())
        .collect();
    let n = unique_variables.len();

    let assignment_permutations = std::iter::repeat(constants.iter())
        .take(n)
        .multi_cartesian_product();

    let mut ground_rule_permutations = HashSet::new();
    for constants in assignment_permutations {
        let mut grounded_rule = rule.clone(); // TODO: obviously very slow!!
        let ungrounded_terms = grounded_rule.iter_terms().filter(|term| term.is_variable());

        let mut variable_assignments = HashMap::<String, Term>::new();

        for term in ungrounded_terms {
            let index = unique_variables.iter().position(|x| *x == term).unwrap();

            let a = constants.get(index).unwrap();
            if let Term::Variable(var) = term {
                variable_assignments.insert(var.clone(), (*a).clone());
            }
        }
        grounded_rule.substitute_variables(&variable_assignments);
        ground_rule_permutations.insert(Ground::new(grounded_rule));
    }
    ground_rule_permutations
}

/// Naive grounding algorithm, generate all possible ground instances of rules
/// O(|constants|^|variables|)
pub fn naive_ground(program: &Program) -> Result<GroundProgram> {
    let constants = program.herbrand_universe(true);

    Ok(GroundProgram {
        facts: program
            .facts
            .iter()
            .map(|atom| Ground::new(atom.clone()))
            .collect(),
        rules: program
            .rules
            .iter()
            .flat_map(|rule| naive_ground_rule(rule, &constants))
            .collect::<Vec<Ground<Rule>>>(),
    })
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use itertools::Itertools;

    use crate::language::{
        glossary::*,
        ground::*,
        parse::{parse_program, parse_rule},
        semantics::{LiteralSet, naive_ground},
    };

    fn create_program_results<T>(program: &str, results: T) -> (GroundProgram, T) {
        let program = naive_ground(&parse_program(program).unwrap()).unwrap();
        (program, results)
    }

    fn allen_van_gelder() -> (GroundProgram, Vec<(Vec<&'static str>, Vec<&'static str>)>) {
        create_program_results(
            // d e f g h
            "
            a :- c, not b.
            b :- not a.
            c.

            d :- e, not f.
            d :- f, not g.
            d :- h.
            e :- d.
            f :- e.
            f :- not c.
            i :- c, not d.
        ",
            // intermediate results
            // k    (I_k,   S_P(I_k))
            vec![
                (vec![], vec!["c"]), // k = 0
                (
                    vec!["a", "b", "d", "e", "f", "g", "h", "i"],
                    vec!["a", "b", "c", "i"],
                ), // k = 1
                (vec!["d", "e", "f", "g", "h"], vec!["c", "i"]), // k = 2
                (
                    vec!["a", "b", "d", "e", "f", "g", "h"],
                    vec!["a", "b", "c", "i"],
                ), // k = 3
                (vec!["d", "e", "f", "g", "h"], vec!["c", "i"]), // k = 4
            ],
        )
    }

    fn not_not_recursion() -> (GroundProgram, Vec<&'static str>) {
        create_program_results("p :- not q. q :- not p.", vec![])
    }

    // lalalala *kah kah*
    fn alberto_the_piano_playing_seagull() -> (GroundProgram, Vec<&'static str>) {
        create_program_results(
            "
            seagull(alberto).
            piano_player(lien).
            piano_player(X) :- seagull(X).
            has_job(X) :- piano_player(X), not seagull(X).
        ",
            vec![
                "seagull(alberto)",
                "piano_player(alberto)",
                "piano_player(lien)",
                "has_job(lien)",
            ],
        )
    }

    fn tux_the_non_flying_penguin() -> (GroundProgram, Vec<&'static str>) {
        create_program_results(
            "
            % instance
            eagle(eddy).
            penguin(tux).

            % encoding
            -fly(X) :- penguin(X).
            bird(X) :- penguin(X).
            bird(X) :- eagle(X).
            fly(X) :- bird(X), not -fly(X).
        ",
            vec![
                "bird(eddy)",
                "bird(tux)",
                "-fly(tux)",
                "fly(eddy)",
                "penguin(tux)",
                "eagle(eddy)",
            ],
        )
    }

    #[test]
    fn test_paper_intermediate_results() {
        let (program, intermediate_results) = allen_van_gelder();

        let mut I = LiteralSet::new(); // Assume nothing is false
        for (i, s_p) in intermediate_results {
            let S_p = program
                .horn_least_fixpoint(&I)
                .unwrap()
                .iter()
                .map(|a| format!("{}", a))
                .sorted()
                .collect::<Vec<String>>();

            assert_eq!(s_p, S_p);
            assert_eq!(
                *i,
                I.iter()
                    .map(|a| format!("{}", a.to_positive()))
                    .sorted()
                    .collect::<Vec<String>>()
            );

            I = program.negative_complement(&I).unwrap();
        }
    }

    #[test]
    // Step 1. Grounding of the input
    fn test_rule_grounding() {
        let program = parse_program(
            "
            eagly(eddy).
            penguin(tux).
            fly(X) :- bird(X), not -fly(X).
            comrad_of(X, Y) :- penguin(X), penguin(Y).     % all penguins are comrads
            friend_of(X) :- penguin(X), eagly(Y).          % all penguins are friend of eddy the eagle
        ",
        ).unwrap();

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
        let grounded_program = naive_ground(&program).unwrap();

        assert_eq!(grounded_rules.len(), grounded_program.rules.len());
        for rule in grounded_rules {
            let parsed_rule = parse_rule(rule).unwrap().unwrap_left();
            let parsed_grounded_rule = Ground::new(parsed_rule);
            assert!(grounded_program.rules.contains(&parsed_grounded_rule));
        }
    }

    #[test]
    // Step 2. Horn least fixpoint
    fn test_horn() {
        let (grounded_program, _) = tux_the_non_flying_penguin();

        let i = LiteralSet::new();
        let inferred_idb = grounded_program
            .horn_least_fixpoint(&i)
            .unwrap()
            .iter()
            .map(|atom| format!("{}", atom))
            .collect::<Vec<String>>();

        let expected_idb = HashSet::from([
            // EDB
            "eagle(eddy)",
            "penguin(tux)",
            // IDB
            "bird(eddy)",
            "bird(tux)",
            "-fly(tux)",
            "fly(eddy)",
            // Optimistic horn fixpoint evaluation causes us to think tux can fly
            // Fixed by using the alternating fixpoint!
            "fly(tux)",
        ]);

        let equal = inferred_idb
            .iter()
            .all(|fact| expected_idb.contains(fact.as_str()));
        assert!(equal);
    }
    #[test]
    fn test_alternating_fixpoints() {
        for (program, positives) in vec![
            tux_the_non_flying_penguin(),
            alberto_the_piano_playing_seagull(),
            not_not_recursion(),
        ] {
            test_alternating_fixpoint(program, positives);
        }
    }

    fn test_alternating_fixpoint(program: GroundProgram, positives: Vec<&'static str>) {
        let interpretation = program.alternating_fixpoint().unwrap();
        let strs: Vec<String> = interpretation
            .true_atoms
            .iter()
            .map(|f| format!("{}", f))
            .collect();

        println!("True: [{}]", interpretation.true_atoms.iter().format(", "));
        println!(
            "False: [{}]",
            interpretation.false_atoms.iter().format(", ")
        );
        println!(
            "Unknown: [{}]",
            program.get_unknowns(&interpretation).iter().format(", ")
        );

        let true_interpretation_matches = strs.iter().all(|t| positives.contains(&t.as_str()));
        assert!(true_interpretation_matches && strs.len() == positives.len());
    }
}
