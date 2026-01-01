use core::fmt;
use std::mem;

use crate::language::cursor::CursorError;
use crate::language::ground::GroundLiteral;
use crate::language::iter::*;

/// A program consists of a set of rules & facts.
/// # Example
///
/// ```
/// % instance
/// eagle(eddy).
/// penguin(tux).
/// % encoding
///  fly(X) :- bird(X), not -fly(X).
/// -fly(X) :- penguin(X).
/// bird(X) :- penguin(X).
/// bird(X) :- eagle(X).
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub facts: Vec<Fact>, // Ground facts (EDB)
    pub rules: Vec<Rule>, // IDB
}

/// Logical implication, with:
/// - a head representing the goal of the rule,
/// - a body with 1, or more, subgoals that can be either positive or negative.
///
/// Comma's separating the body are conjunctions.
/// A rule without a body is a fact.
///
/// # Example
/// X can fly if it is a bird and does not have impeded flight.
/// ```
/// fly(X) :- bird(X), not -fly(X)
/// ^^^^^^    ^^^^^^^^^^^^^^^^^^^^
///  head           body
/// ```
/// Can a coconut fly?
/// ```
/// fly(coconut).
/// ```
///
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Rule {
    pub head: Atom,
    pub body: Vec<Literal>,
}

/// A rule with no body, where the goal is set to true.
/// # Example
/// Tux is a penguin.
/// ```
/// penguin(tux).
/// eagly(eddy).
/// ```
pub type Fact = Atom;

/// A query asked on the set of known information.
/// # Example
/// Is tux a penguin?
/// ```
/// ?- penguin(tux).
/// ```
pub struct Query {
    goal: Atom,
}

/// A predicate applied to terms, string-wise identical to a compound term.
/// # Example
/// ```
/// bigger(tux, eddy)
/// ```
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Atom {
    pub predicate: Identifier,
    pub terms: Vec<Term>,
}

/// An atomic formula (Atom) that can be either positive or negative.
/// # Example
/// ```
/// fly(X), not fly(X)
/// ```
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum Literal {
    Positive(Atom),
    Negative(Atom),
}

/// Valid identifiers are of the following grammar
/// ```
/// identifier   ::- (alphabetic | '-'), {alphanumeric | '_'}
/// alphabetic   ::- 'a'-'z' | 'A'-'Z'
/// alphanumeric ::- alphabetic | '0'-'9'
/// ```
pub type Identifier = String;

/// A term is either a variable, constant, or a function with terms as arguments.
/// Variables start with a capital letter,
/// whereas constants and functions start with a lower case letter.
/// # Example
/// ```
/// X, b, f(...)
/// ```
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Term {
    /// A variable is an identifier starting with a capital letter
    Variable(Identifier),
    /// A constant is an identifier with all lowercase characters
    Constant(Identifier),
    /// A compound term, consisting of a
    Compound(Identifier, Vec<Term>),
}

impl Term {
    pub fn compound(value: &str, terms: Vec<Term>) -> Self {
        Term::Compound(String::from(value), terms)
    }

    pub fn variable(value: &str) -> Self {
        Term::Variable(String::from(value))
    }
    pub fn constant(value: &str) -> Self {
        Term::Constant(String::from(value))
    }
    pub const fn is_compound(&self) -> bool {
        matches!(self, Term::Compound(_, _))
    }
    pub const fn is_variable(&self) -> bool {
        matches!(self, Term::Variable(_))
    }
    pub const fn is_constant(&self) -> bool {
        matches!(self, Term::Constant(_))
    }

    pub fn get_base_terms(&self) -> Vec<Term> {
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

impl Rule {
    pub fn from(head: Atom, body: Vec<Literal>) -> Self {
        Rule { head, body }
    }

    pub fn fact(&self) -> &Fact {
        &self.head
    }
    /// Check if the rule is a fact
    pub fn is_fact(&self) -> bool {
        self.body.is_empty()
    }

    /// Returns an iterator over all nodes in the rule tree (depth first search)
    pub fn iter(&self) -> RuleIter<'_> {
        RuleIter::new(self)
    }

    /// Returns an iterator over all terms in the rule tree
    pub fn iter_terms(&self) -> impl Iterator<Item = &Term> {
        self.iter().filter_map(|element| match element {
            Element::Term(term) => Some(term),
            _ => None,
        })
    }

    /// Returns an iterator over all the atoms in the rule tree
    pub fn iter_atoms(&self) -> impl Iterator<Item = &Atom> {
        self.iter().filter_map(|element| match element {
            Element::Atom(atom) => Some(atom),
            _ => None,
        })
    }

    /// Returns an iterator over all the literals in the rule tree
    pub fn iter_literals(&self) -> impl Iterator<Item = &Literal> {
        self.iter().filter_map(|element| match element {
            Element::Literal(literal) => Some(literal),
            _ => None,
        })
    }

    pub fn negative_literals(&self) -> Vec<&Literal> {
        self.body
            .iter()
            .filter(|lit| lit.is_negative())
            .collect::<Vec<&Literal>>()
    }
    pub fn positive_literals(&self) -> Vec<&Literal> {
        self.body
            .iter()
            .filter(|lit| lit.is_positive())
            .collect::<Vec<&Literal>>()
    }
}

impl Atom {
    pub fn from(pred: &str, terms: Vec<Term>) -> Self {
        Atom {
            predicate: Identifier::from(pred),
            terms,
        }
    }

    /// Returns an iterator over all terms (including nested compound terms)
    // TODO: might wanna remove?
    pub fn iter_terms(&self) -> impl Iterator<Item = &Term> + '_ {
        fn collect_terms<'a>(terms: &'a [Term], output: &mut Vec<&'a Term>) {
            for term in terms {
                output.push(term);
                if let Term::Compound(_, nested) = term {
                    collect_terms(nested, output);
                }
            }
        }

        let mut all_terms = Vec::new();
        collect_terms(&self.terms, &mut all_terms);
        all_terms.into_iter()
    }

    /// Returns an iterator over just the variables in this atom
    pub fn iter_variables(&self) -> impl Iterator<Item = &Term> + '_ {
        self.iter_terms().filter(|t| t.is_variable())
    }

    /// Returns an iterator over just the constants in this atom
    pub fn iter_constants(&self) -> impl Iterator<Item = &Term> + '_ {
        self.iter_terms().filter(|t| t.is_constant())
    }

    /// Clones the atom into a positive literal
    pub fn into_positive(&self) -> Literal {
        Literal::Positive(self.clone())
    }

    /// Clones the atom into a negative literal
    pub fn into_negative(&self) -> Literal {
        Literal::Negative(self.clone())
    }
}

impl Literal {
    pub fn is_complement(&self, other: &Literal) -> bool {
        match (self, other) {
            (Literal::Positive(lhs), Literal::Negative(rhs)) => lhs == rhs,
            (Literal::Negative(lhs), Literal::Positive(rhs)) => lhs == rhs,
            (_, _) => false,
        }
    }
    pub fn positive(pred: &str, terms: Vec<Term>) -> Self {
        Literal::Positive(Atom::from(pred, terms))
    }
    pub fn negative(pred: &str, terms: Vec<Term>) -> Self {
        Literal::Negative(Atom::from(pred, terms))
    }

    pub fn to_positive(&self) -> Self {
        match self {
            Literal::Positive(a) | Literal::Negative(a) => Literal::Positive(a.clone()),
        }
    }

    pub fn atom_mut(&mut self) -> &mut Atom {
        match self {
            Literal::Positive(a) | Literal::Negative(a) => a,
        }
    }
    pub fn atom(&self) -> &Atom {
        match self {
            Literal::Positive(a) | Literal::Negative(a) => a,
        }
    }
    pub fn is_positive(&self) -> bool {
        matches!(self, Literal::Positive(_))
    }
    pub fn is_negative(&self) -> bool {
        matches!(self, Literal::Negative(_))
    }
}

impl From<Atom> for Rule {
    fn from(value: Atom) -> Self {
        Rule {
            head: value,
            body: Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fact_construction() {
        // penguin(tux).
        let written_penguin = Rule {
            head: Atom {
                predicate: Identifier::from("penguin"),
                terms: vec![Term::Constant(Identifier::from("ping"))],
            },
            body: Vec::new(),
        };

        let constructed_penguin_fact = Fact::from("penguin", vec![Term::constant("ping")]);

        assert_eq!(*written_penguin.fact(), constructed_penguin_fact);
    }

    #[test]
    fn test_rule_construction() {
        // fly(X) :- bird(X), not -fly(X)
        let written_fly_rule = Rule {
            head: Atom {
                predicate: Identifier::from("fly"),
                terms: vec![Term::Variable(Identifier::from("X"))],
            },
            body: vec![
                Literal::Positive(Atom {
                    predicate: Identifier::from("bird"),
                    terms: vec![Term::Variable(Identifier::from("X"))],
                }),
                Literal::Negative(Atom {
                    predicate: Identifier::from("-fly"),
                    terms: vec![Term::Variable(Identifier::from("X"))],
                }),
            ],
        };

        let constructed_fly_rule = Rule::from(
            Atom::from("fly", vec![Term::variable("X")]),
            vec![
                Literal::positive("bird", vec![Term::variable("X")]),
                Literal::negative("-fly", vec![Term::variable("X")]),
            ],
        );

        assert_eq!(written_fly_rule, constructed_fly_rule);
    }
}
