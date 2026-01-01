use core::fmt;
use std::{borrow::Borrow, ops::Deref};

use crate::language::glossary::*;

/// Generic wrapper guaranteeing groundness (in debug mode)
#[derive(Debug, Clone, Hash)]
pub struct Ground<T>(T);

// "You are grounded"
/// Something can be constructed to a `Ground` variant once it defines the `is_ground()` check
pub trait Groundable {
    fn is_ground(&self) -> bool;
}

impl<'a, T: Clone> Ground<&'a T> {
    pub fn new_ref(value: &'a T) -> Self
    where
        T: Groundable,
    {
        Ground(value)
    }
    pub fn to_owned(&self) -> Ground<T> {
        Ground(self.0.clone())
    }
}

impl<T> Ground<T> {
    pub fn new(value: T) -> Self
    where
        T: Groundable,
    {
        // debug only check, so we avoid the check in release
        debug_assert!(
            value.is_ground(),
            "Attempted to create a grounded type with a non-grounded value"
        );
        Ground(value)
    }

    pub fn inner(&self) -> &T {
        &self.0
    }

    pub fn into_inner(self) -> T {
        self.0
    }

    pub fn as_ground_ref(&self) -> Ground<&T> {
        Ground(&self.0)
    }
}

// Transparent access via Deref
impl<T> Deref for Ground<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> AsRef<T> for Ground<T> {
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T> Borrow<T> for Ground<T> {
    fn borrow(&self) -> &T {
        &self.0
    }
}

impl<'a, T> Borrow<T> for Ground<&'a T> {
    fn borrow(&self) -> &T {
        self.0
    }
}

/// Grounding
impl Groundable for Atom {
    fn is_ground(&self) -> bool {
        self.iter_terms().all(|t| t.is_constant())
    }
}

impl Groundable for Literal {
    fn is_ground(&self) -> bool {
        self.atom().is_ground()
    }
}
impl Groundable for Rule {
    fn is_ground(&self) -> bool {
        self.iter_terms().all(|t| t.is_constant())
    }
}
impl Groundable for Program {
    fn is_ground(&self) -> bool {
        self.facts.iter().all(|atom| atom.is_ground())
            && self.rules.iter().all(|literal| literal.is_ground())
    }
}
impl Groundable for Term {
    fn is_ground(&self) -> bool {
        match self {
            Term::Variable(_) => false,
            Term::Constant(_) => true,
            Term::Compound(_, terms) => terms.iter().all(|term| term.is_ground()),
        }
    }
}

// Type aliases
pub type GroundProgram = Ground<Program>;
pub type GroundRule = Ground<Rule>;
pub type GroundLiteral = Ground<Literal>;
pub type GroundAtom = Ground<Atom>;

impl Ground<Program> {
    /// Get the grounded facts
    pub fn ground_facts(&self) -> impl Iterator<Item = Ground<&Atom>> {
        self.inner().facts.iter().map(Ground::new_ref)
    }

    /// Get the grounded facts
    pub fn ground_rules(&self) -> impl Iterator<Item = Ground<&Rule>> {
        self.inner().rules.iter().map(Ground::new_ref)
    }
}

impl Ground<Rule> {
    pub fn ground_terms(&self) -> impl Iterator<Item = Ground<&Term>> {
        self.inner().iter_terms().map(Ground::new_ref)
    }

    pub fn ground_head(&self) -> Ground<&Atom> {
        Ground::new_ref(&self.head)
    }
}

impl Ground<Atom> {
    pub fn into_positive(&self) -> Ground<Literal> {
        Ground::new(Literal::Positive(self.0.clone()))
    }
    pub fn into_negative(&self) -> Ground<Literal> {
        Ground::new(Literal::Negative(self.0.clone()))
    }
}

impl<T: fmt::Display> fmt::Display for Ground<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            // Pretty-print with marker: println!("{:#}", ground_rule)
            write!(f, "⊥{:#}", self.inner())
        } else {
            self.inner().fmt(f)
        }
    }
}

// Compare Ground<T> with Ground<&U> where T can be compared with U
impl<T, U> PartialEq<Ground<U>> for Ground<T>
where
    T: PartialEq<U>,
{
    fn eq(&self, other: &Ground<U>) -> bool {
        self.0 == other.0
    }
}

// Compare Ground<T> with &U directly (very convenient!)
impl<T, U> PartialEq<&U> for Ground<T>
where
    T: PartialEq<U>,
{
    fn eq(&self, other: &&U) -> bool {
        &self.0 == *other
    }
}
