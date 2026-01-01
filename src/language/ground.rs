use core::fmt;
use std::{borrow::Borrow, ops::Deref};

use crate::language::glossary::*;

/// Generic wrapper guaranteeing groundness (in debug mode)
#[derive(Debug, Clone, Hash)]
pub struct Ground<T>(T);

/// Something can be constructed to a `Ground` variant once it defines the `is_ground()` check
pub trait Groundable {
    fn is_ground(&self) -> bool;
}

// ============================================================================
// Construction
// ============================================================================

impl<T> Ground<T> {
    pub fn new(value: T) -> Self
    where
        T: Groundable,
    {
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

impl<'a, T> Ground<&'a T> {
    pub fn new_ref(value: &'a T) -> Self
    where
        T: Groundable,
    {
        debug_assert!(
            value.is_ground(),
            "Attempted to create a grounded type with a non-grounded value"
        );
        Ground(value)
    }

    pub fn to_owned(&self) -> Ground<T>
    where
        T: Clone,
    {
        Ground(self.0.clone())
    }

    // Direct access to the inner reference
    pub fn inner_ref(&self) -> &'a T {
        self.0
    }
}

// ============================================================================
// Transparent Access
// ============================================================================

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

impl<'a, T> AsRef<T> for Ground<&'a T> {
    fn as_ref(&self) -> &T {
        self.0
    }
}

impl<T> Borrow<T> for Ground<T> {
    fn borrow(&self) -> &T {
        &self.0
    }
}

impl<'a, T> Borrow<T> for &'a Ground<T> {
    fn borrow(&self) -> &T {
        &self.0
    }
}
impl<'a, T> Borrow<T> for Ground<&'a T> {
    fn borrow(&self) -> &T {
        self.0
    }
}

// ============================================================================
// Comparison - Make Ground<T> and Ground<&T> fully interchangeable
// ============================================================================

impl<T: PartialEq> PartialEq for Ground<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: Eq> Eq for Ground<T> {}

// Ground<T> == &U (compare with raw reference)
impl<T, U: ?Sized> PartialEq<&U> for Ground<T>
where
    T: PartialEq<U>,
{
    fn eq(&self, other: &&U) -> bool {
        &self.0 == *other
    }
}

// ============================================================================
// Shared Trait for Common Operations
// ============================================================================

/// Trait for operations that work on both Ground<T> and Ground<&T>
pub trait GroundOps<T: ?Sized> {
    /// Get a reference to the inner value
    fn as_inner(&self) -> &T;
}

impl<T> GroundOps<T> for Ground<T> {
    fn as_inner(&self) -> &T {
        &self.0
    }
}

impl<'a, T> GroundOps<T> for Ground<&'a T> {
    fn as_inner(&self) -> &T {
        self.0
    }
}

// ============================================================================
// Rule-specific Operations (works for both Ground<Rule> and Ground<&Rule>)
// ============================================================================

pub trait GroundRuleOps {
    fn iter_terms(&self) -> impl Iterator<Item = Ground<&Term>>;
    fn head(&self) -> Ground<&Atom>;
    fn body(&self) -> &[Literal];
}

impl GroundRuleOps for Ground<Rule> {
    fn iter_terms(&self) -> impl Iterator<Item = Ground<&Term>> {
        self.0.iter_terms().map(Ground::new_ref)
    }

    fn head(&self) -> Ground<&Atom> {
        Ground::new_ref(&self.0.head)
    }

    fn body(&self) -> &[Literal] {
        &self.0.body
    }
}

impl<'a> GroundRuleOps for Ground<&'a Rule> {
    fn iter_terms(&self) -> impl Iterator<Item = Ground<&Term>> {
        self.0.iter_terms().map(Ground::new_ref)
    }

    fn head(&self) -> Ground<&Atom> {
        Ground::new_ref(&self.0.head)
    }

    fn body(&self) -> &[Literal] {
        &self.0.body
    }
}

// Backwards compatibility
impl Ground<Rule> {
    pub fn ground_terms(&self) -> impl Iterator<Item = Ground<&Term>> {
        self.iter_terms()
    }

    pub fn ground_head(&self) -> Ground<&Atom> {
        self.head()
    }
}

// ============================================================================
// Atom-specific Operations (works for both Ground<Atom> and Ground<&Atom>)
// ============================================================================

pub trait GroundAtomOps {
    fn into_positive(&self) -> Ground<Literal>;
    fn into_negative(&self) -> Ground<Literal>;
}

impl GroundAtomOps for Ground<Atom> {
    fn into_positive(&self) -> Ground<Literal> {
        Ground::new(Literal::Positive(self.0.clone()))
    }

    fn into_negative(&self) -> Ground<Literal> {
        Ground::new(Literal::Negative(self.0.clone()))
    }
}

impl<'a> GroundAtomOps for Ground<&'a Atom> {
    fn into_positive(&self) -> Ground<Literal> {
        Ground::new(Literal::Positive(self.0.clone()))
    }

    fn into_negative(&self) -> Ground<Literal> {
        Ground::new(Literal::Negative(self.0.clone()))
    }
}

// Backwards compatibility
impl Ground<Atom> {
    pub fn into_(&self) -> Ground<Literal> {
        self.into_positive()
    }
}

// ============================================================================
// Vector/Collection Extensions
// ============================================================================

pub trait GroundVecExt<'a, T> {
    fn contains_ref<U>(&self, item: &U) -> bool
    where
        T: PartialEq<U>;

    fn contains_ground<U>(&self, item: &Ground<U>) -> bool
    where
        Ground<T>: PartialEq<Ground<U>>;

    fn contains_ground_ref<U>(&self, item: &Ground<&'a U>) -> bool
    where
        Ground<T>: PartialEq<Ground<&'a U>>;
}

impl<'a, T> GroundVecExt<'a, T> for Vec<Ground<T>> {
    fn contains_ref<U>(&self, item: &U) -> bool
    where
        T: PartialEq<U>,
    {
        self.iter().any(|g| &g.0 == item)
    }

    fn contains_ground<U>(&self, item: &Ground<U>) -> bool
    where
        Ground<T>: PartialEq<Ground<U>>,
    {
        self.iter().any(|g| g == item)
    }

    fn contains_ground_ref<U>(&self, item: &Ground<&'a U>) -> bool
    where
        Ground<T>: PartialEq<Ground<&'a U>>,
    {
        self.iter().any(|g| g == item)
    }
}

impl<'a, T> GroundVecExt<'a, T> for [Ground<T>] {
    fn contains_ref<U>(&self, item: &U) -> bool
    where
        T: PartialEq<U>,
    {
        self.iter().any(|g| &g.0 == item)
    }

    fn contains_ground<U>(&self, item: &Ground<U>) -> bool
    where
        Ground<T>: PartialEq<Ground<U>>,
    {
        self.iter().any(|g| g == item)
    }

    fn contains_ground_ref<U>(&self, item: &Ground<&'a U>) -> bool
    where
        Ground<T>: PartialEq<Ground<&'a U>>,
    {
        self.iter().any(|g| g == item)
    }
}

// ============================================================================
// Groundable implementations
// ============================================================================

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

// ============================================================================
// Type aliases
// ============================================================================

pub type GroundRule = Ground<Rule>;
pub type GroundLiteral = Ground<Literal>;
pub type GroundAtom = Ground<Atom>;
pub struct GroundProgram {
    pub facts: Vec<GroundAtom>,
    pub rules: Vec<GroundRule>,
}
// ============================================================================
// Display
// ============================================================================

impl<T: fmt::Display> fmt::Display for Ground<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "⊥{:#}", &self.0)
        } else {
            self.0.fmt(f)
        }
    }
}
