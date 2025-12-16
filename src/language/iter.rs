use crate::language::glossary::*;

/// Represents any element/node from rules
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Element<'a> {
    Atom(&'a Atom),
    Literal(&'a Literal),
    Term(&'a Term),
}

pub struct RuleIter<'a> {
    // stack for DFS recursion
    stack: Vec<Element<'a>>,
}

impl<'a> RuleIter<'a> {
    pub fn new(rule: &'a Rule) -> Self {
        let mut stack = Vec::new();

        for literal in rule.body.iter().rev() {
            stack.push(Element::Literal(literal));
        }
        stack.push(Element::Atom(&rule.head));
        RuleIter { stack }
    }
}

impl<'a> Iterator for RuleIter<'a> {
    type Item = Element<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let node = self.stack.pop()?;
        // Push children onto the stack for next
        match node {
            Element::Atom(atom) => {
                for term in atom.terms.iter().rev() {
                    self.stack.push(Element::Term(term));
                }
            }
            Element::Literal(literal) => self.stack.push(Element::Atom(literal.atom())),
            Element::Term(term) => {
                if let Term::Compound(_, terms) = term {
                    for t in terms.iter().rev() {
                        self.stack.push(Element::Term(t))
                    }
                }
            }
        }
        Some(node)
    }
}

#[cfg(test)]
mod tests {
    use crate::language::{glossary::Atom, iter::Element, parse::parse_rule};

    #[test]
    fn test_term_iteration() {
        let rule = parse_rule("first(one) :- not second(two(three), fourth), third(fifth).")
            .unwrap()
            .unwrap_left();

        assert!(matches!(
                rule.iter().next().unwrap(),
                Element::Atom(atom) if atom.predicate == "first"
        ));
    }
}
