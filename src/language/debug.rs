enum Element {
    Term(Term),
    Rule(Rule),
    Literal(Literal),
    Atom(Atom),
}

struct ElementOccurrence {
    element: Element,
    line: Span,
    column: Span,
}

struct Diagnostics {
    occurrences: Vec<ElementOccurrence>,
    element: Element,
}

// Since we want a minimal performant representation during inference,
// we have to reverse engineer debug information once something goes wrong
/// Return occurrences of element inside the source input
/// Reparses the input
fn get_element_occurrences(element: Element) -> Vec<ElementOccurrence> {

}

fn
