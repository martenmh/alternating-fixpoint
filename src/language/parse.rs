use crate::{
    error::{Error, Result},
    language::cursor::*,
    language::glossary::*,
};
use either::*;

#[derive(Debug)]
pub enum ParseError {
    StringError { err: CursorError },
    InvalidIdentifier(char, Span),
    ExpectedIdentifier(usize),
    InvalidCompound(usize),
    InvalidAtom(Term, Span),
    UnexpectedEndOfRule,
    UnexpectedNegation,
}

// Parser converts cursor errors to parse errors
impl From<CursorError> for ParseError {
    fn from(err: CursorError) -> Self {
        ParseError::StringError { err }
    }
}

type ParseResult<T> = std::result::Result<T, ParseError>;

/// Curse of The Sand Parser
/// Turn lines into structured Rule objects
struct Parser<'a> {
    cursor: Cursor<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            cursor: Cursor::new(input),
        }
    }

    fn print_error(&self, err: ParseError) {
        match err {
            ParseError::StringError { err } => todo!(),
            ParseError::InvalidIdentifier(_, _) => todo!(),
            ParseError::ExpectedIdentifier(_) => todo!(),
            ParseError::InvalidCompound(_) => todo!(),
            ParseError::InvalidAtom(term, _) => todo!(),
            ParseError::UnexpectedNegation => todo!(),
            ParseError::UnexpectedEndOfRule => todo!(),
        }
    }

    // The bread and butter of the curse of the sand parser, parsing strings
    fn parse_identifier(&mut self) -> ParseResult<Identifier> {
        let start = self.cursor.position();
        if !self
            .cursor
            .consume_if(|c: char| c.is_alphabetic() || c == '-')
        {
            return Err(ParseError::InvalidIdentifier(
                self.cursor.get_char()?,
                (start, self.cursor.position()),
            ));
        }
        self.cursor
            .consume_while(|c: char| c.is_alphanumeric() || c == '_');

        Ok(Identifier::from(
            self.cursor
                .get(start..self.cursor.position())
                .unwrap()
                .to_owned(),
        ))
    }

    fn parse_compound_terms(&mut self) -> ParseResult<Vec<Term>> {
        self.cursor.expect('(')?;
        if self.cursor.check_after_whitespace(')') {
            return Err(ParseError::ExpectedIdentifier(self.cursor.position()));
        }

        // parse first identifier
        let mut compound_terms = vec![self.parse_term()?];
        while !self.cursor.consume_if(')') {
            self.cursor.expect(',')?;
            self.cursor.skip_whitespace();
            compound_terms.push(self.parse_term()?);
        }

        Ok(compound_terms)
    }

    fn parse_term(&mut self) -> ParseResult<Term> {
        self.cursor.skip_whitespace();
        let identifier = self.parse_identifier()?;

        if self.cursor.check("(") {
            let compound_terms = self.parse_compound_terms()?;
            return Ok(Term::compound(&identifier, compound_terms));
        }

        let is_variable = identifier.starts_with(|c: char| c.is_uppercase());
        if is_variable {
            Ok(Term::variable(identifier.as_str()))
        } else {
            Ok(Term::constant(identifier.as_str()))
        }
    }

    fn parse_atom(&mut self) -> ParseResult<Atom> {
        let start = self.cursor.position();
        let term = self.parse_term()?;
        match term {
            Term::Compound(identifier, terms) => Ok(Atom::from(&identifier, terms)),
            _ => Err(ParseError::InvalidAtom(
                term,
                (start, self.cursor.position()),
            )),
        }
    }

    fn parse_literal(&mut self) -> ParseResult<Literal> {
        self.cursor.skip_whitespace();
        if self.cursor.consume_if("not") {
            Ok(Literal::Negative(self.parse_atom()?))
        } else {
            Ok(Literal::Positive(self.parse_atom()?))
        }
    }

    pub fn parse_rule(&mut self) -> ParseResult<Either<Rule, Fact>> {
        let head = self.parse_atom()?;
        self.cursor.skip_whitespace();
        if self.cursor.consume_if(":-") {
            let mut body: Vec<Literal> = vec![self.parse_literal()?];
            while !self.cursor.consume_if('.') {
                self.cursor
                    .expect(',')
                    .map_err(|_| ParseError::UnexpectedEndOfRule)?;

                body.push(self.parse_literal()?);
                self.cursor.skip_whitespace();
            }
            return Ok(Left(Rule::from(head, body)));
        }

        self.cursor.expect('.')?;
        Ok(Right(Fact::from(&head.predicate, head.terms)))
    }
}

pub fn parse_program(input: &str) -> Result<Program> {
    let mut parser = Parser::new(input);
    let mut rules = Vec::new();
    let mut facts = Vec::new();
    while !parser.cursor.check_eof() {
        let rule_or_fact = parser.parse_rule().map_err(|e| Error::ParsingError {
            error: e,
            line: parser.cursor.get_line(),
            column: parser.cursor.get_column(),
        })?;
        rule_or_fact.either(|rule| rules.push(rule), |fact| facts.push(fact));
    }
    Ok(Program { rules, facts })
}

pub fn parse_rule(input: &str) -> Result<Either<Rule, Fact>> {
    let mut parser = Parser::new(input);

    let result = parser.parse_rule();
    match result {
        Ok(rule) => Ok(rule),
        Err(error) => Err(Error::ParsingError {
            error,
            line: parser.cursor.get_line(),
            column: parser.cursor.get_column(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_identifiers() {
        let identifiers = ["ident", "-ifier", "ident(", "ident stop", "MY_iDeNtifIer2"];
        let expected_parsed_identifiers = ["ident", "-ifier", "ident", "ident", "MY_iDeNtifIer2"];
        for (expectation, term) in expected_parsed_identifiers.iter().zip(identifiers.iter()) {
            let mut parser = Parser::new(term);
            let result = parser.parse_identifier();
            assert!(result.is_ok());
            assert_eq!(result.unwrap(), *expectation);
        }
    }

    #[test]
    fn test_parse_term() {
        let terms_input_expected = [
            ("X", Term::variable("X")),
            ("christopher", Term::constant("christopher")),
            ("Variable", Term::variable("Variable")),
            // Compound terms
            (
                "-fly(pengu)",
                Term::compound("-fly", vec![Term::constant("pengu")]),
            ),
            (
                "penguin(pengu)",
                Term::compound("penguin", vec![Term::constant("pengu")]),
            ),
            (
                "advisor(Y)",
                Term::compound("advisor", vec![Term::variable("Y")]),
            ),
            (
                "smart_people(X, tux, advisor(marten))  ",
                Term::compound(
                    "smart_people",
                    vec![
                        Term::variable("X"),
                        Term::constant("tux"),
                        Term::compound("advisor", vec![Term::constant("marten")]),
                    ],
                ),
            ),
        ];

        for (term, expectation) in terms_input_expected {
            let mut parser = Parser::new(term);
            let result = parser.parse_term().unwrap();
            assert_eq!(result, expectation);
        }
    }

    #[test]
    fn test_parse_atoms() {
        let atoms_input_expected = vec![(
            "pred(term1, term2)",
            Atom::from(
                "pred",
                vec![Term::constant("term1"), Term::constant("term2")],
            ),
        )];

        for (atom, expectation) in atoms_input_expected {
            let mut parser = Parser::new(atom);
            let result = parser.parse_atom().unwrap();
            assert_eq!(result, expectation);
        }
    }

    #[test]
    fn test_parse_literals() {
        let literals_input_expected = vec![
            (
                "not snek(serverus)",
                Literal::negative("snek", vec![Term::constant("serverus")]),
            ),
            (
                "lovely(little_lady)",
                Literal::positive("lovely", vec![Term::constant("little_lady")]),
            ),
        ];

        for (literal, expectation) in literals_input_expected {
            let mut parser = Parser::new(literal);
            let result = parser.parse_literal().unwrap();
            assert_eq!(result, expectation);
        }
    }

    #[test]
    fn test_parse_program_line() {
        let rule_input_expected = vec![
            // fly(X) :- bird(X), not -fly(X)
            (
                "fly(X) :- bird(X), not -fly(X).",
                Rule::from(
                    Atom::from("fly", vec![Term::variable("X")]),
                    vec![
                        Literal::positive("bird", vec![Term::variable("X")]),
                        Literal::negative("-fly", vec![Term::variable("X")]),
                    ],
                ),
            ),
        ];
        let fact_input_expected = vec![(
            "penguin(tux).",
            Fact::from("penguin", vec![Term::constant("tux")]),
        )];
        for (rule, expectation) in rule_input_expected {
            let result = parse_rule(rule).unwrap();
            assert_eq!(result.unwrap_left(), expectation);
        }
        for (fact, expectation) in fact_input_expected {
            let result = parse_rule(fact).unwrap();
            assert_eq!(result.unwrap_right(), expectation);
        }
    }

    #[test]
    fn test_parse_program() {
        let flying_bords = "
            % instance
            eagle(eddy).
            penguin(tux).

            % encoding
            fly(X) :- bird(X), not -fly(X)  .
            -fly(X) :- penguin(X).
            bird(X) :- penguin(X).
            bird(X) :- eagle(X).
        ";

        let program = parse_program(flying_bords).unwrap();
        assert_eq!(4, program.rules.len());
        assert_eq!(2, program.facts.len());
        assert_eq!("eagle", program.facts[0].predicate);
        assert_eq!(
            program.rules[0].body[1],
            Literal::negative("-fly", vec![Term::variable("X")])
        );
    }

    #[test]
    fn test_parse_fact() {
        let input = "parent(john, mary).";
        let result = parse_rule(input).unwrap();

        let result_fact: Fact = result.unwrap_right();

        let expectation = Fact::from(
            "parent",
            vec![Term::constant("john"), Term::constant("mary")],
        );
        assert_eq!(result_fact, expectation);
    }
}
