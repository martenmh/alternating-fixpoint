use std::ops::Range;

/// Match either a predicate or a character
// TODO: Should be changed to Pattern trait
pub trait Matcher {
    fn matches(&self, value: &str) -> bool;
    fn len(&self) -> usize;
}

impl Matcher for &str {
    fn matches(&self, str: &str) -> bool {
        (*self).starts_with(str)
    }
    fn len(&self) -> usize {
        (*self).len()
    }
}

impl Matcher for char {
    fn matches(&self, str: &str) -> bool {
        str.starts_with(*self)
    }
    fn len(&self) -> usize {
        1
    }
}

impl<F> Matcher for F
where
    F: Fn(char) -> bool,
{
    fn matches(&self, value: &str) -> bool {
        value.chars().next().map_or(false, |c| self(c))
    }
    fn len(&self) -> usize {
        1
    }
}

pub type Position = usize;
pub type Span = (usize, Position);

#[derive(Debug)]
pub enum CursorError {
    UnexpectedChar {
        unexpected: char,
        position: usize,
    },
    ExpectedChar {
        expected: char,
        found: Option<char>,
        position: usize,
    },
    UnexpectedEof {
        position: usize,
    },
}

type Result<T> = std::result::Result<T, CursorError>;

/// A string reader, providing peeking, consumption and value testing
#[derive(Debug, Clone, Copy)]
pub struct Cursor<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Self {
        Cursor { input, pos: 0 }
    }

    pub fn position(&self) -> usize {
        self.pos
    }

    pub fn get_line(&self) -> usize {
        self.input[..self.pos]
            .chars()
            .rfind(|c| *c == '\n')
            .iter()
            .count()
            + 1
    }

    pub fn get_column(&self) -> usize {
        let line_start = self.input[..self.pos].rfind('\n').unwrap_or(0);
        self.pos - line_start
    }

    pub fn rest(&self) -> &str {
        &self.input[self.pos..]
    }

    pub fn peek_n(&self, n: usize) -> &str {
        &self.input[self.pos..self.pos + n]
    }

    // peeky (from behind the blinders)
    pub fn peek(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    pub fn get(&self, range: Range<usize>) -> Option<&str> {
        self.input.get(range)
    }

    pub fn get_char(&self) -> Result<char> {
        self.peek()
            .ok_or(CursorError::UnexpectedEof { position: self.pos })
    }

    // en garde
    pub fn advance(&mut self, n: usize) {
        self.pos = (self.pos + n).min(self.input.len());
    }

    pub fn check_eof(&self) -> bool {
        !self.check_after_whitespace(|c: char| true) // there are still non-whitespace characters left
    }

    /// Returns false if there are no more characters after whitespace
    pub fn check_after_whitespace<M: Matcher>(&self, expected: M) -> bool {
        let mut peeky_cursor = self.clone();
        peeky_cursor
            .skip_whitespace()
            .get(peeky_cursor.position()..peeky_cursor.position() + expected.len())
            .map_or(false, |s| expected.matches(s))
    }

    pub fn check(&self, expected: &str) -> bool {
        self.input[self.pos..].starts_with(expected)
    }

    pub fn expect(&mut self, expected: char) -> Result<()> {
        if self.consume_if(expected) {
            return Ok(());
        }
        Err(CursorError::ExpectedChar {
            expected,
            found: self.peek(),
            position: self.pos,
        })
    }

    // like pac-man, only eat snacks
    pub fn consume_while<M: Matcher>(&mut self, matcher: M) -> usize {
        let count = self
            .rest()
            .chars()
            .take_while(|c| matcher.matches(c.to_string().as_str()))
            .map(|ch| ch.len_utf8())
            .sum();
        self.pos += count;
        count
    }

    // like pac-man, avoid ghosts
    pub fn consume_until<M: Matcher>(&mut self, matcher: M) -> usize {
        self.consume_while(|c: char| !matcher.matches(c.to_string().as_str()))
    }

    pub fn skip_whitespace(&mut self) -> Self {
        loop {
            self.consume_while(char::is_whitespace);

            // Skip comment lines starting with %
            if self.consume_if(|c| c == '%') {
                self.consume_until(|ch| ch == '\n');
            } else {
                break;
            }
        }
        *self
    }

    pub fn consume_if<M: Matcher>(&mut self, matcher: M) -> bool {
        if self.pos + matcher.len() > self.input.len() {
            return false;
        }

        let peeky = self.peek_n(matcher.len());
        if !peeky.is_empty() && matcher.matches(peeky) {
            self.advance(matcher.len());
            true
        } else {
            false
        }
    }

    fn pretty_print_input(&self, range: Span) {
        let (start, end) = range;
        let begin_line = self.input[..start].rfind('\n').unwrap_or(0);
        let end_line = self.input[end..].find('\n').unwrap_or(self.input.len());

        if self.input.contains('\n') {
            let line = 0;
            let column = 0;
            println!("  {}:{}", line, column);
        }

        println!("  {}", &self.input[begin_line..end_line]);
        println!("  {}{}", " ".repeat(start), "^".repeat(end - start));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_testing() {
        let input = "
                % a comment that should be considered whitespace

                find_me!
            ";
        let cursor = Cursor::new(input);
        assert!(cursor.check_after_whitespace("find_me!"));
    }

    #[test]
    fn test_string_consumption() {
        let input = "
            % this is a comment
            my123test(input ) .";
        let mut cursor = Cursor::new(input);
        assert_eq!(input, cursor.rest());
        assert_eq!(cursor.peek(), Some('\n'));
        cursor.skip_whitespace(); // skip newline and comment

        assert_eq!(cursor.peek(), Some('m'));
        assert!(cursor.check("m"));
        assert!(cursor.consume_if('m'));
        assert!(cursor.consume_if('y'));
        assert_eq!(3, cursor.consume_until(char::is_alphabetic));
        assert!(cursor.consume_if("test"));
        assert!(cursor.check("("));
    }

    #[test]
    fn test_print_input() {
        let input = "parent(john, mary).";
        let cursor = Cursor::new(input);

        cursor.pretty_print_input((0, 6));
    }
}
