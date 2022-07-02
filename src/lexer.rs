//! Make our own lexer to handle quoted strings more conveniently.
use anyhow::{bail, format_err, Error};
use chrono::NaiveDate;
use once_cell::sync::Lazy;
use regex::Regex;
use std::{borrow::Cow, fmt, str::FromStr};

type Spanned<'i> = Result<(usize, Tok<'i>, usize), Error>;

static NUMBER: Lazy<Regex> = Lazy::new(|| Regex::new(r"^[+-]?[0-9]*\.?[0-9]+").unwrap());
static DATE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^\d{4}-\d{2}-\d{2}").unwrap());

macro_rules! literal {
    ($this:expr, $lit:expr, $tok:expr) => {
        if let Some(itm) = $this.try_literal($lit, $tok) {
            return Some(Ok(itm));
        }
    };
}

#[derive(Debug, Clone, PartialEq)]
pub enum Tok<'i> {
    /// (
    LCurly,
    /// )
    RCurly,
    And,
    Or,
    IsSome,
    IsNone,
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
    Like,
    Regex,
    String(Cow<'i, str>),
    /// Defer parsing till later.
    Integer(i32),
    Float(f64),
    Date(NaiveDate),
}

impl fmt::Display for Tok<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Tok::LCurly => write!(f, "("),
            Tok::RCurly => write!(f, ")"),
            Tok::And => write!(f, "and"),
            Tok::Or => write!(f, "or"),
            Tok::IsSome => write!(f, "is some"),
            Tok::IsNone => write!(f, "is none"),
            Tok::Eq => write!(f, "=="),
            Tok::Neq => write!(f, "!="),
            Tok::Lt => write!(f, "<"),
            Tok::Leq => write!(f, "<="),
            Tok::Gt => write!(f, ">"),
            Tok::Geq => write!(f, ">="),
            Tok::Like => write!(f, "like"),
            Tok::Regex => write!(f, "matches"),
            Tok::String(s) => write!(f, "{:?}", s),
            Tok::Integer(i) => write!(f, "{}", i),
            Tok::Float(float) => write!(f, "{}", float),
            Tok::Date(d) => write!(f, "{}", d),
        }
    }
}

pub struct Lexer<'i> {
    input: &'i str,
    idx: usize,
}

impl<'i> Lexer<'i> {
    pub fn new(input: &'i str) -> Self {
        Self { input, idx: 0 }
    }

    fn consume(&mut self, amt: usize) {
        self.input = &self.input[amt..];
        self.idx += amt;
    }

    fn consume_whitespace(&mut self) {
        while let Some(ch) = self.input.chars().next() {
            if ch.is_whitespace() {
                self.consume(ch.len_utf8());
            } else {
                break;
            }
        }
    }

    fn parse_regex(&mut self, regex: &Regex) -> Option<(usize, &'i str, usize)> {
        assert!(regex.as_str().starts_with("^"));
        if let Some(mat) = regex.find(&self.input) {
            assert!(mat.start() == 0);
            let len = mat.end();
            let span = self.span(len);
            let tok = &self.input[..len];
            self.consume(len);
            Some((span.0, tok, span.1))
        } else {
            None
        }
    }

    fn parse_string(&mut self) -> Option<Spanned<'i>> {
        if self.starts_with(&["'"]) {
            self.consume(1);
            Some(self.parse_delim('\''))
        } else if self.starts_with(&["\""]) {
            self.consume(1);
            Some(self.parse_delim('"'))
        } else {
            // consume until next whitespace
            let end_idx = match self.input.find(|ch: char| ch.is_whitespace()) {
                Some(idx) => idx,
                // return rest of string
                None => self.input.len(),
            };
            let span = self.span(end_idx);
            let s = &self.input[..end_idx];
            if s.contains('\'') || s.contains('"') {
                Some(Err(format_err!("invalid string")))
            } else {
                self.consume(end_idx);
                Some(Ok((span.0, Tok::String(Cow::Borrowed(s)), span.1)))
            }
        }
    }

    /// Helper for parse_string that handles escaping (`\'` => `'`, `\"` => `"`, `\\` => `\`)
    fn parse_delim(&mut self, end: char) -> Spanned<'i> {
        let end_idx = match self.input.find(end) {
            Some(end) => end,
            None => bail!("unclosed `{}`", end),
        };

        // first try zero-copy
        // look for escape character and branch
        let input = &self.input[..end_idx];
        if !input.contains('\\') {
            let span = self.span(end_idx);
            self.consume(end_idx + 1);
            return Ok((span.0, Tok::String(Cow::Borrowed(input)), span.1));
        }

        // we need to build the string manually
        let mut output = String::with_capacity(input.len());
        let mut chars = self.input.char_indices();
        while let Some((idx, ch)) = chars.next() {
            if ch == '\\' {
                match chars.next() {
                    Some((_, ch @ '\\' | ch @ '"' | ch @ '\'')) => output.push(ch),
                    Some((_, ch)) => bail!("expected escape sequence, found `\\{}`", ch),
                    None => bail!("string ended with single `\\`"),
                }
            } else if ch == end {
                let span = self.span(idx);
                self.consume(idx + 1);
                return Ok((span.0, Tok::String(Cow::Owned(output)), span.1));
            } else {
                output.push(ch);
            }
        }
        bail!("unclosed `{}`", end);
    }

    fn starts_with(&self, test: &[&str]) -> bool {
        test.iter()
            .copied()
            .any(|test| self.input.starts_with(test))
    }

    /// If the string starts with the given literal, then return its length and
    /// consume it.
    fn try_literal(
        &mut self,
        lit: &str,
        tok: Tok<'static>,
    ) -> Option<(usize, Tok<'static>, usize)> {
        if self.starts_with(&[lit]) {
            let span = self.span(lit.len());
            self.consume(lit.len());
            Some((span.0, tok, span.1))
        } else {
            None
        }
    }

    fn span(&self, len: usize) -> (usize, usize) {
        (self.idx, self.idx + len)
    }
}

impl<'i> Iterator for Lexer<'i> {
    type Item = Spanned<'i>;
    fn next(&mut self) -> Option<Self::Item> {
        self.consume_whitespace();
        if self.input.is_empty() {
            return None;
        }
        literal!(self, "(", Tok::LCurly);
        literal!(self, ")", Tok::RCurly);
        literal!(self, "and", Tok::And);
        literal!(self, "or", Tok::Or);
        literal!(self, "is some", Tok::IsSome);
        literal!(self, "is none", Tok::IsNone);
        literal!(self, "==", Tok::Eq);
        literal!(self, "=", Tok::Eq);
        literal!(self, "!=", Tok::Neq);
        literal!(self, ">", Tok::Gt);
        literal!(self, ">=", Tok::Geq);
        literal!(self, "<", Tok::Lt);
        literal!(self, "<=", Tok::Leq);
        literal!(self, "like", Tok::Like);
        literal!(self, "matches", Tok::Regex);
        if let Some((start, s, end)) = self.parse_regex(&DATE) {
            return Some(match NaiveDate::from_str(s).map_err(Error::from) {
                Ok(date) => Ok((start, Tok::Date(date), end)),
                Err(e) => Err(e),
            });
        }
        if let Some((start, s, end)) = self.parse_regex(&NUMBER) {
            let tok = if s.contains(".") {
                f64::from_str(s).map(Tok::Float).map_err(Error::from)
            } else {
                i32::from_str(s).map(Tok::Integer).map_err(Error::from)
            };
            return match tok {
                Ok(tok) => Some(Ok((start, tok, end))),
                Err(e) => Some(Err(e)),
            };
        }
        if let Some(span) = self.parse_string() {
            return Some(span);
        }
        Some(Err(format_err!("unexpected character")))
    }
}

#[cfg(test)]
fn test_helper(input: &str, output: Vec<Tok>) {
    assert_eq!(
        Lexer::new(input)
            .map(Result::unwrap)
            .map(|(_, tok, _)| tok)
            .collect::<Vec<_>>(),
        output
    );
}

#[test]
fn text_lexer() {
    test_helper("", vec![]);
    test_helper("2 < 3", vec![Tok::Integer(2), Tok::Lt, Tok::Integer(3)]);
    test_helper(
        "(test < 2 or up > 2020-01-11) and style < 3",
        vec![
            Tok::LCurly,
            Tok::String(Cow::from("test")),
            Tok::Lt,
            Tok::Integer(2),
            Tok::Or,
            Tok::String(Cow::Borrowed("up")),
            Tok::Gt,
            Tok::Date("2020-01-11".parse().unwrap()),
            Tok::RCurly,
            Tok::And,
            Tok::String(Cow::Borrowed("style")),
            Tok::Lt,
            Tok::Integer(3),
        ],
    );
}
