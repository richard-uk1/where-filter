use crate::lexer::Lexer;
use anyhow::{bail, format_err, Error, Result};
use chrono::NaiveDate;
use lalrpop_util::{lalrpop_mod, ParseError};
use regex::Regex;
use std::borrow::Cow;

lalrpop_mod!(where_filter);
mod lexer;

#[derive(Debug)]
pub enum Query<'input> {
    And(Vec<Query<'input>>),
    Or(Vec<Query<'input>>),
    Condition(Condition<'input>),
}

impl<'input> Query<'input> {
    pub fn parse(input: &'input str) -> Result<Self> {
        let lexer = Lexer::new(input);
        let parser = crate::where_filter::QueryParser::new();
        parser.parse(input, lexer).map_err(|e| format_err!("{}", e))
    }
    /// Reduce indirection where possible.
    fn normalize(self) -> Self {
        match self {
            Query::And(mut q) if q.len() == 1 => q.pop().unwrap(),
            Query::Or(mut q) if q.len() == 1 => q.pop().unwrap(),
            other => other,
        }
    }
}

#[derive(Debug)]
pub struct Condition<'input> {
    pub field: Cow<'input, str>,
    pub operation: Operation<'input>,
}

#[derive(Debug)]
pub enum Operation<'input> {
    IsNone,
    IsSome,
    Equal(Value<'input>),
    NotEqual(Value<'input>),
    GreaterThan(NumberOrDate),
    GreaterThanOrEqual(NumberOrDate),
    LessThan(NumberOrDate),
    LessThanOrEqual(NumberOrDate),
    Like(Cow<'input, str>),
    Regex(Regex),
}

#[derive(Debug)]
pub enum Value<'input> {
    String(Cow<'input, str>),
    Number(Number),
    Date(NaiveDate),
}

#[derive(Debug)]
pub enum NumberOrDate {
    Number(Number),
    Date(NaiveDate),
}

#[derive(Debug)]
pub enum Number {
    Integer(i32),
    Float(f64),
}

impl Number {
    pub fn as_i32(self) -> Result<i32> {
        match self {
            Number::Integer(v) => Ok(v),
            Number::Float(f) => {
                if (f as i32) as f64 == f {
                    Ok(f as i32)
                } else {
                    bail!("{} can't be safely converted to an integer", f)
                }
            }
        }
    }

    pub fn as_f64(self) -> f64 {
        match self {
            Number::Integer(v) => v.into(),
            Number::Float(f) => f,
        }
    }
}

fn convert_err<L, T, E: std::error::Error>(
    msg: &'static str,
) -> impl Fn(E) -> ParseError<L, T, Error> {
    move |e: E| ParseError::User {
        error: format_err!("{}: {}", msg, e),
    }
}
