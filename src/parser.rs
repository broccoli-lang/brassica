use nom::{
    branch::*, bytes::complete::*, character::complete::*, combinator::*, multi::*,
    number::complete::*, sequence::*, IResult, Parser,
};
use nom_supreme::{
    error::ErrorTree, final_parser::final_parser, parser_ext::ParserExt, tag::complete::tag,
};

use Expression::*;

#[derive(Debug)]
pub enum Expression {
    Atom(String),
    Parens(Vec<Expression>),
    ScalarVar(String),
    ListVar(String),
    DictVar(String),
    Type(String),
    NumberLiteral(String),
    StringLiteral(String),
    ListLiteral(Vec<Expression>),
    DictLiteral(Vec<(Expression, Expression)>),
}

type Error<'a> = ErrorTree<&'a str>;
type ParseResult<'a, O> = IResult<&'a str, O, Error<'a>>;

fn identifier(input: &str) -> ParseResult<String> {
    map(
        many1(none_of(" \t\r\n()$@%!`'\"")).context("illegal identifier characters"),
        |v| v.iter().collect(),
    )(input)
}

fn atom(input: &str) -> ParseResult<Expression> {
    map(identifier, Atom)(input)
}

fn skip_spaces<'a, O>(
    parser: impl Parser<&'a str, O, Error<'a>>,
) -> impl Parser<&'a str, O, Error<'a>> {
    delimited(multispace0, parser, multispace0)
}

fn parenthesized<'a, O>(
    parser: impl Parser<&'a str, O, Error<'a>>,
) -> impl Parser<&'a str, O, Error<'a>> {
    delimited(skip_spaces(char('(')), parser, skip_spaces(char(')')))
}

fn paren_exps(input: &str) -> ParseResult<Vec<Expression>> {
    parenthesized(many0(skip_spaces(expression_impl))).parse(input)
}

fn sigil_id<'a>(
    c: char,
    ctor: impl Fn(String) -> Expression,
) -> impl Parser<&'a str, Expression, Error<'a>> {
    preceded(char(c), map(identifier, ctor))
}

fn number(input: &str) -> ParseResult<Expression> {
    map(
        alt((
            map(preceded(tag("0x"), hex_digit1), |s| "0x".to_string() + s),
            map(recognize_float, str::to_string),
        )),
        NumberLiteral,
    )(input)
}

fn string(input: &str) -> ParseResult<Expression> {
    map(
        // TODO: process other escape chars
        delimited(
            char('"'),
            escaped(is_not("\"\\"), '\\', one_of("\"\\")),
            char('"'),
        ),
        |s: &str| StringLiteral(s.to_string()),
    )(input)
}

fn list(input: &str) -> ParseResult<Expression> {
    map(preceded(char('\''), paren_exps), ListLiteral)(input)
}

fn dict(input: &str) -> ParseResult<Expression> {
    map(
        preceded(
            char('`'),
            parenthesized(many0(parenthesized(pair(
                skip_spaces(expression_impl),
                skip_spaces(expression_impl),
            )))),
        ),
        DictLiteral,
    )(input)
}

// TODO: better error messages

fn expression_impl(input: &str) -> ParseResult<Expression> {
    alt((
        map(paren_exps, Parens).context("parenthesized expression list"),
        sigil_id('$', ScalarVar).context("scalar variable"),
        sigil_id('@', ListVar).context("list variable"),
        sigil_id('%', DictVar).context("dict variable"),
        sigil_id('!', Type).context("type name"),
        list.context("list literal"),
        dict.context("dict literal"),
        string.context("string literal"),
        number.context("number literal"),
        atom.context("atom"),
    ))(input)
}

pub fn expression(input: &str) -> Result<Expression, Error> {
    final_parser(expression_impl)(input)
}
