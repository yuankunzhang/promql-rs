use crate::ast::*;
use pest::iterators::Pair;
use pest::pratt_parser::{Assoc::*, Op, PrattParser};
use pest::Parser;
use pest_derive::Parser;
use std::fmt::Display;
use std::str::FromStr;

#[derive(Parser)]
#[grammar = "promql.pest"]
struct PromQLParser;

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use Rule::*;

        // Operator precedence, from highest to lowest:
        // 1. ^
        // 2. - +         (prefix)
        // 3. [] [:]      (postfix)
        // 4. * / % atan2
        // 5. - +
        // 6. == != <= < >= >
        // 7. and unless
        // 8. or
        //
        // Operators on the same precedence level are left-associated, except
        // for ^ which is right-associated.
        PrattParser::new()
            .op(Op::infix(or, Left))
            .op(Op::infix(and, Left) | Op::infix(unless, Left))
            .op(Op::infix(eq, Left)
                | Op::infix(ne, Left)
                | Op::infix(le, Left)
                | Op::infix(lt, Left)
                | Op::infix(ge, Left)
                | Op::infix(gt, Left))
            .op(Op::infix(add, Left) | Op::infix(sub, Left))
            .op(Op::infix(mul, Left)
                | Op::infix(div, Left)
                | Op::infix(r#mod, Left)
                | Op::infix(atan2, Left))
            .op(Op::postfix(matrix) | Op::postfix(subquery))
            .op(Op::prefix(neg) | Op::prefix(pos))
            .op(Op::infix(pow, Right))
    };
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
}

impl ParseError {
    pub fn new(message: String) -> Self {
        ParseError { message }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.message.fmt(f)
    }
}

impl<T> From<pest::error::Error<T>> for ParseError
where
    T: pest::RuleType,
{
    fn from(e: pest::error::Error<T>) -> Self {
        ParseError {
            message: e.to_string(),
        }
    }
}

impl From<String> for ParseError {
    fn from(e: String) -> Self {
        ParseError { message: e }
    }
}

pub fn parse(promql: &str) -> Result<Expr, ParseError> {
    let mut pairs = PromQLParser::parse(Rule::promql, promql)?;
    Ok(parse_expr(pairs.next().unwrap())?)
}

fn parse_expr(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    let parse_primary = |primary: Pair<Rule>| match primary.as_rule() {
        Rule::number_literal => parse_number_literal(primary),
        Rule::paren_expr => parse_paren_expr(primary),
        Rule::string_literal => parse_string_literal(primary),
        _ => unreachable!(),
    };

    let parse_prefix = |op, rhs| parse_unary_expr(op, rhs);

    PRATT_PARSER
        .map_primary(parse_primary)
        .map_prefix(parse_prefix)
        .parse(pair.into_inner())
}

fn parse_number_literal(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    let s = pair.into_inner().next().unwrap().as_str();
    match s {
        _ if s.starts_with("0x") => Ok(Expr::NumberLiteral(NumberLiteral {
            value: u64::from_str_radix(&s[2..], 16).unwrap() as f64,
        })),
        _ if s.starts_with("0") && s.len() > 1 && s.as_bytes()[1] != b'.' => {
            Ok(Expr::NumberLiteral(NumberLiteral {
                value: u64::from_str_radix(&s[1..], 8).unwrap() as f64,
            }))
        }
        _ => Ok(Expr::NumberLiteral(NumberLiteral {
            value: s.parse::<f64>().unwrap(),
        })),
    }
}

fn parse_paren_expr(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    Ok(Expr::ParenExpr(ParenExpr {
        expr: Box::new(parse_expr(pair.into_inner().next().unwrap())?),
    }))
}

fn parse_string_literal(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    Ok(Expr::StringLiteral(StringLiteral {
        value: unescape(pair.into_inner().next().unwrap().as_str()),
    }))
}

fn parse_unary_expr(op: Pair<Rule>, rhs: Result<Expr, ParseError>) -> Result<Expr, ParseError> {
    match rhs.unwrap() {
        Expr::NumberLiteral(n) => match op.as_str() {
            "+" => return Ok(Expr::NumberLiteral(NumberLiteral { value: n.value })),
            "-" => return Ok(Expr::NumberLiteral(NumberLiteral { value: -n.value })),
            _ => unreachable!(),
        },
        expr => Ok(Expr::UnaryExpr(UnaryExpr {
            op: UnaryOp::from_str(op.as_str())?,
            rhs: Box::new(expr),
        })),
    }
}

fn unescape(s: &str) -> String {
    let mut string = String::new();
    let mut chars = s.chars().peekable();

    while let Some(current) = chars.next() {
        if current == '\\' && chars.peek().is_some() {
            string.push(match chars.next().unwrap() {
                '\\' => '\\',
                '"' => '"',
                '\'' => '\'',
                '/' => '/',
                'a' => char::from(0x07),
                'b' => char::from(0x08),
                'f' => char::from(0x0C),
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                'v' => char::from(0x0B),
                'x' => u8::from_str_radix(&collect_chars(&mut chars, 2), 16).unwrap() as char,
                'u' => {
                    char::from_u32(u32::from_str_radix(&collect_chars(&mut chars, 4), 16).unwrap())
                        .unwrap()
                }
                'U' => {
                    char::from_u32(u32::from_str_radix(&collect_chars(&mut chars, 8), 16).unwrap())
                        .unwrap()
                }
                d if d.is_digit(8) => char::from(
                    (d as u8 - 0x30) * 64
                        + u8::from_str_radix(&collect_chars(&mut chars, 2), 8).unwrap(),
                ),
                c => c,
            });
        } else {
            string.push(current);
        }
    }

    string
}

fn collect_chars(chars: &mut std::iter::Peekable<std::str::Chars>, n: usize) -> String {
    (0..n).filter_map(|_| chars.next()).collect()
}
