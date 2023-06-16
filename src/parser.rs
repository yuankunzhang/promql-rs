use crate::ast::*;
use crate::functions::*;
use pest::pratt_parser::{Assoc::*, Op, PrattParser};
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use std::fmt::Display;
use std::str::FromStr;
use std::time::Duration;

#[derive(Parser)]
#[grammar = "promql.pest"]
struct PromQLParser;

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use Rule::*;

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

pub fn parse(promql: &str) -> Result<Expr, ParseError> {
    let mut pairs = PromQLParser::parse(Rule::promql, promql)?;
    Ok(parse_expr(pairs.next().unwrap())?)
}

fn parse_expr(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    let parse_primary = |primary: Pair<Rule>| match primary.as_rule() {
        Rule::expr => parse_expr(primary),
        Rule::paren_expr => parse_paren_expr(primary),
        Rule::aggregate_expr => parse_aggregate_expr(primary),
        Rule::function_call => parse_function_call(primary),
        Rule::modifier_expr => parse_modifier_expr(primary),
        Rule::subquery_expr => parse_subquery_expr(primary, OffsetModifier::None, AtModifier::None),
        Rule::matrix_selector => {
            parse_matrix_selector(primary, OffsetModifier::None, AtModifier::None)
        }
        Rule::vector_selector => {
            parse_vector_selector(primary, OffsetModifier::None, AtModifier::None)
        }
        Rule::string_literal => parse_string_literal(primary),
        Rule::number_literal => parse_number_literal(primary),
        _ => {
            println!("parse_primary: {:#?}", primary);
            unreachable!();
        }
    };

    let parse_prefix = |op: Pair<Rule>, rhs: Result<Expr, ParseError>| {
        let rhs = rhs.unwrap();
        match rhs {
            Expr::NumberLiteral(n) => match op.as_str() {
                "+" => return Ok(Expr::NumberLiteral(NumberLiteral { value: n.value })),
                "-" => return Ok(Expr::NumberLiteral(NumberLiteral { value: -n.value })),
                _ => Err(ParseError {
                    message: "invalid operator before number literal".to_string(),
                }),
            },
            _ => Ok(Expr::UnaryExpr(UnaryExpr {
                op: UnaryOp::from_str(op.as_str()).unwrap(),
                rhs: Box::new(rhs),
            })),
        }
    };

    let parse_infix =
        |lhs: Result<Expr, ParseError>, op: Pair<Rule>, rhs: Result<Expr, ParseError>| {
            let mut pairs = op.into_inner();
            let op = BinaryOp::from_str(pairs.next().unwrap().as_str()).unwrap();
            let return_bool = pairs.peek().is_some() && pairs.next().unwrap().as_str() == "bool";
            Ok(Expr::BinaryExpr(BinaryExpr {
                op,
                lhs: Box::new(lhs.unwrap()),
                rhs: Box::new(rhs.unwrap()),
                return_bool,
            }))
        };

    PRATT_PARSER
        .map_primary(parse_primary)
        .map_prefix(parse_prefix)
        .map_infix(parse_infix)
        .parse(pair.into_inner())
}

fn parse_paren_expr(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    Ok(Expr::ParenExpr(ParenExpr {
        expr: Box::new(parse_expr(pair.into_inner().next().unwrap())?),
    }))
}

fn parse_aggregate_expr(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    let mut pairs = pair.into_inner();
    let op = AggregateOp::from_str(pairs.next().unwrap().as_str()).unwrap();

    let mut modifier = match pairs.peek() {
        Some(p) if p.as_rule() == Rule::aggregate_modifier => {
            parse_aggregate_modifier(pairs.next().unwrap())?
        }
        _ => AggregateModifier::None,
    };

    let mut args = parse_function_body(pairs.next().unwrap())?;
    if args.len() != 1 {
        return Err(ParseError::new("wrong number of arguments".to_string()));
    }
    let expr = args
        .pop()
        .ok_or(ParseError::new("missing expression".to_string()))
        .unwrap();
    if expr.get_type() != ValueType::Vector {
        return Err(ParseError::new(format!(
            "wrong argument type: expected {:?}, got {:?}",
            ValueType::Vector,
            expr.get_type()
        )));
    }

    if let Some(rest) = pairs.next() {
        modifier = parse_aggregate_modifier(rest)?;
    }

    Ok(Expr::AggregateExpr(AggregateExpr {
        op,
        expr: Box::new(expr),
        modifier,
    }))
}

fn parse_aggregate_modifier(pair: Pair<Rule>) -> Result<AggregateModifier, ParseError> {
    let mut pairs = pair.into_inner();
    let m = pairs.next().unwrap();
    let labels: Vec<_> = pairs.map(|s| s.as_str().to_string()).collect();
    match m.as_str() {
        "by" => Ok(AggregateModifier::By(labels)),
        "without" => Ok(AggregateModifier::Without(labels)),
        _ => {
            return Err(ParseError::new(format!(
                "unknown aggregate modifier: {}",
                m.as_str()
            )))
        }
    }
}

fn parse_function_call(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    let mut pairs = pair.into_inner();
    let name = pairs.next().unwrap().as_str().to_string();
    let func = FUNCTIONS
        .iter()
        .find(|f| f.name == name)
        .ok_or("unknown function name")
        .unwrap();

    let args = parse_function_body(pairs.next().unwrap())?;
    if args.len() != func.arg_types.len() {
        return Err(ParseError::new("wrong number of arguments".to_string()));
    }
    for (arg, arg_type) in args.iter().zip(func.arg_types.iter()) {
        if arg_type != &arg.get_type() {
            return Err(ParseError::new(format!(
                "wrong argument type: expected {:?}, got {:?}",
                arg_type,
                arg.get_type()
            )));
        }
    }
    Ok(Expr::FunctionCall(FunctionCall { func, args }))
}

fn parse_function_body(pair: Pair<Rule>) -> Result<Vec<Expr>, ParseError> {
    pair.into_inner().map(|p| parse_expr(p)).collect()
}

fn parse_modifier_expr(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    let mut pairs = pair.into_inner();
    let pair = pairs.next().unwrap();
    let mut offset = OffsetModifier::None;
    let mut at = AtModifier::None;

    if pairs.peek().is_some() {
        let mut modifiers = pairs.next().unwrap().into_inner();
        while let Some(modifier) = modifiers.next() {
            match modifier.as_rule() {
                Rule::step_invariant_expr => at = parse_at_modifier(modifier),
                Rule::offset_expr => offset = parse_offset_modifier(modifier),
                _ => {}
            }
        }
    }

    match pair.as_rule() {
        Rule::subquery_expr => parse_subquery_expr(pair, offset, at),
        Rule::matrix_selector => parse_matrix_selector(pair, offset, at),
        _ => unreachable!("parse_modifier_expr: {:#?}", pair),
    }
}

fn parse_at_modifier(pair: Pair<Rule>) -> AtModifier {
    let p = pair.into_inner().next().unwrap();
    match p.as_rule() {
        Rule::at_modifier_preprocessor => match p.as_str() {
            "start" => AtModifier::Start,
            "end" => AtModifier::End,
            _ => AtModifier::None,
        },
        Rule::number => {
            let num = p.as_str().parse::<u64>().unwrap();
            AtModifier::Time(num)
        }
        _ => AtModifier::None,
    }
}

fn parse_offset_modifier(pair: Pair<Rule>) -> OffsetModifier {
    let duration = parse_duration(pair.into_inner().next().unwrap()).unwrap();
    OffsetModifier::Duration(duration)
}

fn parse_subquery_expr(
    pair: Pair<Rule>,
    offset: OffsetModifier,
    at: AtModifier,
) -> Result<Expr, ParseError> {
    let mut pairs = pair.into_inner();
    let vector_selector = parse_vector_selector(pairs.next().unwrap(), offset, at)?;

    let range = parse_duration(pairs.next().unwrap())?;
    let step = if pairs.peek().is_some() {
        parse_duration(pairs.next().unwrap())?
    } else {
        Duration::default()
    };

    Ok(Expr::SubqueryExpr(SubqueryExpr {
        expr: Box::new(vector_selector),
        range,
        step,
    }))
}

fn parse_matrix_selector(
    pair: Pair<Rule>,
    offset: OffsetModifier,
    at: AtModifier,
) -> Result<Expr, ParseError> {
    let mut pairs = pair.into_inner();
    let vector_selector = parse_vector_selector(pairs.next().unwrap(), offset, at)?;

    if let Some(p) = pairs.next() {
        if p.as_rule() == Rule::duration {
            let range = parse_duration(p)?;
            if let Expr::VectorSelector(vs) = vector_selector {
                return Ok(Expr::MatrixSelector(MatrixSelector {
                    vector_selector: vs,
                    range,
                }));
            }
        }
    }

    Ok(vector_selector)
}

fn parse_duration(pair: Pair<Rule>) -> Result<Duration, ParseError> {
    let mut duration = Duration::default();
    let mut num = String::new();
    let mut unit = String::new();

    let add_duration = |duration: &mut Duration,
                        num: &mut String,
                        unit: &mut String|
     -> Result<(), Box<dyn std::error::Error>> {
        let value = num.parse::<u64>().unwrap();
        match unit.as_str() {
            "y" => *duration += Duration::from_secs(value * 365 * 24 * 60 * 60),
            "w" => *duration += Duration::from_secs(value * 7 * 24 * 60 * 60),
            "d" => *duration += Duration::from_secs(value * 24 * 60 * 60),
            "h" => *duration += Duration::from_secs(value * 60 * 60),
            "m" => *duration += Duration::from_secs(value * 60),
            "s" => *duration += Duration::from_secs(value),
            "ms" => *duration += Duration::from_millis(value),
            _ => return Err("unknown duration unit".into()),
        }
        unit.clear();
        num.clear();
        Ok(())
    };

    for c in pair.as_str().chars() {
        if c.is_digit(10) {
            if !unit.is_empty() {
                add_duration(&mut duration, &mut num, &mut unit).unwrap();
            }
            num.push(c);
        } else {
            unit.push(c);
        }
    }

    if !unit.is_empty() {
        add_duration(&mut duration, &mut num, &mut unit).unwrap();
    }

    Ok(duration)
}

fn parse_vector_selector(
    pair: Pair<Rule>,
    offset: OffsetModifier,
    at: AtModifier,
) -> Result<Expr, ParseError> {
    let mut pairs = pair.into_inner();
    let mut metric = String::new();
    if let Rule::metric_identifier = pairs.peek().unwrap().as_rule() {
        metric = pairs.next().unwrap().as_str().to_string();
    }
    let label_matchers = if pairs.peek().is_some() {
        parse_label_matchers(pairs.next().unwrap())?
    } else {
        vec![]
    };
    Ok(Expr::VectorSelector(VectorSelector {
        metric,
        label_matchers,
        offset,
        at,
    }))
}

fn parse_label_matchers(pair: Pair<Rule>) -> Result<Vec<LabelMatcher>, ParseError> {
    pair.into_inner()
        .map(|p| {
            let mut pairs = p.into_inner();
            let name = pairs.next().unwrap().as_str().to_string();
            let op = MatchOp::from_str(pairs.next().unwrap().as_str()).unwrap();
            let value = pairs.next().unwrap().as_str().to_string();
            Ok(LabelMatcher { name, op, value })
        })
        .collect()
}

fn parse_string_literal(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    Ok(Expr::StringLiteral(StringLiteral {
        value: pair
            .into_inner()
            .next()
            .unwrap()
            .as_str()
            .trim_matches(&['"', '\'', '`'] as &[_])
            .to_string(),
    }))
}

fn parse_number_literal(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    let s = pair.into_inner().next().unwrap().as_str();
    match s {
        _ if s.starts_with("0x") => Ok(Expr::NumberLiteral(NumberLiteral {
            value: u64::from_str_radix(&s[2..], 16).unwrap() as f64,
        })),
        _ if s.starts_with("0") => Ok(Expr::NumberLiteral(NumberLiteral {
            value: u64::from_str_radix(&s[1..], 8).unwrap() as f64,
        })),
        _ => Ok(Expr::NumberLiteral(NumberLiteral {
            value: s.parse::<f64>().unwrap(),
        })),
    }
}
