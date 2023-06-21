use crate::ast::*;
use crate::function::FUNCTIONS;
use pest::iterators::Pair;
use pest::pratt_parser::{Assoc::*, Op, PrattParser};
use pest::Parser;
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

        // Operator precedence, from highest to lowest:
        // 1. ^
        // 2. - +         (prefix)
        // 3. [] [:]      (postfix)
        // 4. offset at   (postfix)
        // 5. * / % atan2
        // 6. - +
        // 7. == != <= < >= >
        // 8. and unless
        // 9. or
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
            .op(Op::postfix(offset) | Op::postfix(at))
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

impl From<&str> for ParseError {
    fn from(e: &str) -> Self {
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
        Rule::aggregate_expr => parse_aggregate_expr(primary),
        Rule::function_call => parse_function_call(primary),
        Rule::number_literal => parse_number_literal(primary),
        Rule::paren_expr => parse_paren_expr(primary),
        Rule::string_literal => parse_string_literal(primary),
        Rule::vector_selector => parse_vector_selector(primary),
        _ => unreachable!(),
    };

    let parse_postfix = |lhs: Result<Expr, ParseError>, op: Pair<Rule>| parse_postfix(lhs, op);
    let parse_prefix = |op, rhs| parse_prefix(op, rhs);
    let parse_infix = |lhs, op, rhs| parse_infix(lhs, op, rhs);

    PRATT_PARSER
        .map_primary(parse_primary)
        .map_postfix(parse_postfix)
        .map_prefix(parse_prefix)
        .map_infix(parse_infix)
        .parse(pair.into_inner())
}

fn parse_postfix(lhs: Result<Expr, ParseError>, op: Pair<Rule>) -> Result<Expr, ParseError> {
    let mut expr = lhs?;

    match op.as_rule() {
        Rule::matrix => {
            let mut pairs = op.into_inner();
            let range = parse_duration(pairs.next().unwrap())?;
            Ok(Expr::MatrixSelector(MatrixSelector {
                vector_selector: Box::new(expr),
                range,
            }))
        }
        Rule::subquery => {
            let mut pairs = op.into_inner();
            let range = parse_duration(pairs.next().unwrap())?;
            let step = parse_duration(pairs.next().unwrap())?;
            Ok(Expr::SubqueryExpr(SubqueryExpr {
                expr: Box::new(expr),
                at: AtModifier::None,
                offset: Duration::default(),
                range,
                step,
            }))
        }
        Rule::offset => {
            let offset = parse_duration(op.into_inner().next().unwrap())?;
            match expr {
                Expr::VectorSelector(ref mut vs) => {
                    vs.offset = offset;
                }
                Expr::MatrixSelector(ref mut ms) => match ms.vector_selector.as_mut() {
                    Expr::VectorSelector(ref mut vs) => {
                        vs.offset = offset;
                    }
                    _ => return Err("invalid matrix selector".into()),
                },
                Expr::SubqueryExpr(ref mut sq) => {
                    sq.offset = offset;
                }
                _ => return Err("offset modifier must be preceded by an instant vector selector, a range vector selector, or a subquery".into()),
            }
            Ok(expr)
        }
        Rule::at => {
            let at_modifier = parse_at_modifier(op.into_inner().next().unwrap())?;
            match expr {
                Expr::VectorSelector(ref mut vs) => {
                    vs.at = at_modifier;
                }
                Expr::MatrixSelector(ref mut ms) => match ms.vector_selector.as_mut() {
                    Expr::VectorSelector(ref mut vs) => {
                        vs.at = at_modifier;
                    }
                    _ => return Err("invalid matrix selector".into()),
                },
                Expr::SubqueryExpr(ref mut sq) => {
                    sq.at = at_modifier;
                }
                _ => return Err("@ modifier must be preceded by an instant vector selector, a range vector selector, or a subquery".into()),
            }
            Ok(expr)
        }
        _ => unreachable!(),
    }
}

fn parse_prefix(op: Pair<Rule>, rhs: Result<Expr, ParseError>) -> Result<Expr, ParseError> {
    match rhs? {
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

fn parse_infix(
    lhs: Result<Expr, ParseError>,
    op: Pair<Rule>,
    rhs: Result<Expr, ParseError>,
) -> Result<Expr, ParseError> {
    let mut pairs = op.into_inner();
    let op = BinaryOp::from_str(pairs.next().unwrap().as_str())?;
    let mut return_bool = false;
    let mut vector_matching: VectorMatching = VectorMatching::default();

    if let Some(pair) = pairs.next() {
        let mut pairs = pair.into_inner();

        if let Rule::bool_modifier = pairs.peek().unwrap().as_rule() {
            return_bool = true;
            pairs.next();
        }

        while let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::on_or_ignoring_modifier => {
                    vector_matching.grouping = parse_on_or_ignoring_modifier(pair)?
                }
                Rule::left_or_right_modifier => {
                    vector_matching.cardinality = parse_left_or_right_modifier(pair)?
                }
                _ => unreachable!(),
            }
        }
    }

    Ok(Expr::BinaryExpr(BinaryExpr {
        op,
        lhs: Box::new(lhs?),
        rhs: Box::new(rhs?),
        return_bool,
        vector_matching,
    }))
}

fn parse_at_modifier(pair: Pair<Rule>) -> Result<AtModifier, ParseError> {
    let value = pair.as_str();
    match value {
        "start()" => Ok(AtModifier::Start),
        "end()" => Ok(AtModifier::End),
        _ => Ok(AtModifier::Timestamp(value.parse::<u64>().unwrap())),
    }
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

fn parse_on_or_ignoring_modifier(pair: Pair<Rule>) -> Result<VectorMatchGrouping, ParseError> {
    let mut pairs = pair.into_inner();
    let modifier = pairs.next().unwrap();
    let labels: Vec<_> = pairs.map(|s| s.as_str().to_string()).collect();
    match modifier.as_str() {
        "on" => Ok(VectorMatchGrouping::On(labels)),
        "ignoring" => Ok(VectorMatchGrouping::Ignoring(labels)),
        _ => Ok(VectorMatchGrouping::None),
    }
}

fn parse_left_or_right_modifier(pair: Pair<Rule>) -> Result<VectorMatchCardinality, ParseError> {
    let modifier = pair.into_inner().next().unwrap();
    match modifier.as_str() {
        "group_left" => Ok(VectorMatchCardinality::OneToMany),
        "group_right" => Ok(VectorMatchCardinality::ManyToOne),
        _ => unreachable!(),
    }
}

fn parse_aggregate_expr(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    let mut pairs = pair.into_inner();
    let op = AggregateOp::from_str(pairs.next().unwrap().as_str())?;

    let mut modifier = match pairs.peek() {
        Some(pair) if pair.as_rule() == Rule::aggregate_modifier => {
            parse_aggregate_modifier(pairs.next().unwrap())?
        }
        _ => AggregateModifier::None,
    };

    let mut args = parse_function_args(pairs.next().unwrap())?;
    let expr = args.pop().ok_or("missing expression".to_string())?;

    if let Some(pair) = pairs.next() {
        modifier = parse_aggregate_modifier(pair)?;
    }

    Ok(Expr::AggregateExpr(AggregateExpr {
        op,
        expr: Box::new(expr),
        param: args.pop().map(Box::new),
        modifier,
    }))
}

fn parse_aggregate_modifier(pair: Pair<Rule>) -> Result<AggregateModifier, ParseError> {
    let mut pairs = pair.into_inner();
    let modifier = pairs.next().unwrap();
    let labels: Vec<_> = pairs.map(|s| s.as_str().to_string()).collect();
    match modifier.as_str() {
        "by" => Ok(AggregateModifier::By(labels)),
        "without" => Ok(AggregateModifier::Without(labels)),
        _ => unreachable!(),
    }
}

fn parse_function_call(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    let mut pairs = pair.into_inner();
    let name = pairs.next().unwrap().as_str();
    let func = FUNCTIONS
        .iter()
        .find(|f| f.name == name)
        .ok_or("unknown function name")
        .unwrap();

    let args = parse_function_args(pairs.next().unwrap())?;
    // TODO: args validation
    Ok(Expr::FunctionCall(FunctionCall { func, args }))
}

fn parse_function_args(pair: Pair<Rule>) -> Result<Vec<Expr>, ParseError> {
    pair.into_inner().map(|expr| parse_expr(expr)).collect()
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

fn parse_vector_selector(pair: Pair<Rule>) -> Result<Expr, ParseError> {
    let mut pairs = pair.into_inner();

    let metric = match pairs.peek().unwrap().as_rule() {
        Rule::metric => pairs.next().unwrap().as_str().to_string(),
        _ => String::new(),
    };

    let label_matchers = match pairs.next() {
        Some(pair) => pair
            .into_inner()
            .map(|pair| {
                let mut pairs = pair.into_inner();
                let name = pairs.next().unwrap().as_str().to_string();
                let op = MatchOp::from_str(pairs.next().unwrap().as_str()).unwrap();
                let value = pairs.next().unwrap().as_str().to_string();
                LabelMatcher { op, name, value }
            })
            .collect(),
        None => vec![],
    };

    Ok(Expr::VectorSelector(VectorSelector {
        metric,
        label_matchers,
        original_offset: Duration::default(),
        offset: Duration::default(),
        at: AtModifier::None,
    }))
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
