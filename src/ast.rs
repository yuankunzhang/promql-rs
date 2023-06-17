use std::{str::FromStr, time::Duration};

use crate::functions::*;

#[derive(Debug)]
pub enum Expr {
    BinaryExpr(BinaryExpr),
    UnaryExpr(UnaryExpr),
    ParenExpr(ParenExpr),
    AggregateExpr(AggregateExpr),
    FunctionCall(FunctionCall),
    SubqueryExpr(SubqueryExpr),
    MatrixSelector(MatrixSelector),
    VectorSelector(VectorSelector),
    NumberLiteral(NumberLiteral),
    StringLiteral(StringLiteral),
}

impl Expr {
    pub fn get_type(&self) -> ValueType {
        match self {
            Expr::MatrixSelector(_) => ValueType::Matrix,
            Expr::VectorSelector(_) => ValueType::Vector,
            Expr::NumberLiteral(_) => ValueType::Scalar,
            Expr::StringLiteral(_) => ValueType::String,
            _ => ValueType::None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
    And,
    Or,
    Unless,
    Atan2,
}

impl FromStr for BinaryOp {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(BinaryOp::Add),
            "-" => Ok(BinaryOp::Sub),
            "*" => Ok(BinaryOp::Mul),
            "/" => Ok(BinaryOp::Div),
            "%" => Ok(BinaryOp::Mod),
            "^" => Ok(BinaryOp::Pow),
            "==" => Ok(BinaryOp::Eq),
            "!=" => Ok(BinaryOp::Ne),
            ">" => Ok(BinaryOp::Gt),
            "<" => Ok(BinaryOp::Lt),
            ">=" => Ok(BinaryOp::Ge),
            "<=" => Ok(BinaryOp::Le),
            "and" => Ok(BinaryOp::And),
            "or" => Ok(BinaryOp::Or),
            "unless" => Ok(BinaryOp::Unless),
            "atan2" => Ok(BinaryOp::Atan2),
            _ => Err(format!("Unknown binary operator: {}", s)),
        }
    }
}

#[derive(Debug)]
pub enum VectorModifier {
    None,
    On(Vec<String>),
    Ignoring(Vec<String>),
}

#[derive(Debug)]
pub enum GroupModifier {
    None,
    Left(Vec<String>),
    Right(Vec<String>),
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub return_bool: bool,
    pub vector_modifier: VectorModifier,
    pub group_modifier: GroupModifier,
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Pos,
    Neg,
}

impl FromStr for UnaryOp {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(UnaryOp::Pos),
            "-" => Ok(UnaryOp::Neg),
            _ => Err(format!("Unknown unary operator: {}", s)),
        }
    }
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct ParenExpr {
    pub expr: Box<Expr>,
}

#[derive(Debug)]
pub enum AggregateOp {
    Avg,
    Bottomk,
    Count,
    CountValues,
    Group,
    Max,
    Min,
    Stddev,
    Stdvar,
    Quantile,
    Sum,
    Topk,
}

impl FromStr for AggregateOp {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "avg" => Ok(AggregateOp::Avg),
            "bottomk" => Ok(AggregateOp::Bottomk),
            "count" => Ok(AggregateOp::Count),
            "count_values" => Ok(AggregateOp::CountValues),
            "group" => Ok(AggregateOp::Group),
            "max" => Ok(AggregateOp::Max),
            "min" => Ok(AggregateOp::Min),
            "stddev" => Ok(AggregateOp::Stddev),
            "stdvar" => Ok(AggregateOp::Stdvar),
            "quantile" => Ok(AggregateOp::Quantile),
            "sum" => Ok(AggregateOp::Sum),
            "topk" => Ok(AggregateOp::Topk),
            _ => Err(format!("Unknown aggregate operator: {}", s)),
        }
    }
}

#[derive(Debug)]
pub enum AggregateModifier {
    None,
    By(Vec<String>),
    Without(Vec<String>),
}

#[derive(Debug)]
pub struct AggregateExpr {
    pub op: AggregateOp,
    pub expr: Box<Expr>,
    pub modifier: AggregateModifier,
}

#[derive(Debug)]
pub enum AtModifier {
    None,
    Start,
    End,
    Time(u64),
}

#[derive(Debug)]
pub enum OffsetModifier {
    None,
    Duration(Duration),
}

#[derive(Debug)]
pub struct FunctionCall {
    pub func: &'static Function,
    pub args: Vec<Expr>,
}

#[derive(Debug)]
pub struct SubqueryExpr {
    pub expr: Box<Expr>,
    pub range: Duration,
    pub step: Duration,
}

#[derive(Debug)]
pub struct MatrixSelector {
    pub vector_selector: VectorSelector,
    pub range: Duration,
}

#[derive(Debug)]
pub struct VectorSelector {
    pub metric: String,
    pub label_matchers: Vec<LabelMatcher>,
    pub offset: OffsetModifier,
    pub at: AtModifier,
}

#[derive(Debug)]
pub enum MatchOp {
    Equal,
    NotEqual,
    Regex,
    NNotRegex,
}

impl FromStr for MatchOp {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "=" => Ok(MatchOp::Equal),
            "!=" => Ok(MatchOp::NotEqual),
            "=~" => Ok(MatchOp::Regex),
            "!~" => Ok(MatchOp::NNotRegex),
            _ => Err(format!("Unknown match operator: {}", s)),
        }
    }
}

#[derive(Debug)]
pub struct LabelMatcher {
    pub op: MatchOp,
    pub name: String,
    pub value: String,
}

#[derive(Debug)]
pub struct NumberLiteral {
    pub value: f64,
}

#[derive(Debug)]
pub struct StringLiteral {
    pub value: String,
}
