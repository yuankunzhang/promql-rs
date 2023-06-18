use std::{str::FromStr, time::Duration};

use crate::function::*;

#[derive(Debug)]
pub enum Expr {
    AggregateExpr(AggregateExpr),
    BinaryExpr(BinaryExpr),
    FunctionCall(FunctionCall),
    MatrixSelector(MatrixSelector),
    SubqueryExpr(SubqueryExpr),
    NumberLiteral(NumberLiteral),
    ParenExpr(ParenExpr),
    StringLiteral(StringLiteral),
    UnaryExpr(UnaryExpr),
    VectorSelector(VectorSelector),
}

impl Expr {
    pub fn get_type(&self) -> ValueType {
        match self {
            Expr::AggregateExpr(_) => ValueType::Vector,
            Expr::FunctionCall(call) => call.func.return_type,
            Expr::MatrixSelector(_) => ValueType::Matrix,
            Expr::SubqueryExpr(_) => ValueType::Matrix,
            Expr::NumberLiteral(_) => ValueType::Scalar,
            Expr::ParenExpr(p) => p.expr.get_type(),
            Expr::StringLiteral(_) => ValueType::String,
            Expr::UnaryExpr(u) => u.rhs.get_type(),
            Expr::VectorSelector(_) => ValueType::Vector,
            Expr::BinaryExpr(b) => {
                if b.lhs.get_type() == ValueType::Scalar && b.rhs.get_type() == ValueType::Scalar {
                    ValueType::Scalar
                } else {
                    ValueType::Vector
                }
            }
        }
    }
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum AggregateModifier {
    None,
    By(Vec<String>),
    Without(Vec<String>),
}

/// AggregateExpr represents an aggregation operation over a vector.
#[derive(Debug)]
pub struct AggregateExpr {
    pub op: AggregateOp,
    pub expr: Box<Expr>,
    pub param: Option<Box<Expr>>,
    pub modifier: AggregateModifier,
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
pub enum VectorMatchCardinality {
    OneToOne,
    OneToMany,
    ManyToOne,
    ManyToMany,
}

#[derive(Debug)]
pub enum VectorMatchGrouping {
    None,
    On(Vec<String>),
    Ignoring(Vec<String>),
}

#[derive(Debug)]
pub struct VectorMatching {
    pub cardinality: VectorMatchCardinality,
    pub grouping: VectorMatchGrouping,
}

/// BinaryExpr represents a binary operation between two expressions.
#[derive(Debug)]
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub return_bool: bool,
    pub vector_matching: VectorMatching,
}

/// FunctionCall represents a function call.
#[derive(Debug)]
pub struct FunctionCall {
    pub func: &'static Function,
    pub args: Vec<Expr>,
}

/// MatrixSelector represents a matrix selection.
#[derive(Debug)]
pub struct MatrixSelector {
    pub vector_selector: Box<Expr>,
    pub range: Duration,
}

#[derive(Debug)]
pub enum AtModifier {
    None,
    Start,
    End,
    Time(u64),
}

/// SubqueryExpr represents a subquery expression.
#[derive(Debug)]
pub struct SubqueryExpr {
    pub expr: Box<Expr>,
    pub range: Duration,
    pub step: Duration,
    pub offset: Duration,
    pub at: AtModifier,
}

/// Number literals can be written as literal integer (octal, decimal, or
/// hexadecimal) or floating-point numbers in the format:
///
/// [-+]?(
///       [0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?
///     | 0[xX][0-9a-fA-F]+
///     | [nN][aA][nN]
///     | [iI][nN][fF]
/// )
///
/// Examples:
///
///     23
///     -2.43
///     3.4e-9
///     0x8f
///     -Inf
///     NaN
#[derive(Debug)]
pub struct NumberLiteral {
    pub value: f64,
}

/// ParenExpr represents an expression wrapped in parentheses.
#[derive(Debug)]
pub struct ParenExpr {
    pub expr: Box<Expr>,
}

/// Strings may be specified as literals in single quotes or double quotes.
///
/// In single or double quotes a backslash begins an escape sequence, which may
/// be followed by a, b, f, n, r, t, v or \. Specific characters can be provided
/// using octal (\nnn) or hexadecimal (\xnn, \unnnn and \Unnnnnnnn).
///
/// Example:
///
///     "this is a string"
///     'these are unescaped: \n \\ \t'
#[derive(Debug)]
pub struct StringLiteral {
    pub value: String,
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

/// UnaryExpr represents a unary operation.
#[derive(Debug)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct LabelMatcher {
    pub op: MatchOp,
    pub name: String,
    pub value: String,
}

/// VectorSelector represents a vector selection.
#[derive(Debug)]
pub struct VectorSelector {
    pub metric: String,
    pub label_matchers: Vec<LabelMatcher>,
    pub original_offset: Duration,
    pub offset: Duration,
    pub at: AtModifier,
}
