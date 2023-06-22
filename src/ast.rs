use std::{str::FromStr, time::Duration};

use crate::function::*;

/// The enumaration of all expression types.
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
    /// Returns the [`ValueType`] that the expression evaluates to. Normally,
    /// an expression evaluates to a fixed value type. However, in the case of
    /// [`BinaryExpr`], the value type depends on the value types of its two
    /// operands. If both operands are scalars, the expression evaluates to a
    /// scalar. Otherwise, it evaluates to a vector.
    ///
    /// For more information, see
    /// <https://prometheus.io/docs/prometheus/latest/querying/basics/#expression-types>.
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

/// The enumaration of all value types.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValueType {
    /// A set of time series containing a single sample for each time series,
    /// all sharing the same timestamp.
    Vector,
    /// Also called a range vector, a set of time series containing a range of
    /// data points over time for each time series.
    Matrix,
    /// A simple numeric floating point value.
    Scalar,
    /// A simple string value.
    String,
}

/// Aggregation operators can be used to aggregate the elements of a single
/// vector, resulting in a new vector of fewer elements with aggregated values.
#[derive(Debug, PartialEq)]
pub enum AggregateOp {
    /// Calculate the average over dimensions.
    Avg,
    /// Smallest k elements by sample value.
    Bottomk,
    /// Count number of elements in the vector.
    Count,
    /// Count number of elements with the same value.
    CountValues,
    /// All values in the resulting vector are 1.
    Group,
    /// Select maximum over dimensions.
    Max,
    /// Select minimum over dimensions.
    Min,
    /// Calculate polulation standard deviation over dimensions.
    Stddev,
    /// Calculate population sstandard variance over dimensions.
    Stdvar,
    /// Calculate φ-quantile (0 ≤ φ ≤ 1) over dimensions.
    Quantile,
    /// Calculate sum over dimensions.
    Sum,
    /// Largest k elements by sample value.
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

/// The aggregation modifier can be used to indicate the aggregation operators
/// to use or exclude certain dimensions.
#[derive(Debug, PartialEq)]
pub enum AggregateModifier {
    None,
    /// The `by` modifier drops labels that are not listed.
    By(Vec<String>),
    /// The `without` modifier removes the listed labels from the result vector.
    /// All other labels are preserved.
    Without(Vec<String>),
}

impl Default for AggregateModifier {
    fn default() -> Self {
        AggregateModifier::None
    }
}

impl AggregateModifier {
    pub fn from_str_and_vec(s: &str, v: Vec<String>) -> Result<Self, String> {
        match s {
            "by" => Ok(AggregateModifier::By(v)),
            "without" => Ok(AggregateModifier::Without(v)),
            _ => Err(format!("Unknown aggregate modifier: {}", s)),
        }
    }
}

/// An aggregation expression.
#[derive(Debug)]
pub struct AggregateExpr {
    pub op: AggregateOp,
    pub expr: Box<Expr>,
    /// The `param` is only required for `count_values`, `quantile`, `topk`,
    /// and `bottomk`.
    pub param: Option<Box<Expr>>,
    pub modifier: AggregateModifier,
}

/// Binary operators can do logical and arithmetic operations or comparisons.
#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    /// Addition.
    Add,
    /// Subtraction.
    Sub,
    /// Multiplication.
    Mul,
    /// Division.
    Div,
    /// Modulo.
    Mod,
    /// Exponentiation.
    Pow,
    /// Equal.
    Eq,
    /// Not equal.
    Ne,
    /// Greater than.
    Gt,
    /// Less than.
    Lt,
    /// Greater than or equal.
    Ge,
    /// Less than or equal.
    Le,
    /// Logical and (intersection).
    And,
    /// Logical or (union).
    Or,
    /// Logical unless (complement).
    Unless,
    /// Arc tangent.
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

/// Describes the cardinality relashionship of two vectors in a binary
/// operation.
#[derive(Debug, PartialEq)]
pub enum VectorMatchCardinality {
    OneToOne,
    OneToMany,
    ManyToOne,
    ManyToMany,
}

impl Default for VectorMatchCardinality {
    fn default() -> Self {
        VectorMatchCardinality::OneToOne
    }
}

impl VectorMatchCardinality {
    pub fn from_str(s: &str) -> Result<Self, String> {
        match s {
            "group_right" => Ok(VectorMatchCardinality::OneToMany),
            "group_left" => Ok(VectorMatchCardinality::ManyToOne),
            _ => Err(format!("Unknown vector match cardinality: {}", s)),
        }
    }
}

/// Describes how two vectors in a binary operation are supposed to be grouped.
#[derive(Debug, PartialEq)]
pub enum VectorMatchGrouping {
    None,
    On(Vec<String>),
    Ignoring(Vec<String>),
}

impl Default for VectorMatchGrouping {
    fn default() -> Self {
        VectorMatchGrouping::None
    }
}

impl VectorMatchGrouping {
    pub fn from_str_and_vec(s: &str, v: Vec<String>) -> Result<Self, String> {
        match s {
            "on" => Ok(VectorMatchGrouping::On(v)),
            "ignoring" => Ok(VectorMatchGrouping::Ignoring(v)),
            _ => Err(format!("Unknown vector match grouping: {}", s)),
        }
    }
}

/// Describes how elements from two vectors in a binary operation are supposed
/// to be matched.
#[derive(Debug, PartialEq)]
pub struct VectorMatching {
    pub cardinality: VectorMatchCardinality,
    pub grouping: VectorMatchGrouping,
    pub include: Vec<String>,
}

impl Default for VectorMatching {
    fn default() -> Self {
        VectorMatching {
            cardinality: VectorMatchCardinality::default(),
            grouping: VectorMatchGrouping::default(),
            include: Vec::new(),
        }
    }
}

/// A binary expression.
#[derive(Debug)]
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    /// If a comparison operator, return `0` or `1` rather than filtering.
    pub return_bool: bool,
    /// The matching behavior for the oepration if both operands are vectors.
    pub vector_matching: VectorMatching,
}

/// A function call expression.
#[derive(Debug)]
pub struct FunctionCall {
    pub func: &'static Function,
    pub args: Vec<Expr>,
}

/// A matrix selector expression.
#[derive(Debug)]
pub struct MatrixSelector {
    pub vector_selector: Box<Expr>,
    pub range: Duration,
}

/// The `@` modifier allows changing the evaluation time for individual instant
/// and range vectors in a query. The time supplied is a UNIX timestamp or
/// `start()`/`end()`.
#[derive(Debug, PartialEq)]
pub enum AtModifier {
    None,
    Start,
    End,
    Timestamp(u64),
}

impl Default for AtModifier {
    fn default() -> Self {
        AtModifier::None
    }
}

/// A subquery expression.
#[derive(Debug)]
pub struct SubqueryExpr {
    pub expr: Box<Expr>,
    pub range: Duration,
    pub step: Duration,
    pub offset: Duration,
    pub at: AtModifier,
}

/// Number literals can be written as literal integer (octal, decimal, or
/// hexadecimal) or floating-point numbers.
#[derive(Debug)]
pub struct NumberLiteral {
    pub value: f64,
}

/// A paren expression.
#[derive(Debug)]
pub struct ParenExpr {
    pub expr: Box<Expr>,
}

/// Strings may be specified as literals in single quotes or double quotes.
///
/// In single or double quotes a backslash begins an escape sequence, which may
/// be followed by a, b, f, n, r, t, v or \. Specific characters can be provided
/// using octal (\nnn) or hexadecimal (\xnn, \unnnn and \Unnnnnnnn).
#[derive(Debug)]
pub struct StringLiteral {
    pub value: String,
}

/// Unary operators.
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

/// An unary expression. Unary operations are only supported for scalars.
#[derive(Debug)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub rhs: Box<Expr>,
}

/// Match operators are used to compare a label value in a selector with a
/// literal value or a regular expression.
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

/// The matching of a label.
#[derive(Debug, PartialEq)]
pub struct LabelMatcher {
    pub op: MatchOp,
    pub name: String,
    pub value: String,
}

/// A vector selector expression.
#[derive(Debug)]
pub struct VectorSelector {
    pub metric: String,
    pub label_matchers: Vec<LabelMatcher>,
    pub original_offset: Duration,
    pub offset: Duration,
    pub at: AtModifier,
}
