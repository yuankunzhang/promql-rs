use std::{str::FromStr, time::Duration};
use strum_macros::EnumString;

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

#[derive(Debug, EnumString)]
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

#[derive(Debug)]
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, EnumString)]
pub enum UnaryOp {
    Add,
    Sub,
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

#[derive(Debug, EnumString)]
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

#[derive(Debug)]
pub struct AggregateExpr {
    pub op: AggregateOp,
    pub expr: Box<Expr>,
    pub param: Box<Expr>,
    pub grouping: Vec<String>,
    pub without: bool,
}

#[derive(Debug, EnumString)]
pub enum ValueType {
    None,
    Vector,
    Scalar,
    Matrix,
    String,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub variadic: u32,
    pub arg_types: Vec<ValueType>,
    pub return_type: ValueType,
}

#[derive(Debug)]
pub struct FunctionCall {
    pub func: Function,
    pub args: Vec<Expr>,
}

#[derive(Debug)]
pub enum AtModifierOp {
    Start,
    End,
}

#[derive(Debug)]
pub struct SubqueryExpr {
    pub expr: Box<Expr>,
    pub range: Duration,
    pub offset: Duration,
    pub step: Duration,
    pub at_modifier_op: AtModifierOp,
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
    pub offset: Duration,
    pub at_modifier_op: AtModifierOp,
}

#[derive(Debug)]
pub enum MatchOp {
    Equal,
    NotEqual,
    Regex,
    NNotRegex,
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
