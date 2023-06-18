use crate::ast::*;
use crate::function::FUNCTIONS;
use crate::parser::*;
use std::str::FromStr;
use std::time::Duration;

macro_rules! aggregate_expr {
    ($op:expr, $modifier:expr, $param:expr, $expr:expr) => {
        Expr::AggregateExpr(AggregateExpr {
            op: AggregateOp::from_str($op).unwrap(),
            expr: Box::new($expr),
            param: Option::from(Box::new($param)),
            modifier: $modifier,
        })
    };
}

macro_rules! function_call {
    ($name:expr $(, $arg:expr)*) => {
        Expr::FunctionCall(FunctionCall {
            func: FUNCTIONS.iter().find(|f| f.name == $name).unwrap(),
            args: vec![$($arg),*],
        })
    };
}

macro_rules! number_literal {
    ($value:expr) => {
        Expr::NumberLiteral(NumberLiteral { value: $value })
    };
}

macro_rules! paren_expr {
    ($expr:expr) => {
        Expr::ParenExpr(ParenExpr {
            expr: Box::new($expr),
        })
    };
}

macro_rules! string_literal {
    ($value:expr) => {
        Expr::StringLiteral(StringLiteral {
            value: $value.to_string(),
        })
    };
}

macro_rules! unary_expr {
    ($op:expr, $rhs:expr) => {
        Expr::UnaryExpr(UnaryExpr {
            op: UnaryOp::from_str($op).unwrap(),
            rhs: Box::new($rhs),
        })
    };
}

macro_rules! vector_selector {
    ($metric:expr $(, $op:expr, $name:expr, $value:expr)*) => {
        Expr::VectorSelector(VectorSelector {
            metric: $metric.to_string(),
            label_matchers: vec![$(LabelMatcher {
                op: MatchOp::from_str($op).unwrap(),
                name: $name.to_string(),
                value: $value.to_string(),
            }),*],
            original_offset: Duration::default(),
            offset: Duration::default(),
            at: AtModifier::None,
        })
    };
}

fn assert_parse(s: &str, e: Expr) {
    assert_parse_inner(parse(s).unwrap(), e);
}

fn assert_parse_inner(a: Expr, b: Expr) {
    match (a, b) {
        (Expr::AggregateExpr(a), Expr::AggregateExpr(b)) => {
            assert_eq!(a.op, b.op);
            assert_eq!(a.modifier, b.modifier);
            assert_parse_inner(*a.expr, *b.expr);
        }
        (Expr::FunctionCall(a), Expr::FunctionCall(b)) => {
            assert_eq!(a.func.name, b.func.name);
            assert_eq!(a.args.len(), b.args.len());
        }
        (Expr::NumberLiteral(a), Expr::NumberLiteral(b)) => {
            assert_eq!(a.value, b.value);
        }
        (Expr::ParenExpr(a), Expr::ParenExpr(b)) => {
            assert_parse_inner(*a.expr, *b.expr);
        }
        (Expr::StringLiteral(a), Expr::StringLiteral(b)) => {
            assert_eq!(a.value, b.value);
        }
        (Expr::UnaryExpr(a), Expr::UnaryExpr(b)) => {
            assert_eq!(a.op, b.op);
            assert_parse_inner(*a.rhs, *b.rhs);
        }
        (Expr::VectorSelector(a), Expr::VectorSelector(b)) => {
            assert_eq!(a.metric, b.metric);
            assert_eq!(a.label_matchers.len(), b.label_matchers.len());
        }
        _ => panic!("Expressions do not match"),
    }
}

#[test]
fn parse_aggregate_exprs() {
    assert_parse(
        "sum without(host) (http_requests_total)",
        aggregate_expr!(
            "sum",
            AggregateModifier::Without(vec!["host".to_string()]),
            string_literal!(""),
            vector_selector!("http_requests_total")
        ),
    );

    assert_parse(
        "count by(application, group) (http_requests_total)",
        aggregate_expr!(
            "count",
            AggregateModifier::By(vec!["application".to_string(), "group".to_string()]),
            string_literal!(""),
            vector_selector!("http_requests_total")
        ),
    );
}

#[test]
fn parse_function_calls() {
    assert_parse("abs(-1)", function_call!("abs", number_literal!(1.0)));
    assert_parse(
        r#"absent(nonexistent{job="myjob"})"#,
        function_call!(
            "absent",
            vector_selector!("nonexistent", "=", "job", "myjob")
        ),
    );
}

#[test]
fn parse_number_literals() {
    assert_parse("0", number_literal!(0.0));
    assert_parse("1", number_literal!(1.0));
    assert_parse("-123", number_literal!(-123.0));

    assert_parse("0x0", number_literal!(0.0));
    assert_parse("0x1", number_literal!(1.0));
    assert_parse("-0x123", number_literal!(-291.0));

    assert_parse("00", number_literal!(0.0));
    assert_parse("01", number_literal!(1.0));
    assert_parse("-0123", number_literal!(-83.0));

    assert_parse("1.0", number_literal!(1.0));
    assert_parse("1.", number_literal!(1.0));
    assert_parse("-.1", number_literal!(-0.1));

    assert_parse("0.123e1", number_literal!(1.23));
    assert_parse("2.e3", number_literal!(2000.));
    assert_parse("-.2e-3", number_literal!(-0.0002));

    assert_parse("Inf", number_literal!(f64::INFINITY));
    assert_parse("+iNf", number_literal!(f64::INFINITY));
    assert_parse("-inF", number_literal!(f64::NEG_INFINITY));
}

#[test]
fn parse_string_literals() {
    assert_parse(r#""""#, string_literal!(r#""#));
    assert_parse(r#""foo""#, string_literal!(r#"foo"#));
    assert_parse(r#""foo \"bar\"""#, string_literal!(r#"foo "bar""#));
    assert_parse(r#"'foo \"bar\"'"#, string_literal!(r#"foo "bar""#));
}

#[test]
fn parse_unary_exprs() {
    assert_parse("-(1)", unary_expr!("-", paren_expr!(number_literal!(1.0))));
    assert_parse(
        "--(1)",
        unary_expr!("-", unary_expr!("-", paren_expr!(number_literal!(1.0)))),
    );
}

#[test]
fn parse_vector_selector() {
    assert_parse("up", vector_selector!("up"));
    assert_parse("abs", vector_selector!("abs"));
    assert_parse("up{}", vector_selector!("up"));
    assert_parse("{}", vector_selector!(""));
    assert_parse(
        r#"up{foo!="bar", baz=~"qux"}"#,
        vector_selector!("up", "!=", "foo", "bar", "=~", "baz", "qux"),
    );
    assert_parse(
        r#"{foo="bar", baz!~"qux"}"#,
        vector_selector!("", "=", "foo", "bar", "!~", "baz", "qux"),
    );
}
