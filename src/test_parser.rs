use std::time::Duration;
use std::vec;

use crate::ast::*;
use crate::functions::*;
use crate::parser::parse;

fn assert_parse_inner(a: &Expr, b: &Expr) {
    match (a, b) {
        (Expr::BinaryExpr(a), Expr::BinaryExpr(b)) => {
            assert_eq!(a.op, b.op);
            assert_parse_inner(&a.lhs, &b.lhs);
            assert_parse_inner(&a.rhs, &b.rhs);
        }
        (Expr::UnaryExpr(a), Expr::UnaryExpr(b)) => {
            assert_eq!(a.op, b.op);
            assert_parse_inner(&a.rhs, &b.rhs);
        }
        (Expr::ParenExpr(a), Expr::ParenExpr(b)) => {
            assert_parse_inner(&a.expr, &b.expr);
        }
        (Expr::FunctionCall(a), Expr::FunctionCall(b)) => {
            assert_eq!(a.func.name, b.func.name);
            assert_eq!(a.args.len(), b.args.len());
            for (i, arg) in a.args.iter().enumerate() {
                assert_parse_inner(arg, &b.args[i]);
            }
        }
        (Expr::SubqueryExpr(a), Expr::SubqueryExpr(b)) => {
            assert_parse_inner(&a.expr, &b.expr);
            assert_eq!(a.range, b.range);
            assert_eq!(a.step, b.step);
        }
        (Expr::VectorSelector(a), Expr::VectorSelector(b)) => {
            assert_eq!(a.metric, b.metric);
        }
        (Expr::StringLiteral(a), Expr::StringLiteral(b)) => {
            assert_eq!(a.value, b.value);
        }
        (Expr::NumberLiteral(a), Expr::NumberLiteral(b)) => {
            match (a.value.is_nan(), b.value.is_nan()) {
                (true, true) => {}
                (false, false) => assert_eq!(a.value, b.value),
                _ => panic!("NaN mismatch"),
            }
        }
        _ => panic!("Expressions don't match"),
    }
}

fn assert_parse(s: &str, e: &Expr) {
    let parsed = parse(s).unwrap();
    assert_parse_inner(&parsed, e);
}

fn new_number_literal(value: f64) -> Expr {
    Expr::NumberLiteral(NumberLiteral { value })
}

fn new_string_literal(value: &str) -> Expr {
    Expr::StringLiteral(StringLiteral {
        value: value.to_string(),
    })
}

fn new_unary_expr(op: UnaryOp, rhs: Expr) -> Expr {
    Expr::UnaryExpr(UnaryExpr {
        op,
        rhs: Box::new(rhs),
    })
}

fn new_binary_expr(op: BinaryOp, lhs: Expr, rhs: Expr) -> Expr {
    Expr::BinaryExpr(BinaryExpr {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        return_bool: false,
        vector_modifier: VectorModifier::None,
        group_modifier: GroupModifier::None,
    })
}

fn new_bool_binary_expr(op: BinaryOp, lhs: Expr, rhs: Expr) -> Expr {
    Expr::BinaryExpr(BinaryExpr {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        return_bool: true,
        vector_modifier: VectorModifier::None,
        group_modifier: GroupModifier::None,
    })
}

fn new_binary_expr_with_modifiers(
    op: BinaryOp,
    lhs: Expr,
    rhs: Expr,
    return_bool: bool,
    vector_modifier: VectorModifier,
    group_modifier: GroupModifier,
) -> Expr {
    Expr::BinaryExpr(BinaryExpr {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        return_bool,
        vector_modifier,
        group_modifier,
    })
}

fn new_vector_selector(metric: &str) -> Expr {
    Expr::VectorSelector(VectorSelector {
        metric: metric.to_string(),
        label_matchers: vec![],
        offset: OffsetModifier::None,
        at: AtModifier::None,
    })
}

fn new_vector_selector_with_offset(metric: &str, offset: Duration) -> Expr {
    Expr::VectorSelector(VectorSelector {
        metric: metric.to_string(),
        label_matchers: vec![],
        offset: OffsetModifier::Duration(offset),
        at: AtModifier::None,
    })
}

fn new_vector_selector_with_label_matchers(
    metric: &str,
    label_matchers: Vec<LabelMatcher>,
) -> Expr {
    Expr::VectorSelector(VectorSelector {
        metric: metric.to_string(),
        label_matchers,
        offset: OffsetModifier::None,
        at: AtModifier::None,
    })
}

fn new_paren_expr(expr: Expr) -> Expr {
    Expr::ParenExpr(ParenExpr {
        expr: Box::new(expr),
    })
}

fn new_subquery_expr(expr: Expr, range: Duration, step: Duration) -> Expr {
    Expr::SubqueryExpr(SubqueryExpr {
        expr: Box::new(expr),
        range,
        step,
    })
}

fn new_function_call(name: &str, args: Vec<Expr>) -> Expr {
    let func = FUNCTIONS
        .iter()
        .find(|f| f.name == name)
        .ok_or("unknown function name")
        .unwrap();
    Expr::FunctionCall(FunctionCall { func, args })
}

#[test]
fn number_literal() {
    assert_parse("1", &new_number_literal(1.0));
    assert_parse("+Inf", &new_number_literal(std::f64::INFINITY));
    assert_parse("-Inf", &new_number_literal(std::f64::NEG_INFINITY));
    assert_parse("NaN", &new_number_literal(std::f64::NAN));
    assert_parse("-NaN", &new_number_literal(std::f64::NAN));
    assert_parse(".5", &new_number_literal(0.5));
    assert_parse("5.", &new_number_literal(5.0));
    assert_parse("123.4567", &new_number_literal(123.4567));
    assert_parse("5e-3", &new_number_literal(0.005));
    assert_parse("5e3", &new_number_literal(5000.0));
    assert_parse("0xc", &new_number_literal(12.0));
    assert_parse("0755", &new_number_literal(493.0));
    assert_parse("-0755", &new_number_literal(-493.0));
    assert_parse("+5.5e-3", &new_number_literal(0.0055));
}

#[test]
fn string_literal() {
    assert_parse(
        r#""double-quoted string \" with escaped quote""#,
        &new_string_literal(r#"double-quoted string " with escaped quote"#),
    );

    assert_parse(
        r#"'single-quoted string \' with escaped quote'"#,
        &new_string_literal("single-quoted string ' with escaped quote"),
    );

    assert_parse(
        r#"`backtick-quoted string`"#,
        &new_string_literal("backtick-quoted string"),
    );

    assert_parse(
        r#""\a\b\f\n\r\t\v\\\" - \xFF\377\u1234\U00010111\U0001011111☺""#,
        &new_string_literal(
            "\u{7}\u{8}\u{c}\n\r\t\u{b}\\\" - \u{ff}\u{ff}\u{1234}\u{10111}\u{10111}11☺",
        ),
    );

    assert_parse(
        r#"`\a\b\f\n\r\t\v\\\" - \xFF\377\u1234\U00010111\U0001011111☺`"#,
        &new_string_literal(r#"\a\b\f\n\r\t\v\\\" - \xFF\377\u1234\U00010111\U0001011111☺"#),
    );
}

#[test]
fn unary_expr() {
    assert_parse(
        "-1^2",
        &new_unary_expr(
            UnaryOp::Neg,
            new_binary_expr(
                BinaryOp::Pow,
                new_number_literal(1.0),
                new_number_literal(2.0),
            ),
        ),
    );

    assert_parse(
        "-1^-2",
        &new_unary_expr(
            UnaryOp::Neg,
            new_binary_expr(
                BinaryOp::Pow,
                new_number_literal(1.0),
                new_number_literal(-2.0),
            ),
        ),
    );

    assert_parse(
        "-some_metric",
        &new_unary_expr(UnaryOp::Neg, new_vector_selector("some_metric")),
    );

    assert_parse(
        "+some_metric",
        &new_unary_expr(UnaryOp::Pos, new_vector_selector("some_metric")),
    );
}

#[test]
fn scalar_binary_expr() {
    assert_parse(
        "1 + 1",
        &new_binary_expr(
            BinaryOp::Add,
            new_number_literal(1.0),
            new_number_literal(1.0),
        ),
    );

    assert_parse(
        "1 - 1",
        &new_binary_expr(
            BinaryOp::Sub,
            new_number_literal(1.0),
            new_number_literal(1.0),
        ),
    );

    assert_parse(
        "1 * 1",
        &new_binary_expr(
            BinaryOp::Mul,
            new_number_literal(1.0),
            new_number_literal(1.0),
        ),
    );

    assert_parse(
        "1 / 1",
        &new_binary_expr(
            BinaryOp::Div,
            new_number_literal(1.0),
            new_number_literal(1.0),
        ),
    );

    assert_parse(
        "-1*2",
        &new_binary_expr(
            BinaryOp::Mul,
            new_number_literal(-1.0),
            new_number_literal(2.0),
        ),
    );

    assert_parse(
        "-1+2",
        &new_binary_expr(
            BinaryOp::Add,
            new_number_literal(-1.0),
            new_number_literal(2.0),
        ),
    );
}

#[test]
fn bool_binary_expr() {
    assert_parse(
        "1 == bool 1",
        &new_bool_binary_expr(
            BinaryOp::Eq,
            new_number_literal(1.0),
            new_number_literal(1.0),
        ),
    );

    assert_parse(
        "1 != bool 1",
        &new_bool_binary_expr(
            BinaryOp::Ne,
            new_number_literal(1.0),
            new_number_literal(1.0),
        ),
    );

    assert_parse(
        "1 > bool 1",
        &new_bool_binary_expr(
            BinaryOp::Gt,
            new_number_literal(1.0),
            new_number_literal(1.0),
        ),
    );

    assert_parse(
        "1 >= bool 1",
        &new_bool_binary_expr(
            BinaryOp::Ge,
            new_number_literal(1.0),
            new_number_literal(1.0),
        ),
    );

    assert_parse(
        "1 < bool 1",
        &new_bool_binary_expr(
            BinaryOp::Lt,
            new_number_literal(1.0),
            new_number_literal(1.0),
        ),
    );

    assert_parse(
        "1 <= bool 1",
        &new_bool_binary_expr(
            BinaryOp::Le,
            new_number_literal(1.0),
            new_number_literal(1.0),
        ),
    );

    assert_parse(
        "1 < bool 2 - 1 * 2",
        &new_bool_binary_expr(
            BinaryOp::Lt,
            new_number_literal(1.0),
            new_binary_expr(
                BinaryOp::Sub,
                new_number_literal(2.0),
                new_binary_expr(
                    BinaryOp::Mul,
                    new_number_literal(1.0),
                    new_number_literal(2.0),
                ),
            ),
        ),
    );
}

#[test]
fn multi_scalar_binary_expr() {
    assert_parse(
        "+1 + -2 * 1",
        &new_binary_expr(
            BinaryOp::Add,
            new_number_literal(1.0),
            new_binary_expr(
                BinaryOp::Mul,
                new_number_literal(-2.0),
                new_number_literal(1.0),
            ),
        ),
    );

    assert_parse(
        "1 + 2/(3*1)",
        &new_binary_expr(
            BinaryOp::Add,
            new_number_literal(1.0),
            new_binary_expr(
                BinaryOp::Div,
                new_number_literal(2.0),
                new_paren_expr(new_binary_expr(
                    BinaryOp::Mul,
                    new_number_literal(3.0),
                    new_number_literal(1.0),
                )),
            ),
        ),
    );
}

#[test]
fn vector_binary_expr() {
    assert_parse(
        "foo * bar",
        &new_binary_expr(
            BinaryOp::Mul,
            new_vector_selector("foo"),
            new_vector_selector("bar"),
        ),
    );

    assert_parse(
        "foo * sum",
        &new_binary_expr(
            BinaryOp::Mul,
            new_vector_selector("foo"),
            new_vector_selector("sum"),
        ),
    );

    assert_parse(
        "foo == 1",
        &new_binary_expr(
            BinaryOp::Eq,
            new_vector_selector("foo"),
            new_number_literal(1.0),
        ),
    );

    assert_parse(
        "2.5 / bar",
        &new_binary_expr(
            BinaryOp::Div,
            new_number_literal(2.5),
            new_vector_selector("bar"),
        ),
    );

    assert_parse(
        "foo and bar",
        &new_binary_expr(
            BinaryOp::And,
            new_vector_selector("foo"),
            new_vector_selector("bar"),
        ),
    );

    assert_parse(
        "foo or bar",
        &new_binary_expr(
            BinaryOp::Or,
            new_vector_selector("foo"),
            new_vector_selector("bar"),
        ),
    );

    assert_parse(
        "foo unless bar",
        &new_binary_expr(
            BinaryOp::Unless,
            new_vector_selector("foo"),
            new_vector_selector("bar"),
        ),
    );

    assert_parse(
        "foo == 1",
        &new_binary_expr(
            BinaryOp::Eq,
            new_vector_selector("foo"),
            new_number_literal(1.0),
        ),
    );

    assert_parse(
        "foo == bool 1",
        &new_bool_binary_expr(
            BinaryOp::Eq,
            new_vector_selector("foo"),
            new_number_literal(1.0),
        ),
    );
}

#[test]
fn multi_vector_binary_expr() {
    assert_parse(
        "foo + bar or bla and blub",
        &new_binary_expr(
            BinaryOp::Or,
            new_binary_expr(
                BinaryOp::Add,
                new_vector_selector("foo"),
                new_vector_selector("bar"),
            ),
            new_binary_expr(
                BinaryOp::And,
                new_vector_selector("bla"),
                new_vector_selector("blub"),
            ),
        ),
    );

    assert_parse(
        "foo and bar unless baz or qux",
        &new_binary_expr(
            BinaryOp::Or,
            new_binary_expr(
                BinaryOp::Unless,
                new_binary_expr(
                    BinaryOp::And,
                    new_vector_selector("foo"),
                    new_vector_selector("bar"),
                ),
                new_vector_selector("baz"),
            ),
            new_vector_selector("qux"),
        ),
    );
}

#[test]
fn vector_binary_expr_with_modifiers() {
    assert_parse(
        "bar + on(foo) bla / on(baz, buz) group_right(test) blub",
        &new_binary_expr_with_modifiers(
            BinaryOp::Add,
            new_vector_selector("bar"),
            new_binary_expr_with_modifiers(
                BinaryOp::Div,
                new_vector_selector("bla"),
                new_vector_selector("blub"),
                false,
                VectorModifier::On(vec!["baz".to_string(), "buz".to_string()]),
                GroupModifier::Right(vec!["test".to_string()]),
            ),
            false,
            VectorModifier::On(vec!["foo".to_string()]),
            GroupModifier::None,
        ),
    );

    assert_parse(
        "foo * on(test,blub) bar",
        &new_binary_expr_with_modifiers(
            BinaryOp::Mul,
            new_vector_selector("foo"),
            new_vector_selector("bar"),
            false,
            VectorModifier::On(vec!["test".to_string(), "blub".to_string()]),
            GroupModifier::None,
        ),
    );

    assert_parse(
        "foo * on(test,blub) group_left bar",
        &new_binary_expr_with_modifiers(
            BinaryOp::Mul,
            new_vector_selector("foo"),
            new_vector_selector("bar"),
            false,
            VectorModifier::On(vec!["test".to_string(), "blub".to_string()]),
            GroupModifier::Left(vec![]),
        ),
    );

    assert_parse(
        "foo and on(test,blub) bar",
        &new_binary_expr_with_modifiers(
            BinaryOp::And,
            new_vector_selector("foo"),
            new_vector_selector("bar"),
            false,
            VectorModifier::On(vec!["test".to_string(), "blub".to_string()]),
            GroupModifier::None,
        ),
    );

    assert_parse(
        "foo and on() bar",
        &new_binary_expr_with_modifiers(
            BinaryOp::And,
            new_vector_selector("foo"),
            new_vector_selector("bar"),
            false,
            VectorModifier::On(vec![]),
            GroupModifier::None,
        ),
    );

    assert_parse(
        "foo and ignoring(test,blub) bar",
        &new_binary_expr_with_modifiers(
            BinaryOp::And,
            new_vector_selector("foo"),
            new_vector_selector("bar"),
            false,
            VectorModifier::Ignoring(vec!["test".to_string(), "blub".to_string()]),
            GroupModifier::None,
        ),
    );

    assert_parse(
        "foo and ignoring() bar",
        &new_binary_expr_with_modifiers(
            BinaryOp::And,
            new_vector_selector("foo"),
            new_vector_selector("bar"),
            false,
            VectorModifier::Ignoring(vec![]),
            GroupModifier::None,
        ),
    );

    assert_parse(
        "foo unless on(bar) baz",
        &new_binary_expr_with_modifiers(
            BinaryOp::Unless,
            new_vector_selector("foo"),
            new_vector_selector("baz"),
            false,
            VectorModifier::On(vec!["bar".to_string()]),
            GroupModifier::None,
        ),
    );

    assert_parse(
        "foo / on(test,blub) group_left(bar) baz",
        &new_binary_expr_with_modifiers(
            BinaryOp::Div,
            new_vector_selector("foo"),
            new_vector_selector("baz"),
            false,
            VectorModifier::On(vec!["test".to_string(), "blub".to_string()]),
            GroupModifier::Left(vec!["bar".to_string()]),
        ),
    );

    assert_parse(
        "foo / ignoring(test,blub) group_left(blub) bar",
        &new_binary_expr_with_modifiers(
            BinaryOp::Div,
            new_vector_selector("foo"),
            new_vector_selector("bar"),
            false,
            VectorModifier::Ignoring(vec!["test".to_string(), "blub".to_string()]),
            GroupModifier::Left(vec!["blub".to_string()]),
        ),
    );

    assert_parse(
        "foo / ignoring(test,blub) group_left(bar) bar",
        &new_binary_expr_with_modifiers(
            BinaryOp::Div,
            new_vector_selector("foo"),
            new_vector_selector("bar"),
            false,
            VectorModifier::Ignoring(vec!["test".to_string(), "blub".to_string()]),
            GroupModifier::Left(vec!["bar".to_string()]),
        ),
    );

    assert_parse(
        "foo - on(test,blub) group_right(bar,foo) bar",
        &new_binary_expr_with_modifiers(
            BinaryOp::Sub,
            new_vector_selector("foo"),
            new_vector_selector("bar"),
            false,
            VectorModifier::On(vec!["test".to_string(), "blub".to_string()]),
            GroupModifier::Right(vec!["bar".to_string(), "foo".to_string()]),
        ),
    );

    assert_parse(
        "foo - ignoring(test,blub) group_right(bar,foo) bar",
        &new_binary_expr_with_modifiers(
            BinaryOp::Sub,
            new_vector_selector("foo"),
            new_vector_selector("bar"),
            false,
            VectorModifier::Ignoring(vec!["test".to_string(), "blub".to_string()]),
            GroupModifier::Right(vec!["bar".to_string(), "foo".to_string()]),
        ),
    );
}

#[test]
fn vector_selector() {
    assert_parse("foo", &new_vector_selector("foo"));
    assert_parse("min", &new_vector_selector("min"));
    assert_parse(
        "foo offset 5m",
        &new_vector_selector_with_offset("foo", Duration::from_secs(300)),
    );
}

#[test]
fn subquery_expr() {
    assert_parse(
        "foo{bar=\"baz\"}[10m:6s]",
        &new_subquery_expr(
            new_vector_selector_with_label_matchers(
                "foo",
                vec![LabelMatcher {
                    op: MatchOp::Equal,
                    name: "bar".to_string(),
                    value: "baz".to_string(),
                }],
            ),
            Duration::from_secs(600),
            Duration::from_secs(6),
        ),
    );

    assert_parse(
        "foo{bar=\"baz\"}[10m5s:1h6ms]",
        &new_subquery_expr(
            new_vector_selector_with_label_matchers(
                "foo",
                vec![LabelMatcher {
                    op: MatchOp::Equal,
                    name: "bar".to_string(),
                    value: "baz".to_string(),
                }],
            ),
            Duration::from_secs(605),
            Duration::from_millis(3600006),
        ),
    );

    assert_parse(
        "foo[10m:]",
        &new_subquery_expr(
            new_vector_selector("foo"),
            Duration::from_secs(600),
            Duration::default(),
        ),
    );
}
