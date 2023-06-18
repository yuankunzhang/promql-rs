use crate::ast::*;
use crate::parser::*;

macro_rules! number_literal {
    ($value:expr) => {
        Expr::NumberLiteral(NumberLiteral { value: $value })
    };
}

macro_rules! _string_literal {
    ($value:expr) => {
        Expr::StringLiteral(StringLiteral {
            value: $value.to_string(),
        })
    };
}

fn assert_parse(s: &str, e: Expr) {
    assert_parse_inner(parse(s).unwrap(), e);
}

fn assert_parse_inner(a: Expr, b: Expr) {
    match (a, b) {
        (Expr::NumberLiteral(a), Expr::NumberLiteral(b)) => {
            assert_eq!(a.value, b.value);
        }
        (Expr::StringLiteral(a), Expr::StringLiteral(b)) => {
            assert_eq!(a.value, b.value);
        }
        _ => panic!("Expressions do not match"),
    }
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
    assert_parse(r#""""#, _string_literal!(r#""#));
    assert_parse(r#""foo""#, _string_literal!(r#"foo"#));
    assert_parse(r#""foo \"bar\"""#, _string_literal!(r#"foo "bar""#));
    assert_parse(r#"'foo \"bar\"'"#, _string_literal!(r#"foo "bar""#));
}
