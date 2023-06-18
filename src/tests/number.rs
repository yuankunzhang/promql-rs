use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "../src/promql.pest"]
struct NumberParser;

fn parse_number(input: &str) -> f64 {
    let pairs =
        NumberParser::parse(Rule::number_literal, input).unwrap_or_else(|e| panic!("{}", e));
    let pair = pairs.into_iter().next().unwrap();
    let inner = pair.into_inner().next().unwrap();

    let s = inner.as_str();
    if s.starts_with("0x") || s.starts_with("0X") {
        u64::from_str_radix(&s[2..], 16).unwrap() as f64
    } else if s.starts_with("0") && s.len() > 1 && s.as_bytes()[1] != b'.' {
        u64::from_str_radix(&s[1..], 8).unwrap() as f64
    } else {
        s.parse().unwrap()
    }
}

#[test]
fn integers() {
    assert_eq!(parse_number("0"), 0.0);
    assert_eq!(parse_number("1"), 1.0);
    assert_eq!(parse_number("123"), 123.0);
}

#[test]
fn floats() {
    assert_eq!(parse_number("0.0"), 0.0);
    assert_eq!(parse_number("1.0"), 1.0);
    assert_eq!(parse_number("1."), 1.0);
    assert_eq!(parse_number(".1"), 0.1);
    assert_eq!(parse_number("123.0"), 123.0);
    assert_eq!(parse_number("0.1"), 0.1);
    assert_eq!(parse_number("1.1"), 1.1);
    assert_eq!(parse_number("123.1"), 123.1);
    assert_eq!(parse_number("0.123"), 0.123);
    assert_eq!(parse_number("1.123"), 1.123);
    assert_eq!(parse_number("123.123"), 123.123);
    assert_eq!(parse_number("0.123e1"), 1.23);
    assert_eq!(parse_number("1.123e1"), 11.23);
    assert_eq!(parse_number("2.e3"), 2000.);
    assert_eq!(parse_number(".2e-3"), 0.0002);
    assert_eq!(parse_number("123.123e1"), 1231.23);
    assert_eq!(parse_number("0.123e+1"), 1.23);
    assert_eq!(parse_number("1.123e+1"), 11.23);
    assert_eq!(parse_number("123.123e+1"), 1231.23);
    assert_eq!(parse_number("0.123e-1"), 0.0123);
    assert_eq!(parse_number("1.123e-1"), 0.1123);
    assert_eq!(parse_number("123.123e-1"), 12.3123);
    assert_eq!(parse_number("0.123e+10"), 1230000000.0);
    assert_eq!(parse_number("1.123e+10"), 11230000000.0);
    assert_eq!(parse_number("123.123e+10"), 1231230000000.0);
    assert_eq!(parse_number("0.123e-10"), 0.0000000000123);
    assert_eq!(parse_number("1.123e-10"), 0.0000000001123);
    assert_eq!(parse_number("123.123e-10"), 0.0000000123123);
}

#[test]
fn hexadecimals() {
    assert_eq!(parse_number("0x0"), 0.0);
    assert_eq!(parse_number("0x1"), 1.0);
    assert_eq!(parse_number("0x123"), 291.0);
}

#[test]
fn octals() {
    assert_eq!(parse_number("00"), 0.0);
    assert_eq!(parse_number("01"), 1.0);
    assert_eq!(parse_number("0123"), 83.0);
}

#[test]
fn inf() {
    assert_eq!(parse_number("Inf"), f64::INFINITY);
    assert_eq!(parse_number("+Inf"), f64::INFINITY);
    assert_eq!(parse_number("-Inf"), f64::NEG_INFINITY);
}

#[test]
fn nan() {
    assert!(parse_number("NaN").is_nan());
    assert!(parse_number("+NaN").is_nan());
    assert!(parse_number("-NaN").is_nan());
}
