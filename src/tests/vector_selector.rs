use crate::ast::*;
use pest::Parser;
use pest_derive::Parser;
use std::{str::FromStr, time::Duration, vec};

#[derive(Parser)]
#[grammar = "../src/promql.pest"]
struct VectorSelectorParser;

fn parse(input: &str) -> VectorSelector {
    let pairs = VectorSelectorParser::parse(Rule::vector_selector, input)
        .unwrap_or_else(|e| panic!("{}", e));
    let mut pairs = pairs.into_iter().next().unwrap().into_inner();

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

    VectorSelector {
        metric,
        label_matchers: label_matchers,
        original_offset: Duration::default(),
        offset: Duration::default(),
        at: AtModifier::None,
    }
}

macro_rules! vector_selector {
    ($metric:expr $(, $op:expr, $name:expr, $value:expr)*) => {
        VectorSelector {
            metric: $metric.to_string(),
            label_matchers: vec![$(LabelMatcher {
                op: MatchOp::from_str($op).unwrap(),
                name: $name.to_string(),
                value: $value.to_string(),
            }),*],
            original_offset: Duration::default(),
            offset: Duration::default(),
            at: AtModifier::None,
        }
    };
}

macro_rules! assert_vector_selector_eq {
    ($lhs:expr, $rhs:expr) => {
        assert_eq!($lhs.metric, $rhs.metric);
        assert_eq!($lhs.label_matchers, $rhs.label_matchers);
    };
}

#[test]
fn metrics() {
    assert_vector_selector_eq!(parse("up"), vector_selector!("up"));
    assert_vector_selector_eq!(parse("foo"), vector_selector!("foo"));
}

#[test]
fn label_matchers() {
    assert_vector_selector_eq!(parse("{}"), vector_selector!(""));
    assert_vector_selector_eq!(
        parse("{foo=\"bar\"}"),
        vector_selector!("", "=", "foo", "bar")
    );
    assert_vector_selector_eq!(
        parse("{foo=\"bar\", baz!=\"qux\"}"),
        vector_selector!("", "=", "foo", "bar", "!=", "baz", "qux")
    );
    assert_vector_selector_eq!(
        parse("{foo=~\"bar\", baz!~\"qux\"}"),
        vector_selector!("", "=~", "foo", "bar", "!~", "baz", "qux")
    );
}

#[test]
fn vector_selectors() {
    assert_vector_selector_eq!(parse("up{}"), vector_selector!("up"));
    assert_vector_selector_eq!(
        parse("up{foo=\"bar\"}"),
        vector_selector!("up", "=", "foo", "bar")
    );
    assert_vector_selector_eq!(
        parse("up{foo=\"bar\", baz!=\"qux\"}"),
        vector_selector!("up", "=", "foo", "bar", "!=", "baz", "qux")
    );
    assert_vector_selector_eq!(
        parse("up{foo=~\"bar\", baz!~\"qux\"}"),
        vector_selector!("up", "=~", "foo", "bar", "!~", "baz", "qux")
    );
}
