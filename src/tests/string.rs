use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "../src/promql.pest"]
struct StringParser;

fn parse_string(input: &str) -> String {
    let pairs =
        StringParser::parse(Rule::string_literal, input).unwrap_or_else(|e| panic!("{}", e));
    let pair = pairs.into_iter().next().unwrap();
    let inner = pair.into_inner().next().unwrap();

    inner.as_str().to_string()
}

#[test]
fn strings() {
    assert_eq!(parse_string(r#""""#), r#""#);
    assert_eq!(parse_string(r#""foo""#), r#"foo"#);
    assert_eq!(parse_string(r#""foo \"bar\"""#), r#"foo \"bar\""#);
    assert_eq!(parse_string(r#"'foo \"bar\"'"#), r#"foo \"bar\""#);
}
