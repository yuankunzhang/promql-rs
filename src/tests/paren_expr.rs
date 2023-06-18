use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "../src/promql.pest"]
struct ParenExprParser;

#[test]
fn paren_exprs() {
    let pairs = ParenExprParser::parse(Rule::paren_expr, "(1)").unwrap();
    let mut pairs = pairs.into_iter().next().unwrap().into_inner();
    let expr = pairs.next().unwrap();
    assert_eq!(expr.as_rule(), Rule::expr);
    assert_eq!(expr.as_str(), "1");

    let pairs = ParenExprParser::parse(Rule::paren_expr, r#"("string value")"#).unwrap();
    let mut pairs = pairs.into_iter().next().unwrap().into_inner();
    let expr = pairs.next().unwrap();
    assert_eq!(expr.as_rule(), Rule::expr);
    assert_eq!(expr.as_str(), r#""string value""#);
}
