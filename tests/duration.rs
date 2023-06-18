use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "../src/promql.pest"]
struct DurationParser;

fn parse_duration(input: &str) -> u64 {
    let pairs = DurationParser::parse(Rule::duration, input).unwrap_or_else(|e| panic!("{}", e));
    let pair = pairs.into_iter().next().unwrap();
    let mut duration: u64 = 0;
    let mut num = String::new();
    let mut unit = String::new();

    let add_duration = |duration: &mut u64, num: &mut String, unit: &mut String| {
        let value = num.parse::<u64>().unwrap();
        match unit.as_str() {
            "y" => *duration += value * 365 * 24 * 60 * 60 * 1000,
            "w" => *duration += value * 7 * 24 * 60 * 60 * 1000,
            "d" => *duration += value * 24 * 60 * 60 * 1000,
            "h" => *duration += value * 60 * 60 * 1000,
            "m" => *duration += value * 60 * 1000,
            "s" => *duration += value * 1000,
            "ms" => *duration += value,
            _ => panic!("invalid duration unit: {}", unit),
        }
        num.clear();
        unit.clear();
    };

    for c in pair.as_str().chars() {
        if c.is_digit(10) {
            if !unit.is_empty() {
                add_duration(&mut duration, &mut num, &mut unit);
            }
            num.push(c);
        } else {
            unit.push(c);
        }
    }

    if !unit.is_empty() {
        add_duration(&mut duration, &mut num, &mut unit);
    }

    duration
}

#[test]
fn durations() {
    assert_eq!(parse_duration("0y"), 0);
    assert_eq!(parse_duration("1y"), 365 * 24 * 60 * 60 * 1000);
    assert_eq!(parse_duration("1y2w"), (365 + 14) * 24 * 60 * 60 * 1000);
    assert_eq!(parse_duration("1y2w3d"), (365 + 17) * 24 * 60 * 60 * 1000);
    assert_eq!(parse_duration("1y3d"), (365 + 3) * 24 * 60 * 60 * 1000);
    assert_eq!(parse_duration("3m5ms"), 3 * 60 * 1000 + 5);
}
