# promql-rs

A PromQL parser written in Rust.

## Example

The minimal usage example:

```rust
use promql_rs::parser;

fn main() {
    let ast = parser::parse(r#"sum(rate(http_request_total{app="prometheus"}[5m])) by (host)"#);
    println!("{:#?}", ast.unwrap());
}
```

This will produce the following output:

```rust
AggregateExpr(
    AggregateExpr {
        op: Sum,
        expr: FunctionCall(
            FunctionCall {
                func: Function {
                    name: "rate",
                    arg_types: [
                        Matrix,
                    ],
                    return_type: Vector,
                },
                args: [
                    MatrixSelector(
                        MatrixSelector {
                            vector_selector: VectorSelector(
                                VectorSelector {
                                    metric: "http_request_total",
                                    label_matchers: [
                                        LabelMatcher {
                                            op: Equal,
                                            name: "app",
                                            value: "prometheus",
                                        },
                                    ],
                                    original_offset: 0ns,
                                    offset: 0ns,
                                    at: None,
                                },
                            ),
                            range: 300s,
                        },
                    ),
                ],
            },
        ),
        param: None,
        modifier: By(
            [
                "host",
            ],
        ),
    },
)
```

## TODOs

- [ ] Documentations.
- [ ] Support VictoriaMetrics extensions.
