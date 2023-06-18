# promql-rs

A PromQL parser written in Rust.

## Example

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
