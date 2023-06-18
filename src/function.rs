#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValueType {
    None,
    Vector,
    Scalar,
    Matrix,
    String,
}

#[derive(Debug)]
pub struct Function {
    pub name: &'static str,
    pub arg_types: Vec<ValueType>,
    pub return_type: ValueType,
}

lazy_static::lazy_static! {
    pub static ref FUNCTIONS: Vec<Function> = vec![
        Function {
            name: "abs",
            arg_types: vec![ValueType::Scalar],
            return_type: ValueType::Vector,
        },
        Function {
            name: "absent",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "absent_over_time",
            arg_types: vec![ValueType::Matrix],
            return_type: ValueType::Vector,
        },
        Function {
            name: "acos",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "acosh",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "asin",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "asinh",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "atan",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "atanh",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "avg_over_time",
            arg_types: vec![ValueType::Matrix],
            return_type: ValueType::Vector,
        },
        Function {
            name: "ceil",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "changes",
            arg_types: vec![ValueType::Matrix],
            return_type: ValueType::Vector,
        },
        Function {
            name: "clamp",
            arg_types: vec![ValueType::Vector, ValueType::Scalar, ValueType::Scalar],
            return_type: ValueType::Vector,
        },
        Function {
            name: "clamp_max",
            arg_types: vec![ValueType::Vector, ValueType::Scalar],
            return_type: ValueType::Vector,
        },
        Function {
            name: "clamp_min",
            arg_types: vec![ValueType::Vector, ValueType::Scalar],
            return_type: ValueType::Vector,
        },
        Function {
            name: "cos",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "cosh",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "count_over_time",
            arg_types: vec![ValueType::Matrix],
            return_type: ValueType::Vector,
        },
        Function {
            name: "days_in_month",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "day_of_month",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "day_of_week",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "days_of_year",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "deg",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "delta",
            arg_types: vec![ValueType::Matrix],
            return_type: ValueType::Vector,
        },
        Function {
            name: "deriv",
            arg_types: vec![ValueType::Matrix],
            return_type: ValueType::Vector,
        },
        Function {
            name: "exp",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "floor",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "histogram_count",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "histogram_sum",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "histogram_quantile",
            arg_types: vec![ValueType::Scalar, ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "holt_winters",
            arg_types: vec![ValueType::Matrix, ValueType::Scalar, ValueType::Scalar],
            return_type: ValueType::Vector,
        },
        Function {
            name: "hour",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "idelta",
            arg_types: vec![ValueType::Matrix],
            return_type: ValueType::Vector,
        },
        Function {
            name: "increase",
            arg_types: vec![ValueType::Matrix],
            return_type: ValueType::Vector,
        },
        Function {
            name: "irate",
            arg_types: vec![ValueType::Matrix],
            return_type: ValueType::Vector,
        },
        Function {
            name: "label_replace",
            arg_types: vec![
                ValueType::Vector,
                ValueType::String,
                ValueType::String,
                ValueType::String,
                ValueType::String,
            ],
            return_type: ValueType::Vector,
        },
        Function {
            name: "label_join",
            arg_types: vec![ValueType::Vector, ValueType::String, ValueType::String, ValueType::String],
            return_type: ValueType::Vector,
        },
        Function {
            name: "last_over_time",
            arg_types: vec![ValueType::Matrix],
            return_type: ValueType::Vector,
        },
        Function {
            name: "ln",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "log10",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "log2",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "max_over_time",
            arg_types: vec![ValueType::Matrix],
            return_type: ValueType::Vector,
        },
        Function {
            name: "min_over_time",
            arg_types: vec![ValueType::Matrix],
            return_type: ValueType::Vector,
        },
        Function {
            name: "minute",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "month",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "pi",
            arg_types: vec![],
            return_type: ValueType::Scalar,
        },
        Function {
            name: "predict_linear",
            arg_types: vec![ValueType::Matrix, ValueType::Scalar],
            return_type: ValueType::Vector,
        },
        Function {
            name: "quantile_over_time",
            arg_types: vec![ValueType::Scalar, ValueType::Matrix],
            return_type: ValueType::Vector,
        },
        Function {
            name: "rad",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "rate",
            arg_types: vec![ValueType::Matrix],
            return_type: ValueType::Vector,
        },
        Function {
            name: "resets",
            arg_types: vec![ValueType::Matrix],
            return_type: ValueType::Vector,
        },
        Function {
            name: "round",
            arg_types: vec![ValueType::Vector, ValueType::Scalar],
            return_type: ValueType::Vector,
        },
        Function {
            name: "scalar",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Scalar,
        },
        Function {
            name: "sgn",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "sin",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "sinh",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "sort",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "sort_desc",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "sqrt",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "stddev_over_time",
            arg_types: vec![ValueType::Matrix],
            return_type: ValueType::Vector,
        },
        Function {
            name: "stdvar_over_time",
            arg_types: vec![ValueType::Matrix],
            return_type: ValueType::Vector,
        },
        Function {
            name: "sum_over_time",
            arg_types: vec![ValueType::Matrix],
            return_type: ValueType::Vector,
        },
        Function {
            name: "tan",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "tanh",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
        Function {
            name: "time",
            arg_types: vec![],
            return_type: ValueType::Scalar,
        },
        Function {
            name: "timestamp",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Scalar,
        },
        Function {
            name: "vector",
            arg_types: vec![ValueType::Scalar],
            return_type: ValueType::Vector,
        },
        Function {
            name: "year",
            arg_types: vec![ValueType::Vector],
            return_type: ValueType::Vector,
        },
    ];
}
