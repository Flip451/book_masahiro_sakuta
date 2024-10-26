#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    EmptyTuple,
    F64(f64),
    I64(i64),
    Boolean(bool),
    String(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::EmptyTuple => write!(f, "()"),
            Value::F64(d) => write!(f, "{d}"),
            Value::I64(i) => write!(f, "{i}"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::String(s) => write!(f, "{s}"),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::F64(a), Value::F64(b)) => a.partial_cmp(b),
            (Value::I64(a), Value::I64(b)) => a.partial_cmp(b),
            (Value::F64(a), Value::I64(b)) => a.partial_cmp(&(*b as f64)),
            (Value::I64(a), Value::F64(b)) => (*a as f64).partial_cmp(b),
            // (Value::Boolean(a), Value::Boolean(b)) => a.partial_cmp(b),
            (Value::String(a), Value::String(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

impl Value {
    pub(crate) fn coerce_to_f64(&self) -> f64 {
        match self {
            Value::F64(d) => *d,
            Value::I64(i) => *i as f64,
            Value::Boolean(b) => panic!("Cannot coerce boolean to f64: {b}"),
            Value::String(s) => panic!("Cannot coerce string to f64: {s}"),
            Value::EmptyTuple => panic!("Cannot coerce empty tuple to f64"),
        }
    }

    pub(crate) fn coerce_to_i64(&self) -> i64 {
        match self {
            Value::F64(d) => *d as i64,
            Value::I64(i) => *i,
            Value::Boolean(b) => panic!("Cannot coerce boolean to i64: {b}"),
            Value::String(s) => panic!("Cannot coerce string to i64: {s}"),
            Value::EmptyTuple => panic!("Cannot coerce empty tuple to i64"),
        }
    }

    // 以下、スクリプト上での型キャスト用の関数
    pub(crate) fn as_f64(&self) -> Option<f64> {
        match self {
            Value::F64(d) => Some(*d),
            Value::I64(i) => Some(*i as f64),
            Value::Boolean(b) => Some(if *b { 1.0 } else { 0.0 }),
            Value::String(s) => s.parse().ok(),
            Value::EmptyTuple => None,
        }
    }

    pub(crate) fn as_i64(&self) -> Option<i64> {
        match self {
            Value::F64(d) => Some(*d as i64),
            Value::I64(i) => Some(*i),
            Value::Boolean(b) => Some(if *b { 1 } else { 0 }),
            Value::String(s) => s.parse().ok(),
            Value::EmptyTuple => None,
        }
    }

    pub(crate) fn as_boolean(&self) -> Option<bool> {
        match self {
            Value::Boolean(b) => Some(*b),
            Value::F64(d) => Some(*d != 0.0),
            Value::I64(i) => Some(*i != 0),
            Value::String(s) => Some(s.len() > 0),
            Value::EmptyTuple => None,
        }
    }

    pub(crate) fn as_string(&self) -> Option<String> {
        match self {
            Value::String(s) => Some(s.clone()),
            Value::F64(d) => Some(d.to_string()),
            Value::I64(i) => Some(i.to_string()),
            Value::Boolean(b) => Some(b.to_string()),
            Value::EmptyTuple => None,
        }
    }

    pub(crate) fn binary_op_string(
        lhs: &Value,
        rhs: &Value,
        d: impl Fn(f64, f64) -> f64,
        i: impl Fn(i64, i64) -> i64,
        b: impl Fn(bool, bool) -> bool,
        s: impl Fn(&str, &str) -> String,
    ) -> Self {
        match (lhs, rhs) {
            (Value::F64(lhs), rhs) => Value::F64(d(*lhs, rhs.coerce_to_f64())),
            (lhs, Value::F64(rhs)) => Value::F64(d(lhs.coerce_to_f64(), *rhs)),
            (Value::I64(lhs), Value::I64(rhs)) => Value::I64(i(*lhs, *rhs)),
            (Value::Boolean(lhs), Value::Boolean(rhs)) => Value::Boolean(b(*lhs, *rhs)),
            (Value::String(lhs), Value::String(rhs)) => Value::String(s(lhs, rhs)),
            _ => panic!("Cannot perform binary operation on {:?} and {:?}", lhs, rhs),
        }
    }
}

impl std::ops::Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Value::binary_op_string(
            &self,
            &rhs,
            |a, b| a + b,
            |a, b| a + b,
            |a, b| a || b,
            |a, b| a.to_string() + b,
        )
    }
}

impl std::ops::Sub for Value {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Value::binary_op_string(
            &self,
            &rhs,
            |a, b| a - b,
            |a, b| a - b,
            |_, _| panic!("Subtraction between booleans is not defined."),
            |_, _| panic!("Subtraction between strings is not defined."),
        )
    }
}

impl std::ops::Mul for Value {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Value::binary_op_string(
            &self,
            &rhs,
            |a, b| a * b,
            |a, b| a * b,
            |a, b| a && b,
            |_, _| panic!("Multiplication between strings is not defined."),
        )
    }
}

impl std::ops::Div for Value {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Value::binary_op_string(
            &self,
            &rhs,
            |a, b| a / b,
            |a, b| a / b,
            |_, _| panic!("Division between booleans is not defined."),
            |_, _| panic!("Division between strings is not defined."),
        )
    }
}

impl std::ops::Rem for Value {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        Value::binary_op_string(
            &self,
            &rhs,
            |a, b| a % b,
            |a, b| a % b,
            |_, _| panic!("Remainder between booleans is not defined."),
            |_, _| panic!("Remainder between strings is not defined."),
        )
    }
}
