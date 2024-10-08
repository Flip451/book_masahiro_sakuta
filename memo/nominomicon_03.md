# Chapter 3

## Alternative

- `alt` コンビネータは、引数として二つ以上のパーサを含むタプルを受け取る
- このコンビネータは、与えられたパーサを順番に適用し、最初に成功したパーサの結果を返す
- どちらのパーサも失敗した場合、`alt` は最後のパーサの結果を返す
- 例：

```rust
fn parse_abc_or_def(input: &str) -> IResult<&str, &str> {
    alt((tag("abc"), tag("def")))(input)
}

fn main() -> Result<(), Box<dyn Error>> {
    let (remaining, output) = parse_abc_or_def("abcWorld")?;
    assert_eq!(remaining, "World");
    assert_eq!(output, "abc");

    let (remaining, output) = parse_abc_or_def("def123")?;
    assert_eq!(remaining, "123");
    assert_eq!(output, "def");

    assert!(parse_abc_or_def("ghiWorld").is_err());

    Ok(())
}
```

## Composition

- `tuple` コンビネータは、引数として二つ以上のパーサを含むタプルを受け取る
- このコンビネータは、与えられたパーサを順番に適用し、成功した結果をタプルとして返す
- または、失敗した場合は、最初に失敗したパーサの結果を返す
- 例：

```rust
extern crate nom;
use nom::branch::alt;
use nom::sequence::tuple;
use nom::bytes::complete::tag_no_case;
use nom::IResult;
use std::error::Error;

fn parse_base(input: &str) -> IResult<&str, &str> {
    alt((
        tag_no_case("a"),
        tag_no_case("t"),
        tag_no_case("c"),
        tag_no_case("g")
    ))(input)
}

fn parse_pair(input: &str) -> IResult<&str, (&str, &str)> {
    // the many_m_n combinator might also be appropriate here.
    tuple((
        parse_base,
        parse_base,
    ))(input)
}

fn main() -> Result<(), Box<dyn Error>> {
    let (remaining, parsed) = parse_pair("aTcG")?;
    assert_eq!(parsed, ("a", "T"));
    assert_eq!(remaining, "cG");

    assert!(parse_pair("Dct").is_err());

  Ok(())
}
```

### `tuple` 以外の Composition の例

- `delimited`: 最初のパーサーからのオブジェクトにマッチしてそれを破棄し、次に2番目のパーサーからオブジェクトを取得し、最後に3番目のパーサーからのオブジェクトにマッチしてそれを破棄する
  - 例：`delimited(char('('), take(2), char(')'))`

- `preceded`: 最初のパーサーからのオブジェクトにマッチしてそれを破棄し、次に2番目のパーサーからオブジェクトを取得する
  - 例：`preceded(tag("ab"), tag("XY"))`

- `terminated`: 最初のパーサーからオブジェクトを取得し、次に2番目のパーサーからオブジェクトにマッチし、それを破棄する
  - 例：`terminated(tag("ab"), tag("XY"))`

- `pair`: 最初のパーサーからオブジェクトを取得し、次に2番目のパーサーからオブジェクトを取得する
  - 例：`pair(tag("ab"), tag("XY"))`

- `separated_pair`: 最初のパーサーからオブジェクトを取得し、次にsep_parserからオブジェクトにマッチしてそれを破棄し、次に2番目のパーサーから別のオブジェクトを取得する。
  - 例：`separated_pair(tag("hello"), char(','), tag("world"))`
