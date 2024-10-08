# Chapter 1

## パーサとは

- パーサとは、入力を受け取り、結果を返す関数
  - `Ok` はパーサ自身が探し求めていたものを見つけたことを意味する
  - `Err` はパーサが探し求めていたものを見つけることができなかったことを意味する

- `Ok` の場合、パーサはタプルを返す
  - タプルの第一要素は、パーサが処理しなかった残りの入力
  - タプルの第二要素は、パーサが見つけた結果

- nom ではこの結果を表すために `IResult<I, O, Error>` を使用する
  - `I` は入力の型
  - `O` は出力の型
  - `Error` はエラーの型

### もっとも簡単な例：何もしないパーサ

```rust
use nom::{
    IResult,
};

pub fn do_nothing_parser(input: &str) -> IResult<&str, &str> {
    Ok((input, ""))
}

fn main() -> Result<(), Box<dyn Error>> {
    let (remaining_input, parsed_output) = do_nothing_parser("hello")?;
    assert_eq!(remaining_input, "hello");
    assert_eq!(parsed_output, "");
    Ok(())
}
```
