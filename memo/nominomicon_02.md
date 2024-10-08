# Chapter 2

## tag

- nom では、単純なバイトの集まりを tag と呼ぶ
- `tag` 関数は、与えられた文字列のパーサを返す
- 例えば、文字列 `"abc"` をパースするパーサを作りたい場合は、`tag("abc")` を使う

```rust
pub use nom::bytes::complete::tag;
```

- `tag` 関数のシグネチャは以下の通り：

```rust
pub fn tag<T, Input, Error: ParseError<Input>>(
    tag: T
) -> impl Fn(Input) -> IResult<Input, Input, Error>
where
    Input: InputTake + Compare<T>,
    T: InputLength + Clone,
```

- 文字列をパースする場合は以下のようなシグネチャになる

```rust
pub fn tag_string(
    tag: &str
) -> impl Fn(&str) -> IResult<&str, &str, Error>
```

### `tag` の使用例

```rust
fn parse_input(input: &str) -> IResult<&str, &str> {
    tag("abc")(input)
}

fn main() -> Result<(), Box<dyn Error>> {
    let (leftover_input, output) = parse_input("abcWorld")?;
    assert_eq!(output, "abc");
    assert_eq!(leftover_input, "World");

    assert!(parse_input("defWorld").is_err());
    Ok(())
}
```

## nom の定義済みパーサ

- `alpha0`: 0 個以上のアルファベット文字をパースするパーサ
- `alpha1`: 1 個以上のアルファベット文字をパースするパーサ
- `alphanumeric0`: 0 個以上のアルファベット文字と数字をパースするパーサ
- `alphanumeric1`: 1 個以上のアルファベット文字と数字をパースするパーサ
- `digit0`: 0 個以上の数字をパースするパーサ
- `digit1`: 1 個以上の数字をパースするパーサ
- `multispace0`: 0 個以上の空白文字(スペース、タブ、CR)をパースするパーサ
- `multispace1`: 1 個以上の空白文字(スペース、タブ、CR)をパースするパーサ
- `spaces0`: 0 個以上の空白文字(スペース、タブ)をパースするパーサ
- `spaces1`: 1 個以上の空白文字(スペース、タブ)をパースするパーサ
- `line_ending`: 改行をパースするパーサ(\n, \r\n)
- `newline`: 改行をパースするパーサ(\n)
- `tab`: タブをパースするパーサ(\t)

### 使用例

```rust
pub use nom::character::complete::alpha0;

fn parser(input: &str) -> IResult<&str, &str> {
    alpha0(input)
}

fn main() -> Result<(), Box<dyn Error>> {
    let (remaining, letters) = parser("abc123")?;
    assert_eq!(remaining, "123");
    assert_eq!(letters, "abc");
}
```
