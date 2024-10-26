use nom::{bytes::complete::tag, character::complete::multispace0, sequence::delimited, IResult};

// 要素の左右の空白と改行を無視するための高階関数
// fn space_delimited<'src, F, O, E>(f: F) -> impl FnMut(&'src str) -> IResult<&'src str, O, E>
// where
//     F: Parser<&'src str, O, E>,
//     E: ParseError<&'src str>,
// {
//     delimited(
//         many0(alt((space1, line_ending))),
//         f,
//         many0(alt((space1, line_ending))),
//     )
// }

pub(crate) fn open_brace<'src>(input: &'src str) -> IResult<&'src str, ()> {
    let (input, _) = delimited(multispace0, tag("{"), multispace0)(input)?;
    Ok((input, ()))
}

pub(crate) fn close_brace<'src>(input: &'src str) -> IResult<&'src str, ()> {
    let (input, _) = delimited(multispace0, tag("}"), multispace0)(input)?;
    Ok((input, ()))
}
