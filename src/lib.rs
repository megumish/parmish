//! parmish
#![feature(slice_patterns)]
#![feature(box_syntax)]
#![deny(missing_docs)]

use std::str;
use std::convert::From;
trait Parser: Sized {
    type Output;

    fn run_as_parser(&self, input: String) -> Result<ParserStatus<Self::Output>, ParseError>;
}

fn or<P1: 'static + Parser, P2: 'static + Parser>
    (parser1: P1, parser2: P2) -> OrParser<P1, P2> {
    let run_as_parser1 = box move |x| parser1.run_as_parser(x);
    let run_as_parser2 = box move |x| parser2.run_as_parser(x);
    OrParser {
        run_as_parser1,
        run_as_parser2,
    }
}

fn product<P1: 'static + Parser, P2: 'static + Parser>
    (parser1: P1, parser2: P2) -> ProductParser<P1, P2> {
    let run_as_parser1 = box move |x| parser1.run_as_parser(x);
    let run_as_parser2 = box move |x| parser2.run_as_parser(x);
    ProductParser {
        run_as_parser1,
        run_as_parser2,
    }
}


fn map<P1: 'static + Parser, T>(parser1: P1, function: Box<Fn(P1::Output) -> T>) -> Map<P1, T> {
    let run_as_parser1 = box move |x| parser1.run_as_parser(x);
    let function1 = function;
    Map {
        run_as_parser1,
        function1,
    }
}

struct Map<P1: Parser, T> {
    run_as_parser1: Box<Fn(String) -> Result<ParserStatus<P1::Output>, ParseError>>,
    function1: Box<Fn(P1::Output) -> T>,
}

struct OrParser<P1: Parser, P2: Parser> {
    run_as_parser1: Box<Fn(String) -> Result<ParserStatus<P1::Output>, ParseError>>,
    run_as_parser2: Box<Fn(String) -> Result<ParserStatus<P2::Output>, ParseError>>,
}

struct ProductParser<P1: Parser, P2: Parser> {
    run_as_parser1: Box<Fn(String) -> Result<ParserStatus<P1::Output>, ParseError>>,
    run_as_parser2: Box<Fn(String) -> Result<ParserStatus<P2::Output>, ParseError>>,
}

#[derive(PartialEq, Debug)]
enum Either<S, T> {
    Left(S),
    Right(T),
}

impl<P1: Parser, P2: Parser> Parser for OrParser<P1, P2> {
    type Output = Either<P1::Output, P2::Output>;
    fn run_as_parser(&self, input: String) -> Result<ParserStatus<Self::Output>, ParseError> {
        (self.run_as_parser1)(input.clone()).map(|x| ParserStatus{
            input: x.input,
            output: Either::Left(x.output)})
            .or((self.run_as_parser2)(input).map(|x| ParserStatus{
                input: x.input,
                output: Either::Right(x.output)}))
    }
}

impl<P1: Parser, P2: Parser> Parser for ProductParser<P1, P2> {
    type Output = (P1::Output, P2::Output);
    fn run_as_parser(&self, input: String) -> Result<ParserStatus<Self::Output>, ParseError> {
        (self.run_as_parser1)(input.clone()).and_then(|x| (self.run_as_parser2)(x.input.clone())
                                                      .map(|y| ParserStatus {
                                                          input: y.input,
                                                          output: (x.output, y.output)}))
    }
}

impl<P1: Parser, T> Parser for Map<P1, T> {
    type Output = T;
    fn run_as_parser(&self, input: String) -> Result<ParserStatus<Self::Output>, ParseError> {
        (self.run_as_parser1)(input).map(|x| ParserStatus {
                                        input: x.input.clone(),
                                        output: (self.function1)(x.output)})
    }
}

#[derive(Debug, PartialEq)]
enum ParseError {
    ReachEOF,
    NoMatching,
    Utf8(str::Utf8Error),
}

impl From<str::Utf8Error> for ParseError {
    fn from(error: str::Utf8Error) -> Self {
        ParseError::Utf8(error)
    }
}

#[derive(PartialEq, Debug)]
struct ParserStatus<T> {
    input: String,
    pub output: T,
}

impl Parser for char {
    type Output = char;
    fn run_as_parser(&self, input: String) -> Result<ParserStatus<Self::Output>, ParseError> {
        match input.as_bytes() {
            &[input_head, ref input_tail..] if input_head as char == *self => Ok(ParserStatus{
                input: str::from_utf8(input_tail)?.to_string(),
                output: input_head as char}),
            &[_, _..] => Err(ParseError::NoMatching),
            _ => Err(ParseError::ReachEOF),
        }
    }
}

trait StartsWithOrFailIndex<'a> {
    fn starts_with_or_fail_index(&self, maybe_start_string: &'a [u8]) -> Result<(), ParseError>;
}


impl<'a> StartsWithOrFailIndex<'a> for &'a [u8] {
    fn starts_with_or_fail_index(&self, maybe_start_string: &'a [u8]) -> Result<(), ParseError> {
        match maybe_start_string {
            &[maybe_head, ref maybe_tail..] => match self {
                &[input_head, ref input_tail..] if input_head == &maybe_head => input_tail.starts_with_or_fail_index(maybe_tail),
                &[_, _..] => Err(ParseError::NoMatching),
                _ => Err(ParseError::ReachEOF),
            }
            _ => Ok(())
        }
    }
}

impl Parser for String {
    type Output = String;
    fn run_as_parser(&self, input: String) -> Result<ParserStatus<Self::Output>, ParseError> {
        match input.clone().as_bytes().starts_with_or_fail_index(self.as_bytes()) {
            Ok(_) => Ok(ParserStatus {
                input: str::from_utf8(&input.as_bytes()[self.len()..])?.to_string(),
                output: self.to_string()}),
            Err(error) => Err(error)
        }
    }
}

impl<'a> Parser for &'a str {
    type Output = String;
    fn run_as_parser(&self, input: String) -> Result<ParserStatus<Self::Output>, ParseError> {
        self.to_string().run_as_parser(input)
    }
}

#[cfg(test)]
mod test {
    use Parser;
    use ParseError;
    use or;
    use product;
    use map;
    use Either::*;
    use ParserStatus;
    #[test]
    fn parse_char() {
        assert_eq!('a'.run_as_parser("a".to_string()), Ok(ParserStatus { input: "".to_string(), output:'a' }));
        assert_eq!('b'.run_as_parser("a".to_string()), Err(ParseError::NoMatching));
        assert_eq!('b'.run_as_parser("".to_string()), Err(ParseError::ReachEOF));
    }

    #[test]
    fn parse_string() {
        assert_eq!("abc".run_as_parser("abc".to_string()), Ok(ParserStatus { input: "".to_string(), output: "abc".to_string()}));
        assert_eq!("abc".run_as_parser("abcd".to_string()), Ok(ParserStatus { input: "d".to_string(), output: "abc".to_string()}));
        assert_eq!("def".run_as_parser("gdh".to_string()), Err(ParseError::NoMatching));
        assert_eq!("abc".run_as_parser("ab".to_string()), Err(ParseError::ReachEOF));
    }

    #[test]
    fn or_parser() {
        assert_eq!(or("abc", "def").run_as_parser("abc".to_string()), Ok(ParserStatus { input: "".to_string(), output: Left("abc".to_string())}));
        assert_eq!(or("abc", "def").run_as_parser("def".to_string()), Ok(ParserStatus { input: "".to_string(), output: Right("def".to_string())}));
        assert_eq!(or('a', "def").run_as_parser("a".to_string()), Ok(ParserStatus { input: "".to_string(), output: Left('a')}));
    }

    #[test]
    fn product_parser() {
        assert_eq!(product("abc", "def").run_as_parser("abcdef".to_string()), Ok(ParserStatus { input: "".to_string(), output: ("abc".to_string(), "def".to_string())}));
        assert_eq!(product("abc", "def").run_as_parser("abcgdh".to_string()), Err(ParseError::NoMatching));
    }

    #[test]
    fn map_test() {
        assert_eq!(map(product("abc", "def"), box |output| {
            let (output1, output2) = output;
            format!("{}{}", output1, output2)
        }).run_as_parser("abcdef".to_string()), Ok(ParserStatus { input: "".to_string(), output: "abcdef".to_string()}));
    }
}

