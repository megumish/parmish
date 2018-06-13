//! parmish
#![feature(slice_patterns)]
#![feature(box_syntax)]
#![deny(missing_docs)]

trait Parser {
    type Output;

    fn run_as_parser(&self, input: String) -> Result<Self::Output, ParseError>;

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

struct OrParser<P1: Parser, P2: Parser> {
    run_as_parser1: Box<Fn(String) -> Result<P1::Output, ParseError>>,
    run_as_parser2: Box<Fn(String) -> Result<P2::Output, ParseError>>,
}

#[derive(PartialEq, Debug)]
enum Either<S, T> {
    Left(S),
    Right(T),
}

impl<P1: Parser, P2: Parser> Parser for OrParser<P1, P2> {
    type Output = Either<P1::Output, P2::Output>;
    fn run_as_parser(&self, input: String) -> Result<Self::Output, ParseError> {
        (self.run_as_parser1)(input.clone()).map(Either::Left).or((self.run_as_parser2)(input).map(Either::Right))
    }
}

#[derive(Debug, PartialEq)]
enum ParseError {
    ReachEOF,
    NoMatching,
}

impl Parser for char {
    type Output = char;
    fn run_as_parser(&self, input: String) -> Result<Self::Output, ParseError> {
        match input.as_bytes() {
            &[input_head, _..] if input_head as char == *self => Ok(input_head as char),
            &[_] => Err(ParseError::NoMatching),
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
                &[_] => Err(ParseError::NoMatching),
                _ => Err(ParseError::ReachEOF),
            }
            _ => Ok(())
        }
    }
}

impl Parser for String {
    type Output = String;
    fn run_as_parser(&self, input: String) -> Result<Self::Output, ParseError> {
        input.as_bytes().starts_with_or_fail_index(self.as_bytes()).map(|()| self.to_string())
    }
}

impl<'a> Parser for &'a str {
    type Output = String;
    fn run_as_parser(&self, input: String) -> Result<Self::Output, ParseError> {
        self.to_string().run_as_parser(input)
    }
}

#[cfg(test)]
mod test {
    use Parser;
    use ParseError;
    use or;
    use Either::*;
    #[test]
    fn parse_char() {
        assert_eq!('a'.run_as_parser("a".to_string()), Ok('a'));
        assert_eq!('b'.run_as_parser("a".to_string()), Err(ParseError::NoMatching));
        assert_eq!('b'.run_as_parser("".to_string()), Err(ParseError::ReachEOF));
    }

    #[test]
    fn parse_string() {
        assert_eq!("abc".run_as_parser("abc".to_string()), Ok("abc".to_string()));
        assert_eq!("abc".run_as_parser("abcd".to_string()), Ok("abc".to_string()));
        assert_eq!("abc".run_as_parser("b".to_string()), Err(ParseError::NoMatching));
        assert_eq!("abc".run_as_parser("ab".to_string()), Err(ParseError::ReachEOF));
    }

    #[test]
    fn or_parser() {
        assert_eq!(or("abc", "def").run_as_parser("abc".to_string()), Ok(Left("abc".to_string())));
        assert_eq!(or("abc", "def").run_as_parser("def".to_string()), Ok(Right("def".to_string())));
        assert_eq!(or('a', "def").run_as_parser("a".to_string()), Ok(Left('a')));
        assert_eq!(or("abc", "def").run_as_parser("def".to_string()), Ok(Right("def".to_string())));
    }
}
