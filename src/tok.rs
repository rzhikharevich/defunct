use std::slice;
use std::str;
use std::fmt;
use num;
use num::rational::BigRational;
use num::bigint::{BigInt, ToBigInt};
use unicode_segmentation::{UnicodeSegmentation, Graphemes};

#[derive(Copy, Clone, Debug)]
pub struct Location {
    pub line: u32,
    pub col: u32,
}

impl Location {
    pub fn null() -> Location {
        Location {line: 0, col: 0}
    }
}

#[derive(Clone, Debug)]
pub enum Token<'a> {
    Return,
    Let,
    Ident(&'a str),
    NumLit(BigRational),
    StrLit(Vec<StrLitItem<'a>>),
    Eq,
    Plus,
    Minus,
    Star,
    Slash,
    ArrowUp,
    LeftParen,
    RightParen,
    // Comma,
    Dollar,
    Dot,
    Indent,
    Dedent,
    Error(ErrorInfo),
}

use self::Token::*;

impl<'a> Token<'a> {
    pub fn is_error(&self) -> bool {
        match self {
            &Token::Error(_) => true,
            _ => false,
        }
    }

    pub fn is_expr_object(&self) -> bool {
        match self {
            &Token::Ident(_) | &Token::NumLit(_) | &Token::StrLit(_) => true,
            _ => false,
        }
    }
}

impl<'a> Token<'a> {
    pub fn is_ok(&self) -> bool {
        match *self {
            Error(_) => false,
            _ => true,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum StrLitItem<'a> {
    Str(&'a str),
    Esc(char),
}

#[derive(Copy, Clone, Debug)]
pub enum ErrorInfo {
    Expected(&'static [AtomKind]),
    BadIndent,
}

use self::ErrorInfo::*;

impl fmt::Display for ErrorInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Expected(kinds) => ::utils::write_with_commas_and_or(f, kinds),
            &BadIndent => write!(f, "bad indentation"),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum AtomKind {
    Char(char),
    Ws,
    Punct,
}

impl fmt::Display for AtomKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &AtomKind::Char(ch) => write!(f, "'{}'", ch),
            &AtomKind::Ws => write!(f, "whitespace"),
            &AtomKind::Punct => write!(f, "punctuation"),
        }
    }
}

#[derive(Clone)]
pub struct Tokenizer<'a> {
    base: SubTokenizer<'a>,
    buf: Option<(bool, Location, Token<'a>)>,
    last_line: u32,
    indent_col: u32,
    indent_len: u32,
}

impl<'a> Tokenizer<'a> {
    pub fn new(string: &str, indent_len: u32) -> Tokenizer {
        Tokenizer {
            base: SubTokenizer::new(string),
            buf: None,
            last_line: 0,
            indent_col: 1,
            indent_len: indent_len,
        }
    }

    fn emit_indent(&mut self, loc: Location) -> Option<(Location, Token<'a>)> {
        if loc.line > self.last_line {
            if (loc.col - 1) % self.indent_len != 0 {
                return Some((loc, Error(BadIndent)));
            } else {
                let iloc = Location {
                    line: loc.line,
                    col: self.indent_col,
                };

                if loc.col > self.indent_col {
                    self.indent_col += self.indent_len;
                    return Some((iloc, Indent));
                } else if loc.col < self.indent_col {
                    self.indent_col -= self.indent_len;
                    return Some((iloc, Dedent));
                }
            }
        }

        return None;
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = (Location, Token<'a>);

    fn next(&mut self) -> Option<(Location, Token<'a>)> {
        let (reported, loc, tok) = opt!(
            self.buf.take().or_else(
                || self.base.next().map(|(l, t)| (false, l, t))
            )
        );

        if reported {
            self.last_line = loc.line;
            return Some((loc, tok));
        }
        
        match self.emit_indent(loc) {
            r @ Some(_) => {
                self.buf = Some((true, loc, tok));
                r
            }

            None => {
                self.last_line = loc.line;
                Some((loc, tok))
            }
        }
    }
}

#[derive(Clone)]
struct SubTokenizer<'a> {
    scanner: Scanner<'a>,
}

impl<'a> SubTokenizer<'a> {
    fn new(string: &str) -> SubTokenizer {
        SubTokenizer {
            scanner: Scanner::new(string),
        }
    }

    fn eof_3(&self) -> (Location, &'a str, char) {
        let s = self.scanner.as_str();
    
        (
            Location::null(),
            &s[s.len()..],
            '\0',
        )
    }

    fn next_3_or_eof<I>(&self, it: &mut I) -> I::Item
        where I: Iterator<Item=(Location, &'a str, char)>
    {
        it.next().unwrap_or(self.eof_3())
    }

    fn set_scan_next<I>(&mut self, it: &mut I)
        where I: Iterator<Item=(Location, &'a str, char)>
    {
        let (loc, s, _) = self.next_3_or_eof(it);
        self.scanner = Scanner::new(s).at(loc);
    }
}

impl<'a> Iterator for SubTokenizer<'a> {
    type Item = (Location, Token<'a>);

    fn next(&mut self) -> Option<(Location, Token<'a>)> {
        let mut it0 = self.scanner.clone()
            .skip_while(|&(_, _, ch)| is_ws(ch));
    
        let (tok_loc, tok_base, tok_ch0) = opt!(it0.next());

        Some((tok_loc, match tok_ch0 {
            _ if is_ident_start(tok_ch0) => {
                let mut it1 = it0.skip_while(|&(_, _, ch)| is_ident_cont(ch));

                let (tok_end_loc, tok_end_base, _) =
                    self.next_3_or_eof(&mut it1);

                self.scanner = Scanner::new(tok_end_base).at(tok_end_loc);

                let s = unsafe {clamp_str(tok_base, tok_end_base)};

                match s {
                    "return" => Return,
                    "let" => Let,
                    _ => Ident(s),
                }
            }

            _ if is_digit(tok_ch0) => {
                let mut it1 = it0.skip_while(|&(_, _, ch)| is_digit(ch));

                // fractional part or end
                let (fpoe_loc, fpoe_base, fpoe_ch0) =
                    self.next_3_or_eof(&mut it1);

                // integer part
                let ips = unsafe {clamp_str(tok_base, fpoe_base)};

                // fractional part
                let fps = if fpoe_ch0 == '.' {
                    let mut it2 = it1.skip_while(|&(_, _, ch)| is_digit(ch));

                    let (end_loc, end_base, end_ch0) =
                        self.next_3_or_eof(&mut it2);

                    self.scanner = Scanner::new(end_base).at(end_loc);

                    if
                        is_ws(end_ch0) ||
                        is_punct(end_ch0) ||
                        end_ch0 == '\0'
                    {
                        unsafe {clamp_str(&fpoe_base[1..], end_base)}
                    } else {
                        return Some((
                            tok_loc,
                            Error(Expected(EXP_WS_PUNCT))
                        ));
                    }
                } else if
                    is_ws(fpoe_ch0) ||
                    is_punct(fpoe_ch0) ||
                    fpoe_ch0 == '\0'
                {
                    self.scanner = Scanner::new(fpoe_base).at(fpoe_loc);
                    "0"
                } else {
                    let mut it2 = it1.skip_while(
                        |&(_, _, ch)| !is_ws(ch) && !is_punct(ch)
                    );

                    let (next_loc, next_base, _) =
                        self.next_3_or_eof(&mut it2);

                    self.scanner = Scanner::new(next_base).at(next_loc);

                    return Some((
                        tok_loc,
                        Error(Expected(EXP_DOT_WS))
                    ));
                };

                let ip = BigInt::parse_bytes(ips.as_bytes(), 10).unwrap();
                let fp = BigInt::parse_bytes(fps.as_bytes(), 10).unwrap();

                let denom = num::pow(
                    ToBigInt::to_bigint(&10).unwrap(),
                    fps.len(),
                );

                NumLit(BigRational::new(
                    ip * denom.clone() + fp,
                    denom,
                ))
            }

            // TODO: use a lookup table for these
            '=' => {self.set_scan_next(&mut it0); Eq}
            '+' => {self.set_scan_next(&mut it0); Plus}
            '-' => {self.set_scan_next(&mut it0); Minus}
            '*' => {self.set_scan_next(&mut it0); Star}
            '/' => {self.set_scan_next(&mut it0); Slash}
            '^' => {self.set_scan_next(&mut it0); ArrowUp}
            '$' => {self.set_scan_next(&mut it0); Dollar}
            '.' => {self.set_scan_next(&mut it0); Dot}
            '(' => {self.set_scan_next(&mut it0); LeftParen}
            ')' => {self.set_scan_next(&mut it0); RightParen}

            _ => unimplemented!(),
        }))
    }
}

fn is_ws(ch: char) -> bool {
    [' ', '\t', '\n'].contains(&ch)
}

fn is_alpha(ch: char) -> bool {
    match ch {
        'a' ... 'z' | 'A' ... 'Z' => true,
        _ => false,
    }
}

fn is_digit(ch: char) -> bool {
    match ch {
        '0' ... '9' => true,
        _ => false,
    }
}

fn is_alnum(ch: char) -> bool {
    is_alpha(ch) || is_digit(ch)
}

fn is_ident_start(ch: char) -> bool {
    is_alpha(ch) || ch == '_'
}

fn is_ident_cont(ch: char) -> bool {
    is_alnum(ch) || ch == '_' || ch == '\''
}

fn is_punct(ch: char) -> bool {
    // TODO: lookup table
    ['=', '+', '-', '*', '/', '^', '(', ')', ',', '$', '.'].contains(&ch)
}

unsafe fn clamp_str<'a>(a: &'a str, b: &'a str) -> &'a str {
    str::from_utf8_unchecked(slice::from_raw_parts(
        a.as_ptr(),
        b.as_ptr() as usize - a.as_ptr() as usize,
    ))
}

const EXP_WS_PUNCT: &'static [AtomKind] = &[AtomKind::Ws, AtomKind::Punct];
const EXP_DOT_WS: &'static [AtomKind] = &[AtomKind::Char('.'), AtomKind::Ws];

#[derive(Clone)]
struct Scanner<'a> {
    graphemes: Graphemes<'a>,
    loc: Location,
}

impl<'a> Scanner<'a> {
    fn new(string: &str) -> Scanner {
        Scanner {
            graphemes: string.graphemes(true),
            loc: Location {line: 1, col: 1},
        }
    }

    fn at(self, loc: Location) -> Scanner<'a> {
        Scanner {
            graphemes: self.graphemes,
            loc: loc,
        }
    }

    fn as_str(&self) -> &'a str {
        self.graphemes.as_str()
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = (Location, &'a str, char);

    fn next(&mut self) -> Option<(Location, &'a str, char)> {
        let mut s = self.graphemes.as_str();

        while let Some(grapheme) = self.graphemes.next() {
            match grapheme.chars().next().unwrap() {
                '\r' => {}

                '\n' => {
                    let r = (self.loc, s, '\n');

                    self.loc.line += 1;
                    self.loc.col = 1;

                    return Some(r);
                }

                ch => {
                    let r = (self.loc, s, ch);
                    
                    self.loc.col += 1;

                    return Some(r);
                }
            }

            s = self.graphemes.as_str();
        }

        None
    }
}
