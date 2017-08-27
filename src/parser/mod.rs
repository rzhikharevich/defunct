use std::collections::HashMap;
use std::iter::Peekable;
use std::fmt;
use num::rational::BigRational;

use ::tok::ErrorInfo as TokErrorInfo;
use ::tok::{Location, Tokenizer, Token, StrLitItem};

mod expr;

#[derive(Debug)]
pub struct Fn<'a> {
    arg_ids: Vec<&'a str>,
    stmts: Vec<(Location, Stmt<'a>)>,
    ret_expr: (Location, Expr<'a>),
}

#[derive(Debug)]
pub enum Stmt<'a> {
    Fn(&'a str, Fn<'a>),
    TupleDt(Vec<&'a str>, Expr<'a>),
}

#[derive(Debug)]
pub enum Expr<'a> {
    Ident(&'a str),
    NumLit(BigRational),
    StrLit(Vec<StrLitItem<'a>>),

    FnAppl {
        fn_expr: Box<Expr<'a>>,
        args: Vec<Expr<'a>>,
    },

    BinOperation(Box<BinOperation<'a>>),
}

#[derive(Debug)]
pub struct BinOperation<'a> {
    operator: BinOperator,
    left: Expr<'a>,
    right: Expr<'a>,
}

#[derive(Copy, Clone, Debug)]
pub enum BinOperator {
    Apply, Compose,
    Mul, Div,
    Add, Sub,
    Pow,
}

pub enum Error {
    Expected(&'static [Expectation]),
    Tok(TokErrorInfo),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Error::Expected(exps) =>
                ::utils::write_with_commas_and_or(f, exps),
            &Error::Tok(info) => write!(f, "{}", info),
        }
    }
}

#[derive(Debug)]
pub enum Expectation {
    Ident,
    Eq,
    FnBody,
    Assignment,
    Fn,
    RetExpr,
    Dedent,
    EOF,
    Lit,
}

impl fmt::Display for Expectation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type ParserResult<'a> =
    Result<HashMap<&'a str, Fn<'a>>, Vec<(Location, Error)>>;

pub fn parse_str<'a>(s: &'a str) -> ParserResult<'a> {
    parse_iter(Tokenizer::new(s, 4))
}

pub fn parse_iter<'a, I>(iter_: I) -> ParserResult<'a>
    where I: Iterator<Item=(Location, Token<'a>)>
{
    let mut iter = iter_.peekable();

    let mut fns = HashMap::new();

    let mut errors = vec![];

    while let Some(_) = iter.peek() {
        if let Ok((id, f)) = parse_fn(&mut iter, &mut errors) {
            fns.insert(id, f);
        }
    }

    if errors.is_empty() {
        Ok(fns)
    } else {
        Err(errors)
    }
}

fn parse_fn<'a, I>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<(Location, Error)>,
)
    -> Result<(&'a str, Fn<'a>), ()>
    where I: Iterator<Item=(Location, Token<'a>)>
{
    let maybe_id = parse_id(iter, errors);

    let maybe_arg_ids = parse_ids_and_eq(iter, errors);

    let maybe_body = parse_fn_body(iter, errors);

    match (maybe_id, maybe_arg_ids, maybe_body) {
        (Ok(id), Ok(arg_ids), Ok((stmts, re_loc, ret_expr))) => Ok((id, Fn {
            arg_ids: arg_ids,
            stmts: stmts,
            ret_expr: (re_loc, ret_expr),
        })),

        _ => Err(())
    }
}

fn parse_ids_and_eq<'a, I>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<(Location, Error)>,
)
    -> Result<Vec<&'a str>, ()>
    where I: Iterator<Item=(Location, Token<'a>)>
{
    let mut ok = true;

    let mut ids = vec![];
    
    while let Some((loc, tok)) = iter.next() {
        match tok {
            Token::Eq => {
                return if ok {
                    Ok(ids)
                } else {
                    Err(())
                };
            }
            Token::Ident(id) => ids.push(id),
            _ => {
                errors.push((loc, Error::Expected(EXP_IDENT_EQ)));
                ok = false;
            }
        }
    }

    errors.push((Location::null(), Error::Expected(EXP_IDENT_EQ)));

    Err(())
}

fn parse_fn_body<'a, I>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<(Location, Error)>,
)
    -> Result<(Vec<(Location, Stmt<'a>)>, Location, Expr<'a>), ()>
    where I: Iterator<Item=(Location, Token<'a>)>
{
    match iter.peek() {
        Some(&(_, Token::Indent)) => parse_fn_body_multiline(iter, errors),
        Some(&(loc, _)) =>
            expr::parse(iter, errors).map(|e| (vec![], loc, e)),
        None => {
            errors.push((Location::null(), Error::Expected(EXP_FNBODY)));
            Err(())
        }
    }
}

fn parse_fn_body_multiline<'a, I>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<(Location, Error)>,
)
    -> Result<(Vec<(Location, Stmt<'a>)>, Location, Expr<'a>), ()>
    where I: Iterator<Item=(Location, Token<'a>)>
{
    iter.next();
    
    let mut stmts = vec![];
    
    loop {
        match iter.peek().cloned() {
            Some((loc, Token::Return)) => {
                iter.next();

                let ret = match expr::parse(iter, errors) {
                    Ok(expr) => Ok((stmts, loc, expr)),
                    Err(()) => Err(())
                };

                match iter.next() {
                    Some((_, Token::Dedent)) | None => {}
                    Some((loc, _)) => {
                        errors.push((loc, Error::Expected(EXP_DEDENT_EOF)));
                        return Err(());
                    }
                }

                return ret;
            }

            Some((loc, Token::Let)) => {
                iter.next();
                
                if let Ok((ids, expr)) = parse_tuple_dt(iter, errors) {
                    stmts.push((loc, Stmt::TupleDt(ids, expr)));
                }
            }

            Some((loc, Token::Ident(_))) => {
                if let Ok((id, f)) = parse_fn(iter, errors) {
                    stmts.push((loc, Stmt::Fn(id, f)));
                }
            }

            t @ _ => {
                let loc = t.map_or(Location::null(), |(l, _)| l);
                errors.push((loc, Error::Expected(EXP_ASSIGN_FN_RETEXPR)));
                // TODO: recover properly
                return Err(());
            }
        }
    }
}

fn parse_tuple_dt<'a, I>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<(Location, Error)>,
)
    -> Result<(Vec<&'a str>, Expr<'a>), ()>
    where I: Iterator<Item=(Location, Token<'a>)>
{
    let maybe_ids = parse_ids_and_eq(iter, errors);
    
    let maybe_expr = expr::parse(iter, errors);

    match (maybe_ids, maybe_expr) {
        (Ok(ids), Ok(expr)) => Ok((ids, expr)),
        _ => Err(()),
    }
}

fn parse_id<'a, I>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<(Location, Error)>,
)
    -> Result<(&'a str), ()>
    where I: Iterator<Item=(Location, Token<'a>)>
{
    match iter.next() {
        Some((_, Token::Ident(id))) => Ok(id),
        t @ _ => {
            let loc = t.map_or(Location::null(), |(l, _)| l);
            errors.push((loc, Error::Expected(EXP_IDENT)));
            Err(())
        }
    }
}

macro_rules! expectations {
    ($id:ident, $( $x:ident ),*) => (
        const $id: &'static [Expectation] = &[
            $(
                Expectation::$x,
            )*
        ];
    )
}

expectations!(EXP_IDENT, Ident);
expectations!(EXP_IDENT_EQ, Ident, Eq);
expectations!(EXP_FNBODY, FnBody);
expectations!(EXP_ASSIGN_FN_RETEXPR, Assignment, Fn, RetExpr);
expectations!(EXP_DEDENT_EOF, Dedent, EOF);
