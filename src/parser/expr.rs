use std::iter::Peekable;
use ::tok::{Location, Token};
use super::{Expr, BinOperator, BinOperation, Error};

type Precedence = u32;

#[derive(Debug)]
enum Associativity {
    Left,
    Right,
}

impl Associativity {
    fn is_right(&self) -> bool {
        match self {
            &Associativity::Right => true,
            _ => false,
        }
    }
}

enum Object<'a> {
    Expr(Expr<'a>),
    FnMark,
}

impl<'a> Object<'a> {
    fn expr(self) -> Option<Expr<'a>> {
        match self {
            Object::Expr(expr) => Some(expr),
            _ => None,
        }
    }

    fn is_fn_mark(&self) -> bool {
        match self {
            &Object::FnMark => true,
            _ => false,
        }
    }
}

enum Subject<'a> {
    LeftParen,
    Operator(BinOperator, Precedence, Associativity),
    FnExpr(Expr<'a>),
}

pub fn parse<'a, I>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<(Location, Error)>,
)
    -> Result<Expr<'a>, ()>
    where I: Iterator<Item=(Location, Token<'a>)>
{
    let mut objects = vec![];
    let mut subjects = vec![];

    let mut prev_is_op = true;
    let mut prev_may_be_fne = false;
    let mut prev_line = 0;

    while let Some((loc, tok)) = iter.peek().cloned() {
        if !prev_is_op && loc.line > prev_line {
            break;
        }

        let maybe_bin_op = bin_operator_for_token(&tok);

        match (maybe_bin_op, tok) {
            (Some((op, preced, assoc)), _) => {
                push_bin_operator(
                    &mut objects, &mut subjects,
                    op, preced, assoc,
                    // errors,
                );

                prev_is_op = true;
                prev_may_be_fne = false;
            }

            (None, Token::LeftParen) => subjects.push(Subject::LeftParen),

            (None, Token::RightParen) => {
                // TODO
                push_right_paren(
                    &mut objects, &mut subjects,
                    // errors,
                );

                prev_may_be_fne = true;
            }

            (None, tok) => {
                if tok.is_expr_object() {
                    push_expr(
                        &mut objects, &mut subjects,
                        prev_may_be_fne,
                        expr_for_token(tok).unwrap(),
                    );

                    prev_may_be_fne = prev_is_op;
                    prev_is_op = false;
                } else {
                    // TODO
                }
            }
        }

        prev_line = loc.line;

        iter.next();
    }

    while let Some(subject) = subjects.pop() {
        match subject {
            Subject::LeftParen => unimplemented!(),
            Subject::Operator(op, _, _) => push_bin_operation(&mut objects, op),
            Subject::FnExpr(fn_expr) => push_fn_appl(&mut objects, fn_expr),
        }
    }

    Ok(objects.pop().unwrap().expr().unwrap())
}

fn bin_operator_for_token<'a>(tok: &Token<'a>) ->
    Option<(BinOperator, Precedence, Associativity)>
{
    use self::Token::*;
    use self::BinOperator::*;
    use self::Associativity::*;
    
    Some(match tok {
        &Plus    => (Add,     1, Left),
        &Minus   => (Sub,     1, Left),
        &Star    => (Mul,     2, Left),
        &Slash   => (Div,     2, Left),
        &ArrowUp => (Pow,     3, Right),
        &Dollar  => (Apply,   0, Right),
        &Dot     => (Compose, 4, Right),
        _ => return None,
    })
}

fn expr_for_token<'a>(tok: Token<'a>) -> Result<Expr<'a>, ()> {
    Ok(match tok {
        Token::Ident(id)     => Expr::Ident(id),
        Token::NumLit(num)   => Expr::NumLit(num),
        Token::StrLit(items) => Expr::StrLit(items),
        _ => return Err(()),
    })
}

fn push_bin_operator<'a>(
    objects: &mut Vec<Object<'a>>,
    subjects: &mut Vec<Subject<'a>>,
    op: BinOperator,
    preced: Precedence,
    assoc: Associativity,
    // errors: Vec<(Location, Error)>,
)
    // -> Result<(), ()>
{
    while let Some(subject) = subjects.pop() {
        match subject {
            Subject::LeftParen => {
                subjects.push(subject);
                break;
            }

            Subject::Operator(opn, precedn, assocn) => {
                if precedn < preced || assocn.is_right() {
                    subjects.push(Subject::Operator(opn, precedn, assocn));
                    break;
                }

                push_bin_operation(objects, opn);
            }

            Subject::FnExpr(fn_expr) => {
                push_fn_appl(objects, fn_expr);
            }
        }
    }

    subjects.push(Subject::Operator(op, preced, assoc));
}

fn push_expr<'a>(
    objects: &mut Vec<Object<'a>>,
    subjects: &mut Vec<Subject<'a>>,
    prev_may_be_fne: bool,
    expr: Expr<'a>,
) {
    if prev_may_be_fne {
        let fn_expr = objects.pop().unwrap().expr().unwrap();
        objects.push(Object::FnMark);
        subjects.push(Subject::FnExpr(fn_expr));
    }

    objects.push(Object::Expr(expr));
}

fn push_right_paren<'a>(
    objects: &mut Vec<Object<'a>>,
    subjects: &mut Vec<Subject<'a>>,
) {
    while let Some(subject) = subjects.pop() {
        match subject {
            Subject::LeftParen => break,
            Subject::Operator(op, _, _) => push_bin_operation(objects, op),
            Subject::FnExpr(fn_expr) => push_fn_appl(objects, fn_expr),
        }
    }
}

fn push_bin_operation<'a>(
    objects: &mut Vec<Object<'a>>,
    op: BinOperator,
) {
    let right = objects.pop().unwrap().expr().unwrap();
    let left = objects.pop().unwrap().expr().unwrap();

    objects.push(Object::Expr(Expr::BinOperation(Box::new(BinOperation {
        operator: op,
        left: left,
        right: right,
    }))));
}

fn push_fn_appl<'a>(
    objects: &mut Vec<Object<'a>>,
    fn_expr: Expr<'a>,
) {
    let mark_idx = objects.iter().rposition(Object::is_fn_mark).unwrap();
    
    let args = objects
        .drain(mark_idx..)
        .skip(1)
        .map(|object| object.expr().unwrap())
        .collect();

    objects.push(Object::Expr(Expr::FnAppl {
        fn_expr: Box::new(fn_expr),
        args: args,
    }));
}
