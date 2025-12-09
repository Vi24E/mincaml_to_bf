use crate::id;
use crate::syntax::{Fundef, Syntax};
use crate::ty::Type;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, digit1, multispace0, one_of},
    combinator::{all_consuming, map, map_res, opt, recognize, verify},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};

// --- Lexer Helpers ---

fn ws<'a, F, O, E: nom::error::ParseError<&'a str>>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_lowercase()
}

fn is_ident_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn ident(input: &str) -> IResult<&str, String> {
    map(
        verify(
            recognize(pair(
                take_while1(is_ident_start),
                take_while(is_ident_char),
            )),
            |s: &str| !matches!(s, "if" | "then" | "else" | "let" | "in" | "rec" | "true" | "false" | "not" | "Array.create" | "Array.make"),
        ),
        |s: &str| s.to_string(),
    )(input)
}

fn integer(input: &str) -> IResult<&str, i32> {
    map_res(digit1, |s: &str| s.parse::<i32>())(input)
}

fn float(input: &str) -> IResult<&str, f64> {
    map_res(
        alt((
            recognize(tuple((
                digit1,
                pair(char('.'), opt(digit1)),
                opt(pair(one_of("eE"), pair(opt(one_of("+-")), digit1))),
            ))),
            recognize(tuple((
                digit1,
                pair(one_of("eE"), pair(opt(one_of("+-")), digit1)),
            ))),
        )),
        |s: &str| s.parse::<f64>(),
    )(input)
}

// --- Parser ---

// Forward declaration for recursive parsers
fn exp(input: &str) -> IResult<&str, Syntax> {
    alt((
        let_rec_def,
        let_tuple_def,
        let_def,
        exp_assign_or_semicolon,
    ))(input)
}

fn simple_exp(input: &str) -> IResult<&str, Syntax> {
    alt((
        map(ws(tag("()")), |_| Syntax::Unit),
        delimited(ws(tag("(")), exp, ws(tag(")"))),
        map(ws(tag("true")), |_| Syntax::Bool(true)),
        map(ws(tag("false")), |_| Syntax::Bool(false)),
        map(ws(float), Syntax::Float),
        map(ws(integer), Syntax::Int),
        map(ws(ident), Syntax::Var),
    ))(input)
}

fn exp_access(input: &str) -> IResult<&str, Syntax> {
    let (input, e1) = simple_exp(input)?;
    let (input, accesses) = many0(preceded(
        ws(tag(".")),
        delimited(ws(tag("(")), exp, ws(tag(")"))),
    ))(input)?;
    
    let res = accesses.into_iter().fold(e1, |acc, idx| Syntax::Get(Box::new(acc), Box::new(idx)));
    Ok((input, res))
}

fn exp_app(input: &str) -> IResult<&str, Syntax> {
    let (input, e1) = exp_access(input)?;
    let (input, args) = many0(simple_exp)(input)?;
    
    if args.is_empty() {
        Ok((input, e1))
    } else {
        Ok((input, Syntax::App(Box::new(e1), args)))
    }
}

fn array_create(input: &str) -> IResult<&str, Syntax> {
    tuple((
        preceded(ws(alt((tag("Array.create"), tag("Array.make")))), simple_exp),
        simple_exp,
    ))(input).map(|(input, (e1, e2))| {
        (input, Syntax::Array(Box::new(e1), Box::new(e2)))
    })
}

fn exp_app_fixed(input: &str) -> IResult<&str, Syntax> {
    alt((
        array_create,
        exp_app,
    ))(input)
}

fn exp_unary(input: &str) -> IResult<&str, Syntax> {
    alt((
        map(preceded(ws(tag("not")), exp_app_fixed), |e| Syntax::Not(Box::new(e))),
        map(preceded(ws(tag("-.")), exp_app_fixed), |e| Syntax::FNeg(Box::new(e))),
        map(preceded(ws(tag("-")), exp_app_fixed), |e| {
            match e {
                Syntax::Float(f) => Syntax::Float(-f),
                _ => Syntax::Neg(Box::new(e)),
            }
        }),
        exp_app_fixed,
    ))(input)
}

fn exp_mul(input: &str) -> IResult<&str, Syntax> {
    let (input, e1) = exp_unary(input)?;
    let (input, ops) = many0(pair(
        ws(alt((tag("*."), tag("/.")))),
        exp_unary
    ))(input)?;
    
    let res = ops.into_iter().fold(e1, |acc, (op, val)| {
        match op {
            "*." => Syntax::FMul(Box::new(acc), Box::new(val)),
            "/." => Syntax::FDiv(Box::new(acc), Box::new(val)),
            _ => unreachable!(),
        }
    });
    Ok((input, res))
}

fn exp_add(input: &str) -> IResult<&str, Syntax> {
    let (input, e1) = exp_mul(input)?;
    let (input, ops) = many0(pair(
        ws(alt((tag("+."), tag("-."), tag("+"), tag("-")))),
        exp_mul
    ))(input)?;
    
    let res = ops.into_iter().fold(e1, |acc, (op, val)| {
        match op {
            "+." => Syntax::FAdd(Box::new(acc), Box::new(val)),
            "-." => Syntax::FSub(Box::new(acc), Box::new(val)),
            "+" => Syntax::Add(Box::new(acc), Box::new(val)),
            "-" => Syntax::Sub(Box::new(acc), Box::new(val)),
            _ => unreachable!(),
        }
    });
    Ok((input, res))
}

fn exp_eq(input: &str) -> IResult<&str, Syntax> {
    let (input, e1) = exp_add(input)?;
    let (input, ops) = many0(pair(
        ws(alt((tag("="), tag("<>"), tag("<="), tag(">="), tag("<"), tag(">")))),
        exp_add
    ))(input)?;
    
    let res = ops.into_iter().fold(e1, |acc, (op, val)| {
        match op {
            "=" => Syntax::Eq(Box::new(acc), Box::new(val)),
            "<>" => Syntax::Not(Box::new(Syntax::Eq(Box::new(acc), Box::new(val)))),
            "<=" => Syntax::LE(Box::new(acc), Box::new(val)),
            ">=" => Syntax::LE(Box::new(val), Box::new(acc)),
            "<" => Syntax::Not(Box::new(Syntax::LE(Box::new(val), Box::new(acc)))),
            ">" => Syntax::Not(Box::new(Syntax::LE(Box::new(acc), Box::new(val)))),
            _ => unreachable!(),
        }
    });
    Ok((input, res))
}

fn exp_if(input: &str) -> IResult<&str, Syntax> {
    alt((
        map(tuple((
            preceded(ws(tag("if")), exp),
            preceded(ws(tag("then")), exp),
            preceded(ws(tag("else")), exp),
        )), |(e1, e2, e3)| Syntax::If(Box::new(e1), Box::new(e2), Box::new(e3))),
        exp_eq,
    ))(input)
}

fn exp_comma(input: &str) -> IResult<&str, Syntax> {
    let (input, mut exprs) = separated_list1(ws(tag(",")), exp_if)(input)?;
    if exprs.len() == 1 {
        Ok((input, exprs.remove(0)))
    } else {
        Ok((input, Syntax::Tuple(exprs)))
    }
}

fn let_def(input: &str) -> IResult<&str, Syntax> {
    tuple((
        preceded(ws(tag("let")), ws(ident)),
        preceded(ws(tag("=")), exp),
        preceded(ws(tag("in")), exp),
    ))(input).map(|(input, (name, val, body))| {
        (input, Syntax::Let((name, Type::gentyp()), Box::new(val), Box::new(body)))
    })
}

fn let_rec_def(input: &str) -> IResult<&str, Syntax> {
    tuple((
        preceded(ws(tag("let")), preceded(ws(tag("rec")), ws(ident))),
        many1(ws(ident)),
        preceded(ws(tag("=")), exp),
        preceded(ws(tag("in")), exp),
    ))(input).map(|(input, (name, args, val, body))| {
        (input, Syntax::LetRec(
            Fundef {
                name: (name, Type::gentyp()),
                args: args.into_iter().map(|a| (a, Type::gentyp())).collect(),
                body: Box::new(val),
            },
            Box::new(body),
        ))
    })
}

fn let_tuple_def(input: &str) -> IResult<&str, Syntax> {
    tuple((
        preceded(ws(tag("let")), delimited(ws(tag("(")), separated_list1(ws(tag(",")), ws(ident)), ws(tag(")")))),
        preceded(ws(tag("=")), exp),
        preceded(ws(tag("in")), exp),
    ))(input).map(|(input, (names, val, body))| {
        (input, Syntax::LetTuple(
            names.into_iter().map(|n| (n, Type::gentyp())).collect(),
            Box::new(val),
            Box::new(body),
        ))
    })
}

fn exp_assign_or_semicolon(input: &str) -> IResult<&str, Syntax> {
    let (input, e1) = exp_comma(input)?;
    
    // Check for `<-`
    if let Ok((input, _)) = ws::<_, _, nom::error::Error<&str>>(tag("<-"))(input) {
        if let Syntax::Get(arr, idx) = e1 {
             let (input, val) = exp(input)?;
             return Ok((input, Syntax::Put(arr, idx, Box::new(val))));
        }
    }
    
    // Check for `;`
    let (input, exprs) = many0(preceded(ws(tag(";")), exp))(input)?;
    let res = exprs.into_iter().fold(e1, |acc, e| {
        Syntax::Let((id::gentmp(&Type::Unit), Type::Unit), Box::new(acc), Box::new(e))
    });
    
    Ok((input, res))
}

pub fn parse(input: &str) -> IResult<&str, Syntax> {
    all_consuming(exp)(input)
}

