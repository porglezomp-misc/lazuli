use std::borrow::Cow;

use ess::Sexp;
use ess::span::ByteSpan;

#[derive(Debug)]
pub enum AstError<'a> {
    ArgList(Sexp<'a>),
    ArgListNoName,
    ArgListName(Sexp<'a>),
    ArgListArg(Sexp<'a>),
    InvalidInCall(Sexp<'a>),
    StructName(Sexp<'a>),
    StructMember(Sexp<'a>),
}

impl<'a> AstError<'a> {
    pub fn to_owned(&self) -> AstError<'static> {
        use self::AstError::*;
        match *self {
            ArgList(ref s) => ArgList(s.to_owned()),
            ArgListNoName => ArgListNoName,
            ArgListName(ref s) => ArgListName(s.to_owned()),
            ArgListArg(ref s) => ArgListArg(s.to_owned()),
            InvalidInCall(ref s) => InvalidInCall(s.to_owned()),
            StructName(ref s) => StructName(s.to_owned()),
            StructMember(ref s) => StructMember(s.to_owned()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Ast<'a> {
    Call(Box<Ast<'a>>, Vec<Ast<'a>>, ByteSpan),
    If(Box<Ast<'a>>, Box<Ast<'a>>, Box<Ast<'a>>, ByteSpan),
    Let(Box<Ast<'a>>, Box<Ast<'a>>, ByteSpan),
    Begin(Vec<Ast<'a>>, ByteSpan),
    Loop(Vec<Ast<'a>>, ByteSpan),
    Return(Option<Box<Ast<'a>>>, ByteSpan),
    Break(Option<Box<Ast<'a>>>, ByteSpan),
    Continue(ByteSpan),
    Defn(Cow<'a, str>, Vec<Cow<'a, str>>, Vec<Ast<'a>>, ByteSpan),
    Struct(Cow<'a, str>, Vec<Cow<'a, str>>, ByteSpan),
    Var(Cow<'a, str>, ByteSpan),
    Literal(Cow<'a, Sexp<'a>>),
    Int(i64),
    Float(f64),
    Nil,
}

impl<'a> Ast<'a> {
    pub fn to_owned(&self) -> Ast<'static> {
        use self::Ast::*;
        match *self {
            Call(ref f, ref args, span) => Call(
                Box::new((**f).to_owned()),
                args.iter().map(Ast::to_owned).collect(),
                span
            ),
            If(ref cond, ref if_true, ref if_false, span) => If(
                Box::new((**cond).to_owned()),
                Box::new((**if_true).to_owned()),
                Box::new((**if_false).to_owned()),
                span,
            ),
            Let(ref var, ref val, span) => Let(
                Box::new((**var).to_owned()),
                Box::new((**val).to_owned()),
                span,
            ),
            Begin(ref body, span) => Begin(
                body.iter().map(Ast::to_owned).collect(),
                span,
            ),
            Loop(ref body, span) => Begin(
                body.iter().map(Ast::to_owned).collect(),
                span
            ),
            Return(None, span) => Return(None, span),
            Return(Some(ref val), span) => Return(
                Some(Box::new((**val).to_owned())),
                span,
            ),
            Break(None, span) => Break(None, span),
            Break(Some(ref val), span) => Break(
                Some(Box::new((**val).to_owned())),
                span,
            ),
            Continue(span) => Continue(span),
            Defn(ref name, ref args, ref body, span) => Defn(
                name.clone().into_owned().into(),
                args.iter().map(|arg| arg.clone().into_owned().into()).collect(),
                body.iter().map(Ast::to_owned).collect(),
                span,
            ),
            Struct(ref name, ref members, span) => Struct(
                name.clone().into_owned().into(),
                members.iter().map(|arg| arg.clone().into_owned().into()).collect(),
                span,
            ),
            Var(ref name, span) => Var(name.clone().into_owned().into(), span),
            Literal(ref lit) => Literal(
                Cow::Owned(lit.clone().into_owned().to_owned()),
            ),
            Int(i) => Int(i),
            Float(f) => Float(f),
            Nil => Nil,
        }
    }
}

fn defn_get_args<'a>(arg_list: &'a Sexp<'a>)
-> Result<(Cow<'a, str>, Vec<Cow<'a, str>>), AstError<'a>> {
    match *arg_list {
        Sexp::List(ref xs, ..) => {
            let name = match xs.first() {
                Some(&Sexp::Sym(ref s, ..)) => Ok(s),
                Some(s) => Err(AstError::ArgListName(s.clone())),
                None => Err(AstError::ArgListNoName),
            }?;
            let args = xs[1..].iter().map(|arg| {
                match *arg {
                    Sexp::Sym(ref s, ..) => Ok(s[..].into()),
                    ref s => Err(AstError::ArgListArg(s.clone())),
                }
            }).collect::<Result<_, _>>()?;
            Ok((name.clone(), args))
        }
        ref s => Err(AstError::ArgList(s.clone())),
    }
}

pub fn make_ast<'a>(sexp: &'a Sexp<'a>) -> Result<Ast<'a>, AstError<'a>> {
    match *sexp {
        Sexp::List(ref xs, span) => match xs.first() {
            Some(x) => match *x {
                Sexp::Sym(ref s, ..) if s == "if" => {
                    // TODO: Not too many args
                    let cond = make_ast(&xs[1])?;
                    let if_true = make_ast(&xs[2])?;
                    let if_false = xs.get(3).map(make_ast).unwrap_or(Ok(Ast::Nil))?;
                    Ok(Ast::If(
                        Box::new(cond), Box::new(if_true), Box::new(if_false),
                        span,
                    ))
                }
                Sexp::Sym(ref s, ..) if s == "let!" => {
                    // TODO: Not too many args
                    let var = make_ast(&xs[1])?;
                    let val = make_ast(&xs[2])?;
                    Ok(Ast::Let(
                        Box::new(var), Box::new(val),
                        span,
                    ))
                }
                Sexp::Sym(ref s, ..) if s == "begin" => {
                    let body = xs[1..].iter().map(make_ast)
                        .collect::<Result<_, _>>()?;
                    Ok(Ast::Begin(body, span))
                }
                Sexp::Sym(ref s, ..) if s == "loop" => {
                    let body = xs[1..].iter().map(make_ast)
                        .collect::<Result<_, _>>()?;
                    Ok(Ast::Loop(body, span))
                }
                Sexp::Sym(ref s, ..) if s == "return" => {
                    // TODO: Assert not too many args
                    let result = if let Some(res) = xs.get(1) {
                        Some(Box::new(make_ast(&res)?))
                    } else {
                        None
                    };
                    Ok(Ast::Return(result, span))
                }
                Sexp::Sym(ref s, ..) if s == "break" => {
                    // TODO: Assert not too many args
                    let result = if let Some(res) = xs.get(1) {
                        Some(Box::new(make_ast(&res)?))
                    } else {
                        None
                    };
                    Ok(Ast::Break(result, span))
                }
                Sexp::Sym(ref s, ..) if s == "continue" => {
                    // TODO: Assert not too many args
                    Ok(Ast::Continue(span))
                }
                Sexp::Sym(ref s, ..) if s == "defn" => {
                    let (name, args) = defn_get_args(&xs[1])?;
                    let body = xs[2..].iter()
                        .map(make_ast)
                        .collect::<Result<_, _>>()?;
                    Ok(Ast::Defn(name.into(), args.into(), body, span))
                }
                Sexp::Sym(ref s, ..) if s == "struct" => {
                    let name = match xs[1] {
                        Sexp::Sym(ref name, ..) => Ok(name),
                        ref s => {
                            Err(AstError::StructName(s.clone()))
                        }
                    }?;
                    let members = xs[2..].iter().map(|x| {
                        match *x {
                            Sexp::Sym(ref name, ..) => Ok(name[..].into()),
                            ref s => Err(AstError::StructMember(s.clone())),
                        }
                    }).collect::<Result<_, _>>()?;
                    Ok(Ast::Struct(name.clone(), members, span))
                }
                Sexp::Sym(ref s, ..) if s == "quote" => {
                    // TODO: Assert not too many args
                    Ok(Ast::Literal(Cow::Borrowed(&xs[1])))
                }
                Sexp::Sym(ref s, var_span) => {
                    let func = Ast::Var(Cow::Borrowed(s), var_span);
                    let args = xs[1..].iter()
                        .map(make_ast)
                        .collect::<Result<_, _>>()?;
                    Ok(Ast::Call(Box::new(func), args, span))
                }
                ref s @ Sexp::List(..) => {
                    let func = make_ast(s)?;
                    let args = xs[1..].iter()
                        .map(make_ast)
                        .collect::<Result<_, _>>()?;
                    Ok(Ast::Call(Box::new(func), args, span))
                }
                ref s => Err(AstError::InvalidInCall(s.clone())),
            },
            None => Ok(Ast::Nil),
        },
        Sexp::Sym(ref s, span) => Ok(Ast::Var(s.clone(), span)),
        ref s @ Sexp::Str(..) => Ok(Ast::Literal(Cow::Borrowed(s))),
        ref s @ Sexp::Char(..) => Ok(Ast::Literal(Cow::Borrowed(s))),
        Sexp::Int(i, ..) => Ok(Ast::Int(i)),
        Sexp::Float(f, ..) => Ok(Ast::Float(f)),
    }
}
