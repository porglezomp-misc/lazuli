use std::rc::Rc;
use eval::{Val, EvalError, Type, type_of_term};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Prim {
    Plus,
    Minus,
    Times,
    Divide,
    Equals,
    NotEquals,
    LessEquals,
    GreaterEquals,
    Less,
    Greater,
    Print,
    MakeTuple,
    IndexTuple,
    MakeList,
}

impl Prim {
    pub fn call<'a>(&self, args: &Vec<Rc<Val<'a>>>) -> Result<Rc<Val<'a>>, EvalError> {
        use self::Prim::*;
        match *self {
            Plus => call_plus(args),
            Minus => call_minus(args),
            Times => call_times(args),
            Divide => call_divide(args),
            Equals => call_equals(args),
            NotEquals => call_not_equals(args),
            LessEquals => call_less_equals(args),
            GreaterEquals => call_greater_equals(args),
            Less => call_less(args),
            Greater => call_greater(args),
            Print => call_print(args),
            MakeTuple => call_make_tuple(args),
            IndexTuple => call_index_tuple(args),
            MakeList => call_make_list(args),
        }
    }
}


// Arithmetic Operators ////////////////////////////////////////////////////////

macro_rules! should_use_float {
    ($args:expr) => {{
        let mut use_float = false;
        for expr in $args {
            let ty = type_of_term(expr);
            if ty == Type::Float { use_float = true; }
            if ty != Type::Float && ty != Type::Int {
                return Err(EvalError::TypeError {
                    expected: Type::Float,
                    got: ty,
                });
            }
        }
        use_float
    }}
}

fn call_plus<'a>(args: &Vec<Rc<Val<'a>>>) -> Result<Rc<Val<'a>>, EvalError> {
    let use_float = should_use_float!(args);
    Ok(Rc::new(if use_float {
        Val::Float(args.iter().fold(0.0, |a, b| a + unwrap_as_float(b)))
    } else {
        Val::Int(args.iter().fold(0, |a, b| a + unwrap_as_int(b)))
    }))
}

fn call_minus<'a>(args: &Vec<Rc<Val<'a>>>) -> Result<Rc<Val<'a>>, EvalError> {
    let use_float = should_use_float!(args);
    Ok(Rc::new(if use_float {
        if args.len() == 0 {
            Val::Float(0.0)
        } else if args.len() == 1 {
            Val::Float(-unwrap_as_float(&args[0]))
        } else {
            Val::Float(args[1..].iter().fold(
                unwrap_as_float(&args[0]),
                |a, b| a - unwrap_as_float(b)
            ))
        }
    } else {
        if args.len() == 0 {
            Val::Int(0)
        } else if args.len() == 1 {
            Val::Int(-unwrap_as_int(&args[0]))
        } else {
            Val::Int(args[1..].iter().fold(
                unwrap_as_int(&args[0]),
                |a, b| a - unwrap_as_int(b)
            ))
        }
    }))
}

fn call_times<'a>(args: &Vec<Rc<Val<'a>>>) -> Result<Rc<Val<'a>>, EvalError> {
    let use_float = should_use_float!(args);
    Ok(Rc::new(if use_float {
        Val::Float(args.iter().fold(1.0, |a, b| a * unwrap_as_float(b)))
    } else {
        Val::Int(args.iter().fold(1, |a, b| a * unwrap_as_int(b)))
    }))
}

fn call_divide<'a>(args: &Vec<Rc<Val<'a>>>) -> Result<Rc<Val<'a>>, EvalError> {
    let _ = should_use_float!(args);
    Ok(Rc::new(if args.len() == 0 {
        Val::Float(1.0)
    } else if args.len() == 1 {
        Val::Float(1.0 / unwrap_as_float(&args[0]))
    } else {
        Val::Float(args[1..].iter().fold(
            unwrap_as_float(&args[0]),
            |a, b| a / unwrap_as_float(b)
        ))
    }))
}


// Relational Operators ////////////////////////////////////////////////////////

fn call_equals<'a>(args: &Vec<Rc<Val<'a>>>) -> Result<Rc<Val<'a>>, EvalError> {
    Ok(bool_to_term(args.iter().zip(&args[1..]).all(|(a, b)| a == b)))
}

fn call_not_equals<'a>(args: &Vec<Rc<Val<'a>>>) -> Result<Rc<Val<'a>>, EvalError> {
    for (i, lhs) in args.iter().enumerate() {
        for (j, rhs) in args.iter().enumerate() {
            if i == j { continue; }
            if lhs == rhs { return Ok(bool_to_term(false)); }
        }
    }
    Ok(bool_to_term(true))
}

macro_rules! require_numeric_args {
    ($args:expr) => {
        for expr in $args {
            let ty = type_of_term(expr);
            if ty != Type::Float && ty != Type::Int {
                return Err(EvalError::TypeError {
                    expected: Type::Float,
                    got: ty,
                });
            }
        }
    }
}

fn call_less_equals<'a>(args: &Vec<Rc<Val<'a>>>) -> Result<Rc<Val<'a>>, EvalError> {
    require_numeric_args!(args);
    Ok(bool_to_term(args.iter()
                    .zip(&args[1..])
                    .all(|(a, b)| unwrap_as_float(a) <= unwrap_as_float(b))))
}

fn call_greater_equals<'a>(args: &Vec<Rc<Val<'a>>>) -> Result<Rc<Val<'a>>, EvalError> {
    require_numeric_args!(args);
    Ok(bool_to_term(args.iter()
                    .zip(&args[1..])
                    .all(|(a, b)| unwrap_as_float(a) >= unwrap_as_float(b))))

}

fn call_less<'a>(args: &Vec<Rc<Val<'a>>>) -> Result<Rc<Val<'a>>, EvalError> {
    require_numeric_args!(args);
    Ok(bool_to_term(args.iter().zip(&args[1..])
                    .all(|(a, b)| unwrap_as_float(a) < unwrap_as_float(b))))
}

fn call_greater<'a>(args: &Vec<Rc<Val<'a>>>) -> Result<Rc<Val<'a>>, EvalError> {
    require_numeric_args!(args);
    Ok(bool_to_term(args.iter().zip(&args[1..])
                    .all(|(a, b)| unwrap_as_float(a) > unwrap_as_float(b))))
}


// Print ///////////////////////////////////////////////////////////////////////

fn call_print<'a>(args: &Vec<Rc<Val<'a>>>) -> Result<Rc<Val<'a>>, EvalError> {
    for (i, expr) in args.iter().enumerate() {
        if i == args.len() - 1 {
            println!("{}", expr);
        } else {
            print!("{} ", expr);
        }
    }
    Ok(args.last().cloned().unwrap_or(Rc::new(Val::Nil)))
}


// Tuple Functions /////////////////////////////////////////////////////////////

fn call_make_tuple<'a>(args: &Vec<Rc<Val<'a>>>) -> Result<Rc<Val<'a>>, EvalError> {
    if args.len() == 0 {
        Ok(Rc::new(Val::Nil))
    } else {
        Ok(Rc::new(Val::Tuple(args.clone())))
    }
}

fn call_index_tuple<'a>(args: &Vec<Rc<Val<'a>>>) -> Result<Rc<Val<'a>>, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::BadArity { expected: 2, got: args.len() });
    }

    let idx = match *args[0] {
        Val::Int(idx) => idx as usize,
        ref other => return Err(EvalError::TypeError {
            expected: Type::Int,
            got: type_of_term(other),
        })
    };

    let tup = match *args[1] {
        Val::Tuple(ref items) => items,
        ref other => return Err(EvalError::TypeError{
            expected: Type::Tuple,
            got: type_of_term(other),
        })
    };

    if tup.len() <= idx {
        return Err(EvalError::OutOfBounds {
            index: idx,
            length: tup.len(),
        })
    }

    Ok(tup[idx].clone())
}

fn call_make_list<'a>(args: &Vec<Rc<Val<'a>>>) -> Result<Rc<Val<'a>>, EvalError> {
    Ok(args.iter().rev()
       .fold(Rc::new(Val::Nil),
             |cdr, car| Rc::new(Val::Tuple(vec![car.clone(), cdr]))))
}


// Conversion Helper Functions /////////////////////////////////////////////////

fn unwrap_as_float<'a>(val: &Rc<Val<'a>>) -> f64 {
    match **val {
        Val::Int(i) => i as f64,
        Val::Float(f) => f,
        _ => panic!("Only valid for Val::Int or Val::Float, got {:?}", val),
    }
}

fn unwrap_as_int(val: &Rc<Val>) -> i64 {
    match **val {
        Val::Int(i) => i,
        Val::Float(f) => f as i64,
        _ => panic!("Only valid for Val::Int or Val::Float, got {:?}", val),
    }
}

fn bool_to_term<'a>(p: bool) -> Rc<Val<'a>> {
    if p {
        Rc::new(Val::Sym("t".into()))
    } else {
        Rc::new(Val::Nil)
    }
}
