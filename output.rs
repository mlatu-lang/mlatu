#![feature(destructuring_assignment)]
#![allow(warnings)]
type StackFn = fn(Vec<Rep>, Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>);
#[derive(Debug, Clone)]
enum Rep {
    Name(StackFn),
    Closure(StackFn, Vec<Rep>),
    Algebraic(i64, Vec<Rep>),
    Char(char),
    Text(String),
    Int(i64),
    Float(f64),
    List(Vec<Rep>),
}
use Rep::*;
fn convertBool(b: bool, stack: Vec<Rep>, closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    if b {
        (stack, closures) = mtrue(stack, closures);
        (stack, closures)
    } else {
        (stack, closures) = mfalse(stack, closures);
        (stack, closures)
    }
}
fn convertOption(
    o: Option<Rep>,
    stack: Vec<Rep>,
    closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    if let Some(x) = o {
        stack.push(x);
        (stack, closures) = msome(stack, closures);
        (stack, closures)
    } else {
        (stack, closures) = mnone(stack, closures);
        (stack, closures)
    }
}
#[must_use]
#[inline]
fn mnip2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mdrop23a3a5bA2c20B5d(stack, closures);
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mor(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mtrue(stack, closures);
    stack.push(Name(m_3a3aor3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfold_left3a3a5bBool2c20Bool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3aassert3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m3e3a3a5bBool5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m263a3a5bBool5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mexp(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        stack.push(Float(a.exp()));
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mid(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures)
}
#[must_use]
#[inline]
fn m3d3a3a5bInt5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Int(a)) = x {
        let x = stack.pop();
        if let Some(Int(b)) = x {
            (stack, closures) = convertBool(a == b, stack, closures);
        } else {
            panic!("Expected `Some(Int(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Int(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mfold_left(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mtail_head3a3a5bB5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bList5bB5d2c20B5d(stack, closures);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mfold_left3a3a5bA2c20B5d(stack, closures);
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn masin(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        stack.push(Float(a.asin()));
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3aset3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = m3d3a3a5bInt5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                stack.push(closures.last().unwrap().clone()[1].clone());
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m7e3a3a5bBool5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bBool2c20Bool5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = mnot3a3a5bBool5d(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mnot3a3a5bInt5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Int(a)) = x {
        stack.push(Int(!a));
    } else {
        panic!("Expected `Some(Int(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap_reduce_left2(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mmap23a3a5bA2c20B2c20T5d(stack, closures);
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mreduce_left3a3a5bT5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mfalse(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mempty(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        (stack, closures) = convertBool(a.is_empty(), stack, closures);
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m2d3a3a5bInt5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Int(a)) = x {
        let x = stack.pop();
        if let Some(Int(b)) = x {
            stack.push(Int(a - b));
        } else {
            panic!("Expected `Some(Int(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Int(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3amap_index3a3ahelper(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail3a3a5bA5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bA2c20List5bA5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                stack.push(locals[locals.len() - 4].clone());
                stack.push(Int(1));
                (stack, closures) = m2b3a3a5bInt5d(stack, closures);
                (stack, closures) = m_3a3amap_index3a3ahelper3a3a5bA2c20T5d(stack, closures);
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 4].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = mprefix3a3a5bT5d(stack, closures);
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mfind(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mfilter_in3a3a5bT5d(stack, closures);
    (stack, closures) = mhead3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mtri2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 3].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 4].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 5].clone());
    (stack, closures) = mcall(stack, closures);
    locals.pop();
    locals.pop();
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m3d3a3a5bList5bInt5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mlength3a3a5bInt5d(stack, closures);
    stack.push(locals[locals.len() - 2].clone());
    (stack, closures) = mlength3a3a5bInt5d(stack, closures);
    (stack, closures) = m3c3e3a3a5bInt5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = mhead_tail3a3a5bInt5d(stack, closures);
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                            (stack, closures) = mtrue(stack, closures);
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            (stack, closures) = munpair3a3a5bInt2c20List5bInt5d5d(stack, closures);
                            locals.push(stack.pop().unwrap());
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 4].clone());
                            (stack, closures) = mhead_tail3a3a5bInt5d(stack, closures);
                            let x = stack.pop();
                            if let Some(a) = x {
                                match a {
                                    Algebraic(0, fields) => {
                                        stack.extend(fields);
                                        (stack, closures) = mtrue(stack, closures);
                                    }
                                    Algebraic(1, fields) => {
                                        stack.extend(fields);
                                        (stack, closures) =
                                            munpair3a3a5bInt2c20List5bInt5d5d(stack, closures);
                                        locals.push(stack.pop().unwrap());
                                        locals.push(stack.pop().unwrap());
                                        stack.push(locals[locals.len() - 3].clone());
                                        stack.push(locals.last().unwrap().clone());
                                        (stack, closures) = m3c3e3a3a5bInt5d(stack, closures);
                                        let x = stack.pop();
                                        if let Some(a) = x {
                                            match a {
                                                Algebraic(0, fields) => {
                                                    stack.extend(fields);
                                                    stack.push(locals[locals.len() - 4].clone());
                                                    stack.push(locals[locals.len() - 2].clone());
                                                    (stack, closures) =
                                                        m3d3a3a5bList5bInt5d5d(stack, closures);
                                                }
                                                Algebraic(1, fields) => {
                                                    stack.extend(fields);
                                                    (stack, closures) = mfalse(stack, closures);
                                                }
                                                _ => {
                                                    (stack, closures) = mabort(stack, closures);
                                                }
                                            };
                                        } else {
                                            panic!("Expected `Some(a)`, but found `{:?}`", x);
                                        }
                                        locals.pop();
                                        locals.pop();
                                    }
                                    _ => {
                                        (stack, closures) = mabort(stack, closures);
                                    }
                                };
                            } else {
                                panic!("Expected `Some(a)`, but found `{:?}`", x);
                            }
                            locals.pop();
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = mfalse(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3ainsert_nth3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = m3d3a3a5bInt5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                let v = vec![stack.pop().unwrap()];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                stack.push(closures.last().unwrap().clone()[1].clone());
                stack.push(locals.last().unwrap().clone());
                let v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
                stack.push(List(v));
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m_pointer(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn m3c3e3a3a5bInt5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3d3a3a5bInt5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3apartition3a3alambda1(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mfilter_out3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3aeach23a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair3a3a5bA2c20B5d(stack, closures);
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3acurry23a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    stack.push(closures.last().unwrap().clone()[1].clone());
    stack.push(closures.last().unwrap().clone()[2].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mwrite_file(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Text(a)) = x {
        let x = stack.pop();
        if let Some(Text(b)) = x {
            use std::io::Write;
            let mut file = std::fs::File::create(b).unwrap();
            file.write_all(a.as_bytes()).unwrap();
        } else {
            panic!("Expected `Some(Text(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Text(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail3a3a5bA5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bA2c20List5bA5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mmap3a3a5bA2c20T5d(stack, closures);
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = mprefix3a3a5bT5d(stack, closures);
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m2a3a3a5bInt5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Int(a)) = x {
        let x = stack.pop();
        if let Some(Int(b)) = x {
            stack.push(Int(a * b));
        } else {
            panic!("Expected `Some(Int(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Int(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn msqrt(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        stack.push(Float(a.sqrt()));
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap_reduce_right(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mmap3a3a5bA2c20T5d(stack, closures);
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mreduce_right3a3a5bT5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mswapped(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3aswapped3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mswap3a3a5b28R2e2e2e2c20A2c20B202d3eS2e2e2e202bP292c2028R2e2e2e2c20B2c20A202d3eR2e2e2e2c20A2c20B202bP295d(stack, closures);
    (stack, closures) = mcompose(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mlength(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Int(0));
    stack.push(Name(m_3a3alength3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfold_left3a3a5bInt2c20T5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m3c3d3a3a5bBool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3e3a3a5bBool5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mzero3a3a5bInt5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Int(0));
    (stack, closures)
}
#[must_use]
#[inline]
fn mread3a3a5bChar5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mchars(stack, closures);
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mlength3a3a5bChar5d(stack, closures);
    stack.push(Int(1));
    (stack, closures) = m3d3a3a5bInt5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                (stack, closures) = mnone3a3a5bChar5d(stack, closures);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = mhead3a3a5bChar5d(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mwhile(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mcall(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = mwhile(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m3e3d3a3a5bList5bInt5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3c3a3a5bList5bInt5d5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap_reduce_left(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mmap3a3a5bA2c20T5d(stack, closures);
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mreduce_left3a3a5bT5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3amaximum3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mmax3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn monce(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    let v = vec![stack.pop().unwrap()];
    stack.push(List(v));
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m3c3d3a3a5bDouble5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3e3a3a5bDouble5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mcosh(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        stack.push(Float(a.cosh()));
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mreduce_right(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = minit_last3a3a5bT5d(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3areduce_right3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mmap_optional3a3a5bPair5bList5bT5d5d5bT5d2c20T5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mnip(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mdrop3a3a5bA5d(stack, closures);
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn meither(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
                (stack, closures) = mcall(stack, closures);
            }
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = mcall(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mtri(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 3].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 4].clone());
    (stack, closures) = mcall(stack, closures);
    locals.pop();
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3alefts3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mget_left3a3a5bA2c20B5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mfix(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3afix3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mcall(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn msuffix(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = monce3a3a5bT5d(stack, closures);
    (stack, closures) = mappend3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m3c3d3a3a5bString5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3e3a3a5bString5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mappend_file(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Text(a)) = x {
        let x = stack.pop();
        if let Some(Text(b)) = x {
            use std::io::Write;
            let mut file = std::fs::File::open(b).unwrap();
            file.write_all(a.as_bytes()).unwrap();
        } else {
            panic!("Expected `Some(Text(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Text(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mis_left(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let _ = stack.pop().unwrap();
                (stack, closures) = mtrue(stack, closures);
            }
            _ => {
                (stack, closures) = mfalse(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mremove(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3aremove3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfilter_out3a3a5bT5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn many(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mtrue(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3aany3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfold_left3a3a5bBool2c20T5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m3d3a3a5bChar5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Char(a)) = x {
        let x = stack.pop();
        if let Some(Char(b)) = x {
            (stack, closures) = convertBool(a == b, stack, closures);
        } else {
            panic!("Expected `Some(Char(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Char(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3aand3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m263a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mover(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m3e3a3a5bDouble5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        let x = stack.pop();
        if let Some(Float(b)) = x {
            (stack, closures) = convertBool(a > b, stack, closures);
        } else {
            panic!("Expected `Some(Float(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mIO(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Closure(n, rs)) = x {
        closures.push(rs);
        (stack, closures) = n(stack, closures);
        closures.pop();
    } else {
        panic!("Expected `Some(Closure(n, rs))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn macosh(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        stack.push(Float(a.acosh()));
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mfold_right(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = minit_last3a3a5bA5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bList5bA5d2c20A5d(stack, closures);
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mfold_right3a3a5bA2c20B5d(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m7c3a3a5bInt5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Int(a)) = x {
        let x = stack.pop();
        if let Some(Int(b)) = x {
            stack.push(Int(a | b));
        } else {
            panic!("Expected `Some(Int(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Int(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mtrue(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![];
    v.reverse();
    stack.push(Algebraic(1, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn m3e3a3a5bString5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Text(a)) = x {
        let x = stack.pop();
        if let Some(Text(b)) = x {
            (stack, closures) = convertBool(a > b, stack, closures);
        } else {
            panic!("Expected `Some(Text(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Text(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m3c3a3a5bInt5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bInt2c20Int5d(stack, closures);
    (stack, closures) = m3e3a3a5bInt5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn macos(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        stack.push(Float(a.acos()));
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mfrom_right(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
            }
            _ => {
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = mfail(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m3c3e3a3a5bBool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3d3a3a5bBool5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3aremove3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures)
}
#[must_use]
#[inline]
fn m2f3a3a5bDouble5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        let x = stack.pop();
        if let Some(Float(b)) = x {
            stack.push(Float(a / b));
        } else {
            panic!("Expected `Some(Float(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn melem(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bT2c20List5bT5d5d(stack, closures);
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3aelem3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = many3a3a5bT5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3aany3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures) = m7c3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mdup(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mis_some(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = mdrop3a3a5bT5d(stack, closures);
                (stack, closures) = mtrue(stack, closures);
            }
            _ => {
                (stack, closures) = mfalse(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mdrop(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let _ = stack.pop().unwrap();
    (stack, closures)
}
#[must_use]
#[inline]
fn mpartition_eithers(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mlefts3a3a5bA2c20B5d(stack, closures);
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mrights3a3a5bA2c20B5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m3c3a3a5bString5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bString2c20String5d(stack, closures);
    (stack, closures) = m3e3a3a5bString5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mabs(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3acartesian_with3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mswap3a3a5bB2c20A5d(stack, closures);
    stack.push(closures.last().unwrap().clone()[1].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m3e3d3a3a5bChar5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3c3a3a5bChar5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3ainit_last3a3alambda2(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mpair3a3a5bList5bT5d2c20T5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m7e3a3a5bInt5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Int(a)) = x {
        let x = stack.pop();
        if let Some(Int(b)) = x {
            stack.push(Int(a ^ b));
        } else {
            panic!("Expected `Some(Int(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Int(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mshow3a3a5bChar5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = monce3a3a5bChar5d(stack, closures);
    (stack, closures) = mfrom_chars(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3afilter_out3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3atail_head3a3alambda2(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mpair3a3a5bList5bT5d2c20T5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3ahead_tail3a3alambda2(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mpair3a3a5bT2c20List5bT5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn munzip(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mhead_tail3a3a5bPair5bA5d5bB5d5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) =
                    munpair3a3a5bPair5bA5d5bB5d2c20List5bPair5bA5d5bB5d5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = munpair3a3a5bA2c20B5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = munzip3a3a5bA2c20B5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mprefix3a3a5bA5d(stack, closures);
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mprefix3a3a5bB5d(stack, closures);
                locals.pop();
                locals.pop();
                locals.pop();
                locals.pop();
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mmodify(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 3].clone());
    (stack, closures) = mget3a3a5bT5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mset3a3a5bT5d(stack, closures);
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3alast_init3a3alambda2(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mpair3a3a5bT2c20List5bT5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m3e3a3a5bInt5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Int(a)) = x {
        let x = stack.pop();
        if let Some(Int(b)) = x {
            (stack, closures) = convertBool(a > b, stack, closures);
        } else {
            panic!("Expected `Some(Int(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Int(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3aall3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures) = m263a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mbi3(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 3].clone());
    stack.push(locals[locals.len() - 4].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 3].clone());
    stack.push(locals[locals.len() - 5].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 3].clone());
    stack.push(locals[locals.len() - 6].clone());
    (stack, closures) = mcall(stack, closures);
    locals.pop();
    locals.pop();
    locals.pop();
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3aassert_eq3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    stack.push(closures.last().unwrap().clone()[1].clone());
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap_optionally(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail3a3a5bA5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bA2c20List5bA5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                            stack.push(locals[locals.len() - 2].clone());
                            stack.push(locals[locals.len() - 3].clone());
                            (stack, closures) = mmap_optionally3a3a5bA2c20B5d(stack, closures);
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 3].clone());
                            stack.push(locals[locals.len() - 4].clone());
                            (stack, closures) = mmap_optionally3a3a5bA2c20B5d(stack, closures);
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = mprefix3a3a5bB5d(stack, closures);
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mkeep3(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 3].clone());
    stack.push(locals[locals.len() - 4].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 3].clone());
    locals.pop();
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mfloor(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        stack.push(Float(a.floor()));
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mlefts(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3alefts3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mmap_optionally3a3a5bEither5bA5d5bB5d2c20A5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mmaximum(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3amaximum3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mreduce_left3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m3c3a3a5bDouble5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bDouble2c20Double5d(stack, closures);
    (stack, closures) = m3e3a3a5bDouble5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mtail(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        (stack, closures) = convertOption(
            a.split_first().map(|(_, t)| List(t.to_vec())),
            stack,
            closures,
        );
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mtanh(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        stack.push(Float(a.tanh()));
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mceil(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        stack.push(Float(a.ceil()));
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mchars(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Text(a)) = x {
        stack.push(List(a.chars().map(Char).collect::<Vec<_>>()));
    } else {
        panic!("Expected `Some(Text(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3aany23a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair3a3a5bA2c20B5d(stack, closures);
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3areduce_left3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair3a3a5bList5bT5d2c20T5d(stack, closures);
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mfold_left3a3a5bT2c20T5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m3c3a3a5bBool5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bBool2c20Bool5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures) = m263a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m253a3a5bDouble5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        let x = stack.pop();
        if let Some(Float(b)) = x {
            stack.push(Float(a % b));
        } else {
            panic!("Expected `Some(Float(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap_right(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                (stack, closures) = mleft3a3a5bA2c20C5d(stack, closures);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = mright3a3a5bA2c20C5d(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m2d2d2d3e(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bBool2c2028R2e2e2e202d3eR2e2e2e2c20Bool202bP295d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                (stack, closures) = mcall(stack, closures);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) =
                    mdrop3a3a5b28R2e2e2e202d3eR2e2e2e2c20Bool202bP295d(stack, closures);
                (stack, closures) = mtrue(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mshow3a3a5bInt5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Int(a)) = x {
        stack.push(Text(format!("{:?}", a)));
    } else {
        panic!("Expected `Some(Int(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3aconcat3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mappend3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m7c3a3a5bBool5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bBool2c20Bool5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = mdrop3a3a5bBool5d(stack, closures);
                (stack, closures) = mtrue(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m3e3d3a3a5bInt5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3c3a3a5bInt5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m2b3a3a5bInt5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Int(a)) = x {
        let x = stack.pop();
        if let Some(Int(b)) = x {
            stack.push(Int(a + b));
        } else {
            panic!("Expected `Some(Int(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Int(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mand(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mtrue(stack, closures);
    stack.push(Name(m_3a3aand3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfold_left3a3a5bBool2c20Bool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mdrop2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mdrop3a3a5bB5d(stack, closures);
    (stack, closures) = mdrop3a3a5bA5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mdup2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3acartesian3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mpair3a3a5bA2c20B5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mzip(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mhead_tail3a3a5bA5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mhead_tail3a3a5bB5d(stack, closures);
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                            let v = vec![];
                            stack.push(List(v));
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 2].clone());
                            (stack, closures) = msecond3a3a5bA2c20List5bA5d5d(stack, closures);
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = msecond3a3a5bB2c20List5bB5d5d(stack, closures);
                            (stack, closures) = mzip3a3a5bA2c20B5d(stack, closures);
                            stack.push(locals[locals.len() - 2].clone());
                            (stack, closures) = mfirst3a3a5bA2c20List5bA5d5d(stack, closures);
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = mfirst3a3a5bB2c20List5bB5d5d(stack, closures);
                            (stack, closures) = mpair3a3a5bA2c20B5d(stack, closures);
                            (stack, closures) = mprefix3a3a5bPair5bA5d5bB5d5d(stack, closures);
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3acurry3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    stack.push(closures.last().unwrap().clone()[1].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mzero3a3a5bDouble5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Float(0.0));
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3atail_head3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mtail3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3ahead_tail3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mhead3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn minit(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        (stack, closures) = convertOption(
            a.split_last().map(|(_, i)| List(i.to_vec())),
            stack,
            closures,
        );
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn muntil(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = muntil(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3alast_init3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mlast3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mnone(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mpad_head(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mlength3a3a5bT5d(stack, closures);
    stack.push(locals[locals.len() - 2].clone());
    (stack, closures) = m3c3a3a5bInt5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = mlength3a3a5bT5d(stack, closures);
                (stack, closures) = m2d3a3a5bInt5d(stack, closures);
                (stack, closures) = mreplicate3a3a5bT5d(stack, closures);
                (stack, closures) = mprepend3a3a5bT5d(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mjoin(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    let v = vec![];
    stack.push(List(v));
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3ajoin3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfold_left3a3a5bList5bT5d2c20List5bT5d5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mfunction(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3afunction3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mmin(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mcall(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Closure(n, rs)) = x {
        closures.push(rs);
        (stack, closures) = n(stack, closures);
        closures.pop();
    } else {
        panic!("Expected `Some(Closure(n, rs))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn matan(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        stack.push(Float(a.atan()));
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m2a3a3a5bDouble5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        let x = stack.pop();
        if let Some(Float(b)) = x {
            stack.push(Float(a * b));
        } else {
            panic!("Expected `Some(Float(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mFail(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Closure(n, rs)) = x {
        closures.push(rs);
        (stack, closures) = n(stack, closures);
        closures.pop();
    } else {
        panic!("Expected `Some(Closure(n, rs))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mpartition(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3apartition3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3apartition3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mbi3a3a5bList5bT5d2c20List5bT5d2c20List5bT5d5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m3c3e3a3a5bString5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3d3a3a5bString5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mget_left(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                (stack, closures) = msome3a3a5bA5d(stack, closures);
            }
            _ => {
                (stack, closures) = mnone3a3a5bA5d(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mfail(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Text(a)) = x {
        panic!("Execution failure: {}", a);
    } else {
        panic!("Expected `Some(Text(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn matanh(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3ainit_last3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = minit3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m3c3e3a3a5bDouble5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3d3a3a5bDouble5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mshow3a3a5bString5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures)
}
#[must_use]
#[inline]
fn meach2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mzip3a3a5bA2c20B5d(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3aeach23a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = meach3a3a5bPair5bA5d5bB5d5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m2d2d3e3a3a5bBool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bBool2c20Bool5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = mdrop3a3a5bBool5d(stack, closures);
                (stack, closures) = mtrue(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mshow3a3a5bDouble5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        stack.push(Text(format!("{:?}", a)));
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3afunction3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures)
}
#[must_use]
#[inline]
fn m253a3a5bInt5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Int(a)) = x {
        let x = stack.pop();
        if let Some(Int(b)) = x {
            stack.push(Int(a % b));
        } else {
            panic!("Expected `Some(Int(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Int(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mappend3(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mappend3a3a5bT5d(stack, closures);
    (stack, closures) = mappend3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m2b3a3a5bDouble5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        let x = stack.pop();
        if let Some(Float(b)) = x {
            stack.push(Float(a + b));
        } else {
            panic!("Expected `Some(Float(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m263a3a5bBool5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bBool2c20Bool5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = mdrop3a3a5bBool5d(stack, closures);
                (stack, closures) = mfalse(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mfrom_some(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
            }
            _ => {
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = mfail(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn masinh(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        stack.push(Float(a.asinh()));
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mcurry2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 3].clone());
    stack.push(Name(m_3a3acurry23a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap_optional(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = msome3a3a5bB5d(stack, closures);
                locals.pop();
            }
            _ => {
                (stack, closures) = mnone3a3a5bB5d(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m2b3a3a5bString5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Text(a)) = x {
        let x = stack.pop();
        if let Some(Text(b)) = x {
            let mut new_string = b;
            new_string.push_str(&a);
            stack.push(Text(new_string));
        } else {
            panic!("Expected `Some(Text(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Text(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3ajoin3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mglue3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap_index(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Int(0));
    (stack, closures) = m_3a3amap_index3a3ahelper3a3a5bA2c20B5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m3c3a3a5bChar5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bChar2c20Char5d(stack, closures);
    (stack, closures) = m3e3a3a5bChar5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m_list(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![
        stack.pop().unwrap(),
        stack.pop().unwrap(),
        stack.pop().unwrap(),
    ];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mdip(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mis_right(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                let _ = stack.pop().unwrap();
                (stack, closures) = mtrue(stack, closures);
            }
            _ => {
                (stack, closures) = mfalse(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3apartition3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mfilter_in3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mfib(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Int(2));
    (stack, closures) = m3c3a3a5bInt5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                stack.push(Int(2));
                (stack, closures) = m2d3a3a5bInt5d(stack, closures);
                (stack, closures) = mfib(stack, closures);
                stack.push(locals.last().unwrap().clone());
                stack.push(Int(1));
                (stack, closures) = m2d3a3a5bInt5d(stack, closures);
                (stack, closures) = mfib(stack, closures);
                (stack, closures) = m2b3a3a5bInt5d(stack, closures);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                stack.push(Int(1));
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m3e3d3a3a5bString5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3c3a3a5bString5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m3e3d3a3a5bDouble5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3c3a3a5bDouble5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mmax(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mtri3(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 3].clone());
    stack.push(locals[locals.len() - 4].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 3].clone());
    stack.push(locals[locals.len() - 5].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 3].clone());
    stack.push(locals[locals.len() - 6].clone());
    (stack, closures) = mcall(stack, closures);
    locals.pop();
    locals.pop();
    locals.pop();
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3aor3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m7c3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mread3a3a5bInt5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Text(a)) = x {
        (stack, closures) = convertOption(a.parse().ok().map(Int), stack, closures);
    } else {
        panic!("Expected `Some(Text(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn msinh(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        stack.push(Float(a.sinh()));
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mlog(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        stack.push(Float(a.log10()));
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mover2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 3].clone());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn msome(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(1, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mconcat(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let v = vec![];
    stack.push(List(v));
    stack.push(Name(m_3a3aconcat3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfold_left3a3a5bList5bT5d2c20List5bT5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mcartesian(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3acartesian3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mcartesian_with3a3a5bA2c20B2c20Pair5bA5d5bB5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mreduce_left(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mtail_head3a3a5bT5d(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3areduce_left3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mmap_optional3a3a5bPair5bList5bT5d5d5bT5d2c20T5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m3c3e3a3a5bList5bInt5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3d3a3a5bList5bInt5d5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn msin(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        stack.push(Float(a.sin()));
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn many2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mzip3a3a5bA2c20B5d(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3aany23a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = many3a3a5bPair5bA5d5bB5d5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3aget_all3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mswap3a3a5bInt2c20List5bT5d5d(stack, closures);
    (stack, closures) = mget3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mlast_init(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3alast_init3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(Name(m_3a3alast_init3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mbi3a3a5bList5bT5d2c20Optional5bT5d2c20Optional5bList5bT5d5d5d(stack, closures);
    stack.push(Name(m_3a3alast_init3a3alambda2));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mlift_optional_23a3a5bT2c20List5bT5d2c20Pair5bT5d5bList5bT5d5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mhead_tail(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3ahead_tail3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(Name(m_3a3ahead_tail3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mbi3a3a5bList5bT5d2c20Optional5bT5d2c20Optional5bList5bT5d5d5d(stack, closures);
    stack.push(Name(m_3a3ahead_tail3a3alambda2));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mlift_optional_23a3a5bT2c20List5bT5d2c20Pair5bT5d5bList5bT5d5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap_concat(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mmap3a3a5bA2c20List5bT5d5d(stack, closures);
    (stack, closures) = mconcat3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mcos(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        stack.push(Float(a.cos()));
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn minit_last(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3ainit_last3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(Name(m_3a3ainit_last3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mbi3a3a5bList5bT5d2c20Optional5bList5bT5d5d2c20Optional5bT5d5d(stack, closures);
    stack.push(Name(m_3a3ainit_last3a3alambda2));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mlift_optional_23a3a5bList5bT5d2c20T2c20Pair5bList5bT5d5d5bT5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mis_none(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                (stack, closures) = mtrue(stack, closures);
            }
            _ => {
                (stack, closures) = mfalse(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mfrom_optional(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
            }
            _ => {
                stack.push(locals.last().unwrap().clone());
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m2f3a3a5bInt5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Int(a)) = x {
        let x = stack.pop();
        if let Some(Int(b)) = x {
            stack.push(Int(a / b));
        } else {
            panic!("Expected `Some(Int(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Int(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m26263a3a5bBool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bBool2c2028R2e2e2e202d3eR2e2e2e2c20Bool202bP295d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                (stack, closures) =
                    mdrop3a3a5b28R2e2e2e202d3eR2e2e2e2c20Bool202bP295d(stack, closures);
                (stack, closures) = mfalse(stack, closures);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = mcall(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mcurry(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(Name(m_3a3acurry3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m7c7c3a3a5bBool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bBool2c2028R2e2e2e202d3eR2e2e2e2c20Bool202bP295d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                (stack, closures) = mcall(stack, closures);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) =
                    mdrop3a3a5b28R2e2e2e202d3eR2e2e2e2c20Bool202bP295d(stack, closures);
                (stack, closures) = mtrue(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3acompose3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(closures.last().unwrap().clone()[1].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mget_line(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    stack.push(Text(buf));
    (stack, closures)
}
#[must_use]
#[inline]
fn mtail_head(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3atail_head3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(Name(m_3a3atail_head3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mbi3a3a5bList5bT5d2c20Optional5bList5bT5d5d2c20Optional5bT5d5d(stack, closures);
    stack.push(Name(m_3a3atail_head3a3alambda2));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mlift_optional_23a3a5bList5bT5d2c20T2c20Pair5bList5bT5d5d5bT5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m3c3e3a3a5bChar5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3d3a3a5bChar5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn meach(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail3a3a5bT5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bT2c20List5bT5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = meach3a3a5bT5d(stack, closures);
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mzip3a3a5bA2c20B5d(stack, closures);
    stack.push(Name(m_3a3amap23a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mcompose(stack, closures);
    (stack, closures) = mmap3a3a5bPair5bA5d5bB5d2c20C5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mshow3a3a5bOptional5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(Text("none".to_owned()));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                locals.push(stack.pop().unwrap());
                stack.push(Text("some ".to_owned()));
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = m2b3a3a5bString5d(stack, closures);
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mget(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Int(a)) = x {
        let x = stack.pop();
        if let Some(List(b)) = x {
            (stack, closures) = convertOption(b.get(a as usize).cloned(), stack, closures);
        } else {
            panic!("Expected `Some(List(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Int(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m3e3d3a3a5bBool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3c3a3a5bBool5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mappend(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        let x = stack.pop();
        if let Some(List(b)) = x {
            let mut new_vec = b.clone();
            new_vec.extend(a.into_iter());
            stack.push(List(new_vec));
        } else {
            panic!("Expected `Some(List(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mshow3a3a5bBool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(Text("false".to_owned()));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                stack.push(Text("true".to_owned()));
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3arights3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mget_right3a3a5bA2c20B5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn moptional(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
                (stack, closures) = mcall(stack, closures);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = mcall(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3aremove_nth3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = m3d3a3a5bInt5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                let v = vec![stack.pop().unwrap()];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mtrunc(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        stack.push(Float(a.trunc()));
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mminimum(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3aminimum3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mreduce_left3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3aall23a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair3a3a5bA2c20B5d(stack, closures);
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mneg3a3a5bInt5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(Int(0));
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = m2d3a3a5bInt5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn msurround(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mprepend3a3a5bT5d(stack, closures);
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mappend3a3a5bT5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn munit(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mconcat_optionals(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let v = vec![];
    stack.push(List(v));
    stack.push(Name(m_3a3aconcat_optionals3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfold_left3a3a5bList5bT5d2c20Optional5bT5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap_reduce_right2(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mmap23a3a5bA2c20B2c20T5d(stack, closures);
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mreduce_right3a3a5bT5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m3c3d3a3a5bInt5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3e3a3a5bInt5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mfilter_out(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3afilter_out3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfilter_in3a3a5bT5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mbi2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 3].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 4].clone());
    (stack, closures) = mcall(stack, closures);
    locals.pop();
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mask(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mprint3a3a5bString5d(stack, closures);
    use std::io::Write;
    std::io::stdout().flush().unwrap();
    (stack, closures) = mget_line(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3aconcat_optionals3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = msuffix3a3a5bT5d(stack, closures);
            }
            _ => {}
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mall(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mtrue(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3aall3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfold_left3a3a5bBool2c20T5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mkeep2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 3].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mprint(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Text(a)) = x {
        println!("{}", a);
    } else {
        panic!("Expected `Some(Text(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3amap23a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair3a3a5bA2c20B5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mcartesian_with(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(Name(m_3a3acartesian_with3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mmap_concat3a3a5bA2c20C5d(stack, closures);
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap_pair(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = munpair3a3a5bA2c20B5d(stack, closures);
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 4].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 5].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures) = mpair3a3a5bC2c20D5d(stack, closures);
    locals.pop();
    locals.pop();
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m3c3d3a3a5bList5bInt5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3e3a3a5bList5bInt5d5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mget_right(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = msome3a3a5bB5d(stack, closures);
            }
            _ => {
                (stack, closures) = mnone3a3a5bB5d(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3aminimum3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mmin3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mpick(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 3].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3acartesian_with3a3alambda1(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(closures.last().unwrap().clone()[0].clone());
    stack.push(locals.last().unwrap().clone());
    stack.push(closures.last().unwrap().clone()[1].clone());
    stack.push(Name(m_3a3acartesian_with3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mmap3a3a5bB2c20C5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mreplicate(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(Int(0));
    (stack, closures) = m3c3d3a3a5bInt5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(Int(1));
                (stack, closures) = m2d3a3a5bInt5d(stack, closures);
                (stack, closures) = mreplicate3a3a5bT5d(stack, closures);
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = msuffix3a3a5bT5d(stack, closures);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn massert_eq(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 3].clone());
    stack.push(Name(m_3a3aassert_eq3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = massert(stack, closures);
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mpi(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Float(3.141592653589793));
    (stack, closures)
}
#[must_use]
#[inline]
fn mread3a3a5bBool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Text("true".to_owned()));
    (stack, closures) = m3d3a3a5bString5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                stack.push(Text("false".to_owned()));
                (stack, closures) = m3d3a3a5bString5d(stack, closures);
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                            (stack, closures) = mnone3a3a5bBool5d(stack, closures);
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            (stack, closures) = mfalse(stack, closures);
                            (stack, closures) = msome3a3a5bBool5d(stack, closures);
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = mtrue(stack, closures);
                (stack, closures) = msome3a3a5bBool5d(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn matan2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        let x = stack.pop();
        if let Some(Float(b)) = x {
            stack.push(Float(a.atan2(b)));
        } else {
            panic!("Expected `Some(Float(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mhead(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        (stack, closures) = convertOption(a.first().cloned(), stack, closures);
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mneg3a3a5bDouble5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(Float(0.0));
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = m2d3a3a5bDouble5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mget_all(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bList5bT5d2c20List5bInt5d5d(stack, closures);
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3aget_all3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mmap3a3a5bInt2c20Optional5bT5d5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn msecond(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair3a3a5bA2c20B5d(stack, closures);
    (stack, closures) = mswap3a3a5bA2c20B5d(stack, closures);
    (stack, closures) = mdrop3a3a5bA5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mprefix(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = monce3a3a5bT5d(stack, closures);
    (stack, closures) = mprepend3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mfilter_in(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mtail_head3a3a5bT5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bList5bT5d2c20T5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mfilter_in3a3a5bT5d(stack, closures);
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            stack.push(locals[locals.len() - 2].clone());
                            (stack, closures) = mprefix3a3a5bT5d(stack, closures);
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mExit(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Closure(n, rs)) = x {
        closures.push(rs);
        (stack, closures) = n(stack, closures);
        closures.pop();
    } else {
        panic!("Expected `Some(Closure(n, rs))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mcompose(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(Name(m_3a3acompose3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m3e3a3a5bList5bInt5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mhead_tail3a3a5bInt5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                (stack, closures) = mfalse(stack, closures);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bInt2c20List5bInt5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mhead_tail3a3a5bInt5d(stack, closures);
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                            (stack, closures) = mtrue(stack, closures);
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            (stack, closures) = munpair3a3a5bInt2c20List5bInt5d5d(stack, closures);
                            locals.push(stack.pop().unwrap());
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 3].clone());
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = m3e3a3a5bInt5d(stack, closures);
                            let x = stack.pop();
                            if let Some(a) = x {
                                match a {
                                    Algebraic(0, fields) => {
                                        stack.extend(fields);
                                        stack.push(locals[locals.len() - 3].clone());
                                        stack.push(locals.last().unwrap().clone());
                                        (stack, closures) = m3c3a3a5bInt5d(stack, closures);
                                        let x = stack.pop();
                                        if let Some(a) = x {
                                            match a {
                                                Algebraic(0, fields) => {
                                                    stack.extend(fields);
                                                    stack.push(locals[locals.len() - 4].clone());
                                                    stack.push(locals[locals.len() - 2].clone());
                                                    (stack, closures) =
                                                        m3e3a3a5bList5bInt5d5d(stack, closures);
                                                }
                                                Algebraic(1, fields) => {
                                                    stack.extend(fields);
                                                    (stack, closures) = mfalse(stack, closures);
                                                }
                                                _ => {
                                                    (stack, closures) = mabort(stack, closures);
                                                }
                                            };
                                        } else {
                                            panic!("Expected `Some(a)`, but found `{:?}`", x);
                                        }
                                    }
                                    Algebraic(1, fields) => {
                                        stack.extend(fields);
                                        (stack, closures) = mtrue(stack, closures);
                                    }
                                    _ => {
                                        (stack, closures) = mabort(stack, closures);
                                    }
                                };
                            } else {
                                panic!("Expected `Some(a)`, but found `{:?}`", x);
                            }
                            locals.pop();
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mnot3a3a5bBool5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                (stack, closures) = mtrue(stack, closures);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = mfalse(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m263a3a5bInt5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Int(a)) = x {
        let x = stack.pop();
        if let Some(Int(b)) = x {
            stack.push(Int(a & b));
        } else {
            panic!("Expected `Some(Int(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Int(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap_left(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = mright3a3a5bC2c20B5d(stack, closures);
            }
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = mleft3a3a5bC2c20B5d(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mprepend(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bList5bT5d2c20List5bT5d5d(stack, closures);
    (stack, closures) = mappend3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mexit(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Int(i)) = x {
        std::process::exit(i as i32);
    } else {
        panic!("Expected `Some(Int(i))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mreverse(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mhead_tail3a3a5bT5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bT2c20List5bT5d5d(stack, closures);
                (stack, closures) = mreverse3a3a5bT5d(stack, closures);
                (stack, closures) = mswap3a3a5bT2c20List5bT5d5d(stack, closures);
                (stack, closures) = msuffix3a3a5bT5d(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m2d2d3e3a3a5bInt5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bInt2c20Int5d(stack, closures);
    (stack, closures) = mnot3a3a5bInt5d(stack, closures);
    (stack, closures) = m7c3a3a5bInt5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3aelem3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures)
}
#[must_use]
#[inline]
fn mright(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(1, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn m3d3a3a5bBool5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                (stack, closures) = mnot3a3a5bBool5d(stack, closures);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3aswapped3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bB2c20A5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m3d3a3a5bString5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Text(a)) = x {
        let x = stack.pop();
        if let Some(Text(b)) = x {
            (stack, closures) = convertBool(a == b, stack, closures);
        } else {
            panic!("Expected `Some(Text(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Text(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mleft(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3alength3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mdrop3a3a5bT5d(stack, closures);
    stack.push(Int(1));
    (stack, closures) = m2b3a3a5bInt5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mprintln(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mprint3a3a5bT5d(stack, closures);
    (stack, closures) = mnewline(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m2d3a3a5bDouble5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        let x = stack.pop();
        if let Some(Float(b)) = x {
            stack.push(Float(a - b));
        } else {
            panic!("Expected `Some(Float(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mkeep(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mrights(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3arights3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mmap_optionally3a3a5bEither5bA5d5bB5d2c20B5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mbi(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 3].clone());
    (stack, closures) = mcall(stack, closures);
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m3e3a3a5bChar5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Char(a)) = x {
        let x = stack.pop();
        if let Some(Char(b)) = x {
            (stack, closures) = convertBool(a > b, stack, closures);
        } else {
            panic!("Expected `Some(Char(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Char(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mtan(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        stack.push(Float(a.tan()));
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3ainit_last3a3alambda1(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mlast3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mread_file(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Text(a)) = x {
        stack.push(Text(std::fs::read_to_string(a).unwrap()));
    } else {
        panic!("Expected `Some(Text(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mflip(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair3a3a5bA2c20B5d(stack, closures);
    (stack, closures) = mswap3a3a5bA2c20B5d(stack, closures);
    (stack, closures) = mpair3a3a5bB2c20A5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mall2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mzip3a3a5bA2c20B5d(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3aall23a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mall3a3a5bPair5bA5d5bB5d5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mlift_optional_2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 2].clone());
                            stack.push(locals.last().unwrap().clone());
                            stack.push(locals[locals.len() - 5].clone());
                            (stack, closures) = mcall(stack, closures);
                            (stack, closures) = msome3a3a5bC5d(stack, closures);
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) = mnone3a3a5bC5d(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
            }
            _ => {
                (stack, closures) = mnone3a3a5bC5d(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3atail_head3a3alambda1(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mhead3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3ahead_tail3a3alambda1(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mtail3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mremove_nth(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3aremove_nth3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mmap_index3a3a5bT2c20List5bT5d5d(stack, closures);
    (stack, closures) = mconcat3a3a5bT5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m3d3a3a5bDouble5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        let x = stack.pop();
        if let Some(Float(b)) = x {
            (stack, closures) = convertBool(a == b, stack, closures);
        } else {
            panic!("Expected `Some(Float(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mglue(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mprepend3a3a5bT5d(stack, closures);
    (stack, closures) = mappend3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mshow3a3a5bEither5bA5d5bB5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
            }
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3alast_init3a3alambda1(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = minit3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mnot_elem(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = melem3a3a5bT5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
fn main() -> () {
    let mut stack = Vec::new();
    let mut locals: Vec<Rep> = Vec::new();
    let mut closures = Vec::new();
    stack.push(Int(15));
    (stack, closures) = mfib(stack, closures);
    (stack, closures) = mprintln3a3a5bInt5d(stack, closures);
}
#[must_use]
#[inline]
fn mround(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Float(a)) = x {
        stack.push(Float(a.round()));
    } else {
        panic!("Expected `Some(Float(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mfirst(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair3a3a5bA2c20B5d(stack, closures);
    (stack, closures) = mdrop3a3a5bB5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m3c3a3a5bList5bInt5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bList5bInt5d2c20List5bInt5d5d(stack, closures);
    (stack, closures) = m3e3a3a5bList5bInt5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mdrop3(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mdrop3a3a5bC5d(stack, closures);
    (stack, closures) = mdrop3a3a5bB5d(stack, closures);
    (stack, closures) = mdrop3a3a5bA5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mfrom_chars(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        stack.push(Text(
            a.iter()
                .filter_map(|e| if let Char(c) = e { Some(c) } else { None })
                .collect::<String>(),
        ));
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn massert(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(Name(m_3a3aassert3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    let x = stack.pop();
    if let Some(Closure(n, rs)) = x {
        closures.push(rs);
        (stack, closures) = n(stack, closures);
        closures.pop();
    } else {
        panic!("Expected `Some(Closure(n, rs))`, but found `{:?}`", x);
    }
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = mfail(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mpair(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mnewline(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Text("\n".to_owned()));
    (stack, closures) = mprint3a3a5bString5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m3c3d3a3a5bChar5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3e3a3a5bChar5d(stack, closures);
    (stack, closures) = mnot3a3a5bBool5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3afix3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mfix(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3areduce_right3a3alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair3a3a5bList5bT5d2c20T5d(stack, closures);
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mfold_right3a3a5bT2c20T5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mdup3(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 3].clone());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 3].clone());
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn minsert_nth(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3ainsert_nth3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mmap_index3a3a5bT2c20List5bT5d5d(stack, closures);
    (stack, closures) = mconcat3a3a5bT5d(stack, closures);
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mabort(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Text("abort called".to_owned()));
    (stack, closures) = mfail(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mread3a3a5bDouble5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Text(a)) = x {
        (stack, closures) = convertOption(a.parse().ok().map(Float), stack, closures);
    } else {
        panic!("Expected `Some(Text(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mlast(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        (stack, closures) = convertOption(a.last().cloned(), stack, closures);
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mset(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3aset3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mmap_index3a3a5bT2c20T5d(stack, closures);
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mread3a3a5bString5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = msome3a3a5bString5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mdiv_mod(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mfrom_left(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = mfail(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mpad_tail(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mlength3a3a5bT5d(stack, closures);
    stack.push(locals[locals.len() - 2].clone());
    (stack, closures) = m3c3a3a5bInt5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 3].clone());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = mlength3a3a5bT5d(stack, closures);
                (stack, closures) = m2d3a3a5bInt5d(stack, closures);
                (stack, closures) = mreplicate3a3a5bT5d(stack, closures);
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = mprepend3a3a5bT5d(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mprepend3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bList5bT5d2c20List5bT5d5d(stack, closures);
    (stack, closures) = mappend3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mreplicate3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(Int(0));
    (stack, closures) = m3c3d3a3a5bInt5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(Int(1));
                (stack, closures) = m2d3a3a5bInt5d(stack, closures);
                (stack, closures) = mreplicate3a3a5bT5d(stack, closures);
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = msuffix3a3a5bT5d(stack, closures);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mlength3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Int(0));
    stack.push(Name(m_3a3alength3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfold_left3a3a5bInt2c20T5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn msome3a3a5bString5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(1, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap_index3a3a5bT2c20T5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Int(0));
    (stack, closures) = m_3a3amap_index3a3ahelper3a3a5bT2c20T5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mconcat3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let v = vec![];
    stack.push(List(v));
    stack.push(Name(m_3a3aconcat3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfold_left3a3a5bList5bT5d2c20List5bT5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap_index3a3a5bT2c20List5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Int(0));
    (stack, closures) = m_3a3amap_index3a3ahelper3a3a5bList5bT5d2c20T5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mfold_right3a3a5bT2c20T5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = minit_last3a3a5bT5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bList5bT5d2c20T5d(stack, closures);
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mfold_right3a3a5bT2c20T5d(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bList5bT5d2c20T5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mprint3a3a5bString5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mshow3a3a5bString5d(stack, closures);
    let x = stack.pop();
    if let Some(Text(a)) = x {
        println!("{}", a);
    } else {
        panic!("Expected `Some(Text(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mdrop3a3a5bA5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let _ = stack.pop().unwrap();
    (stack, closures)
}
#[must_use]
#[inline]
fn mdrop3a3a5bB5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let _ = stack.pop().unwrap();
    (stack, closures)
}
#[must_use]
#[inline]
fn mdrop3a3a5bC5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let _ = stack.pop().unwrap();
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bList5bInt5d2c20List5bInt5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mdrop3a3a5bB5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let _ = stack.pop().unwrap();
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mprintln3a3a5bInt5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mprint3a3a5bInt5d(stack, closures);
    (stack, closures) = mnewline(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn melem3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bT2c20List5bT5d5d(stack, closures);
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3aelem3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = many3a3a5bT5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn minit3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        (stack, closures) = convertOption(
            a.split_last().map(|(_, i)| List(i.to_vec())),
            stack,
            closures,
        );
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mappend3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        let x = stack.pop();
        if let Some(List(b)) = x {
            let mut new_vec = b.clone();
            new_vec.extend(a.into_iter());
            stack.push(List(new_vec));
        } else {
            panic!("Expected `Some(List(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mprepend3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bList5bT5d2c20List5bT5d5d(stack, closures);
    (stack, closures) = mappend3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mconcat3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let v = vec![];
    stack.push(List(v));
    stack.push(Name(m_3a3aconcat3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfold_left3a3a5bList5bT5d2c20List5bT5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap_index3a3a5bT2c20List5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Int(0));
    (stack, closures) = m_3a3amap_index3a3ahelper3a3a5bList5bT5d2c20T5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mtail3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        (stack, closures) = convertOption(
            a.split_first().map(|(_, t)| List(t.to_vec())),
            stack,
            closures,
        );
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mhead3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        (stack, closures) = convertOption(a.first().cloned(), stack, closures);
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mnone3a3a5bC5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn msome3a3a5bC5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(1, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mall3a3a5bPair5bA5d5bB5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mtrue(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3aall3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfold_left3a3a5bBool2c20Pair5bA5d5bB5d5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mzip3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mhead_tail3a3a5bA5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mhead_tail3a3a5bB5d(stack, closures);
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                            let v = vec![];
                            stack.push(List(v));
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 2].clone());
                            (stack, closures) = msecond3a3a5bA2c20List5bA5d5d(stack, closures);
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = msecond3a3a5bB2c20List5bB5d5d(stack, closures);
                            (stack, closures) = mzip3a3a5bA2c20B5d(stack, closures);
                            stack.push(locals[locals.len() - 2].clone());
                            (stack, closures) = mfirst3a3a5bA2c20List5bA5d5d(stack, closures);
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = mfirst3a3a5bB2c20List5bB5d5d(stack, closures);
                            (stack, closures) = mpair3a3a5bA2c20B5d(stack, closures);
                            (stack, closures) = mprefix3a3a5bPair5bA5d5bB5d5d(stack, closures);
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mpair3a3a5bB2c20A5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mlast3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        (stack, closures) = convertOption(a.last().cloned(), stack, closures);
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap_optionally3a3a5bEither5bA5d5bB5d2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail3a3a5bEither5bA5d5bB5d5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) =
                    munpair3a3a5bEither5bA5d5bB5d2c20List5bEither5bA5d5bB5d5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                            stack.push(locals[locals.len() - 2].clone());
                            stack.push(locals[locals.len() - 3].clone());
                            (stack, closures) =
                                mmap_optionally3a3a5bEither5bA5d5bB5d2c20B5d(stack, closures);
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 3].clone());
                            stack.push(locals[locals.len() - 4].clone());
                            (stack, closures) =
                                mmap_optionally3a3a5bEither5bA5d5bB5d2c20B5d(stack, closures);
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = mprefix3a3a5bB5d(stack, closures);
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mprint3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Text(a)) = x {
        println!("{}", a);
    } else {
        panic!("Expected `Some(Text(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mdrop3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let _ = stack.pop().unwrap();
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bB2c20A5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bInt2c20Int5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn msuffix3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = monce3a3a5bT5d(stack, closures);
    (stack, closures) = mappend3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bT2c20List5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mreverse3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mhead_tail3a3a5bT5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bT2c20List5bT5d5d(stack, closures);
                (stack, closures) = mreverse3a3a5bT5d(stack, closures);
                (stack, closures) = mswap3a3a5bT2c20List5bT5d5d(stack, closures);
                (stack, closures) = msuffix3a3a5bT5d(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bT2c20List5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mhead_tail3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3ahead_tail3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(Name(m_3a3ahead_tail3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mbi3a3a5bList5bT5d2c20Optional5bT5d2c20Optional5bList5bT5d5d5d(stack, closures);
    stack.push(Name(m_3a3ahead_tail3a3alambda2));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mlift_optional_23a3a5bT2c20List5bT5d2c20Pair5bT5d5bList5bT5d5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mappend3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        let x = stack.pop();
        if let Some(List(b)) = x {
            let mut new_vec = b.clone();
            new_vec.extend(a.into_iter());
            stack.push(List(new_vec));
        } else {
            panic!("Expected `Some(List(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bList5bT5d2c20List5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mleft3a3a5bC2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mright3a3a5bC2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(1, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bInt2c20List5bInt5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mhead_tail3a3a5bInt5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3ahead_tail3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(Name(m_3a3ahead_tail3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mbi3a3a5bList5bInt5d2c20Optional5bInt5d2c20Optional5bList5bInt5d5d5d(stack, closures);
    stack.push(Name(m_3a3ahead_tail3a3alambda2));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mlift_optional_23a3a5bInt2c20List5bInt5d2c20Pair5bInt5d5bList5bInt5d5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mprefix3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = monce3a3a5bT5d(stack, closures);
    (stack, closures) = mprepend3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mfilter_in3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mtail_head3a3a5bT5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bList5bT5d2c20T5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mfilter_in3a3a5bT5d(stack, closures);
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            stack.push(locals[locals.len() - 2].clone());
                            (stack, closures) = mprefix3a3a5bT5d(stack, closures);
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bList5bT5d2c20T5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mtail_head3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3atail_head3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(Name(m_3a3atail_head3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mbi3a3a5bList5bT5d2c20Optional5bList5bT5d5d2c20Optional5bT5d5d(stack, closures);
    stack.push(Name(m_3a3atail_head3a3alambda2));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mlift_optional_23a3a5bList5bT5d2c20T2c20Pair5bList5bT5d5d5bT5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mprepend3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bList5bT5d2c20List5bT5d5d(stack, closures);
    (stack, closures) = mappend3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn monce3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    let v = vec![stack.pop().unwrap()];
    stack.push(List(v));
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mdrop3a3a5bA5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let _ = stack.pop().unwrap();
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap3a3a5bInt2c20Optional5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail3a3a5bOptional5bT5d5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) =
                    munpair3a3a5bOptional5bT5d2c20List5bOptional5bT5d5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mmap3a3a5bOptional5bT5d2c20Int5d(stack, closures);
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = mprefix3a3a5bInt5d(stack, closures);
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bList5bT5d2c20List5bInt5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn msome3a3a5bBool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(1, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn msome3a3a5bBool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(1, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mnone3a3a5bBool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn msuffix3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = monce3a3a5bT5d(stack, closures);
    (stack, closures) = mappend3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mreplicate3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(Int(0));
    (stack, closures) = m3c3d3a3a5bInt5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(Int(1));
                (stack, closures) = m2d3a3a5bInt5d(stack, closures);
                (stack, closures) = mreplicate3a3a5bT5d(stack, closures);
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = msuffix3a3a5bT5d(stack, closures);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap3a3a5bB2c20C5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail3a3a5bC5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bC2c20List5bC5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mmap3a3a5bC2c20B5d(stack, closures);
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = mprefix3a3a5bB5d(stack, closures);
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mmin3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mnone3a3a5bB5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn msome3a3a5bB5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(1, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mpair3a3a5bC2c20D5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap_concat3a3a5bA2c20C5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mmap3a3a5bC2c20List5bA5d5d(stack, closures);
    (stack, closures) = mconcat3a3a5bA5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mfold_left3a3a5bBool2c20T5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mtail_head3a3a5bT5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bList5bT5d2c20T5d(stack, closures);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mfold_left3a3a5bBool2c20T5d(stack, closures);
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn msuffix3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = monce3a3a5bT5d(stack, closures);
    (stack, closures) = mappend3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mprint3a3a5bString5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mshow3a3a5bString5d(stack, closures);
    let x = stack.pop();
    if let Some(Text(a)) = x {
        println!("{}", a);
    } else {
        panic!("Expected `Some(Text(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mfilter_in3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mtail_head3a3a5bT5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bList5bT5d2c20T5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mfilter_in3a3a5bT5d(stack, closures);
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            stack.push(locals[locals.len() - 2].clone());
                            (stack, closures) = mprefix3a3a5bT5d(stack, closures);
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mreduce_right3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = minit_last3a3a5bT5d(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3areduce_right3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mmap_optional3a3a5bPair5bList5bT5d5d5bT5d2c20T5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap23a3a5bA2c20B2c20T5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mzip3a3a5bA2c20B5d(stack, closures);
    stack.push(Name(m_3a3amap23a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mcompose(stack, closures);
    (stack, closures) = mmap3a3a5bPair5bA5d5bB5d2c20T5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mfold_left3a3a5bList5bT5d2c20Optional5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mtail_head3a3a5bOptional5bT5d5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) =
                    munpair3a3a5bList5bOptional5bT5d5d2c20Optional5bT5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mfold_left3a3a5bList5bT5d2c20Optional5bT5d5d(stack, closures);
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mappend3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        let x = stack.pop();
        if let Some(List(b)) = x {
            let mut new_vec = b.clone();
            new_vec.extend(a.into_iter());
            stack.push(List(new_vec));
        } else {
            panic!("Expected `Some(List(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mprepend3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bList5bT5d2c20List5bT5d5d(stack, closures);
    (stack, closures) = mappend3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mreduce_left3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mtail_head3a3a5bT5d(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3areduce_left3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mmap_optional3a3a5bPair5bList5bT5d5d5bT5d2c20T5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mget_right3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = msome3a3a5bB5d(stack, closures);
            }
            _ => {
                (stack, closures) = mnone3a3a5bB5d(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap3a3a5bPair5bA5d5bB5d2c20C5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail3a3a5bC5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bC2c20List5bC5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mmap3a3a5bC2c20Pair5bA5d5bB5d5d(stack, closures);
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = mprefix3a3a5bPair5bA5d5bB5d5d(stack, closures);
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mzip3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mhead_tail3a3a5bA5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mhead_tail3a3a5bB5d(stack, closures);
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                            let v = vec![];
                            stack.push(List(v));
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 2].clone());
                            (stack, closures) = msecond3a3a5bA2c20List5bA5d5d(stack, closures);
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = msecond3a3a5bB2c20List5bB5d5d(stack, closures);
                            (stack, closures) = mzip3a3a5bA2c20B5d(stack, closures);
                            stack.push(locals[locals.len() - 2].clone());
                            (stack, closures) = mfirst3a3a5bA2c20List5bA5d5d(stack, closures);
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = mfirst3a3a5bB2c20List5bB5d5d(stack, closures);
                            (stack, closures) = mpair3a3a5bA2c20B5d(stack, closures);
                            (stack, closures) = mprefix3a3a5bPair5bA5d5bB5d5d(stack, closures);
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn meach3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail3a3a5bT5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bT2c20List5bT5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = meach3a3a5bT5d(stack, closures);
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bT2c20List5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mhead_tail3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3ahead_tail3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(Name(m_3a3ahead_tail3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mbi3a3a5bList5bT5d2c20Optional5bT5d2c20Optional5bList5bT5d5d5d(stack, closures);
    stack.push(Name(m_3a3ahead_tail3a3alambda2));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mlift_optional_23a3a5bT2c20List5bT5d2c20Pair5bT5d5bList5bT5d5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mlift_optional_23a3a5bList5bT5d2c20T2c20Pair5bList5bT5d5d5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 2].clone());
                            stack.push(locals.last().unwrap().clone());
                            stack.push(locals[locals.len() - 5].clone());
                            (stack, closures) = mcall(stack, closures);
                            (stack, closures) =
                                msome3a3a5bPair5bList5bT5d5d5bT5d5d(stack, closures);
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) =
                                mnone3a3a5bPair5bList5bT5d5d5bT5d5d(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
            }
            _ => {
                (stack, closures) = mnone3a3a5bPair5bList5bT5d5d5bT5d5d(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mbi3a3a5bList5bT5d2c20Optional5bList5bT5d5d2c20Optional5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 3].clone());
    (stack, closures) = mcall(stack, closures);
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mdrop3a3a5b28R2e2e2e202d3eR2e2e2e2c20Bool202bP295d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let _ = stack.pop().unwrap();
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bBool2c2028R2e2e2e202d3eR2e2e2e2c20Bool202bP295d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mdrop3a3a5b28R2e2e2e202d3eR2e2e2e2c20Bool202bP295d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let _ = stack.pop().unwrap();
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bBool2c2028R2e2e2e202d3eR2e2e2e2c20Bool202bP295d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mlift_optional_23a3a5bList5bT5d2c20T2c20Pair5bList5bT5d5d5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 2].clone());
                            stack.push(locals.last().unwrap().clone());
                            stack.push(locals[locals.len() - 5].clone());
                            (stack, closures) = mcall(stack, closures);
                            (stack, closures) =
                                msome3a3a5bPair5bList5bT5d5d5bT5d5d(stack, closures);
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) =
                                mnone3a3a5bPair5bList5bT5d5d5bT5d5d(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
            }
            _ => {
                (stack, closures) = mnone3a3a5bPair5bList5bT5d5d5bT5d5d(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mbi3a3a5bList5bT5d2c20Optional5bList5bT5d5d2c20Optional5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 3].clone());
    (stack, closures) = mcall(stack, closures);
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mconcat3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let v = vec![];
    stack.push(List(v));
    stack.push(Name(m_3a3aconcat3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfold_left3a3a5bList5bT5d2c20List5bT5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap3a3a5bA2c20List5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail3a3a5bList5bT5d5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bList5bT5d2c20List5bList5bT5d5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mmap3a3a5bList5bT5d2c20A5d(stack, closures);
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = mprefix3a3a5bA5d(stack, closures);
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mlift_optional_23a3a5bT2c20List5bT5d2c20Pair5bT5d5bList5bT5d5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 2].clone());
                            stack.push(locals.last().unwrap().clone());
                            stack.push(locals[locals.len() - 5].clone());
                            (stack, closures) = mcall(stack, closures);
                            (stack, closures) =
                                msome3a3a5bPair5bT5d5bList5bT5d5d5d(stack, closures);
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) =
                                mnone3a3a5bPair5bT5d5bList5bT5d5d5d(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
            }
            _ => {
                (stack, closures) = mnone3a3a5bPair5bT5d5bList5bT5d5d5d(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mbi3a3a5bList5bT5d2c20Optional5bT5d2c20Optional5bList5bT5d5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 3].clone());
    (stack, closures) = mcall(stack, closures);
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mlift_optional_23a3a5bT2c20List5bT5d2c20Pair5bT5d5bList5bT5d5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 2].clone());
                            stack.push(locals.last().unwrap().clone());
                            stack.push(locals[locals.len() - 5].clone());
                            (stack, closures) = mcall(stack, closures);
                            (stack, closures) =
                                msome3a3a5bPair5bT5d5bList5bT5d5d5d(stack, closures);
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) =
                                mnone3a3a5bPair5bT5d5bList5bT5d5d5d(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
            }
            _ => {
                (stack, closures) = mnone3a3a5bPair5bT5d5bList5bT5d5d5d(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mbi3a3a5bList5bT5d2c20Optional5bT5d2c20Optional5bList5bT5d5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 3].clone());
    (stack, closures) = mcall(stack, closures);
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mget3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Int(a)) = x {
        let x = stack.pop();
        if let Some(List(b)) = x {
            (stack, closures) = convertOption(b.get(a as usize).cloned(), stack, closures);
        } else {
            panic!("Expected `Some(List(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Int(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bInt2c20List5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn many3a3a5bPair5bA5d5bB5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mtrue(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3aany3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfold_left3a3a5bBool2c20Pair5bA5d5bB5d5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mzip3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mhead_tail3a3a5bA5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mhead_tail3a3a5bB5d(stack, closures);
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                            let v = vec![];
                            stack.push(List(v));
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 2].clone());
                            (stack, closures) = msecond3a3a5bA2c20List5bA5d5d(stack, closures);
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = msecond3a3a5bB2c20List5bB5d5d(stack, closures);
                            (stack, closures) = mzip3a3a5bA2c20B5d(stack, closures);
                            stack.push(locals[locals.len() - 2].clone());
                            (stack, closures) = mfirst3a3a5bA2c20List5bA5d5d(stack, closures);
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = mfirst3a3a5bB2c20List5bB5d5d(stack, closures);
                            (stack, closures) = mpair3a3a5bA2c20B5d(stack, closures);
                            (stack, closures) = mprefix3a3a5bPair5bA5d5bB5d5d(stack, closures);
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap_optional3a3a5bPair5bList5bT5d5d5bT5d2c20T5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = msome3a3a5bT5d(stack, closures);
                locals.pop();
            }
            _ => {
                (stack, closures) = mnone3a3a5bT5d(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mtail_head3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3atail_head3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(Name(m_3a3atail_head3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mbi3a3a5bList5bT5d2c20Optional5bList5bT5d5d2c20Optional5bT5d5d(stack, closures);
    stack.push(Name(m_3a3atail_head3a3alambda2));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mlift_optional_23a3a5bList5bT5d2c20T2c20Pair5bList5bT5d5d5bT5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mcartesian_with3a3a5bA2c20B2c20Pair5bA5d5bB5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(Name(m_3a3acartesian_with3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mmap_concat3a3a5bA2c20Pair5bA5d5bB5d5d(stack, closures);
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mfold_left3a3a5bList5bT5d2c20List5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mtail_head3a3a5bList5bT5d5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bList5bList5bT5d5d2c20List5bT5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mfold_left3a3a5bList5bT5d2c20List5bT5d5d(stack, closures);
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mfilter_in3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mtail_head3a3a5bT5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bList5bT5d2c20T5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mfilter_in3a3a5bT5d(stack, closures);
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            stack.push(locals[locals.len() - 2].clone());
                            (stack, closures) = mprefix3a3a5bT5d(stack, closures);
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bChar2c20Char5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3amap_index3a3ahelper3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail3a3a5bB5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bB2c20List5bB5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                stack.push(locals[locals.len() - 4].clone());
                stack.push(Int(1));
                (stack, closures) = m2b3a3a5bInt5d(stack, closures);
                (stack, closures) = m_3a3amap_index3a3ahelper3a3a5bB2c20A5d(stack, closures);
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 4].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = mprefix3a3a5bA5d(stack, closures);
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mglue3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mprepend3a3a5bT5d(stack, closures);
    (stack, closures) = mappend3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mnone3a3a5bB5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn msome3a3a5bB5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(1, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mdrop3a3a5bBool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let _ = stack.pop().unwrap();
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bBool2c20Bool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mappend3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        let x = stack.pop();
        if let Some(List(b)) = x {
            let mut new_vec = b.clone();
            new_vec.extend(a.into_iter());
            stack.push(List(new_vec));
        } else {
            panic!("Expected `Some(List(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mdrop3a3a5bBool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let _ = stack.pop().unwrap();
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bBool2c20Bool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn meach3a3a5bPair5bA5d5bB5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail3a3a5bPair5bA5d5bB5d5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) =
                    munpair3a3a5bPair5bA5d5bB5d2c20List5bPair5bA5d5bB5d5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = meach3a3a5bPair5bA5d5bB5d5d(stack, closures);
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mzip3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mhead_tail3a3a5bA5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mhead_tail3a3a5bB5d(stack, closures);
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                            let v = vec![];
                            stack.push(List(v));
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 2].clone());
                            (stack, closures) = msecond3a3a5bA2c20List5bA5d5d(stack, closures);
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = msecond3a3a5bB2c20List5bB5d5d(stack, closures);
                            (stack, closures) = mzip3a3a5bA2c20B5d(stack, closures);
                            stack.push(locals[locals.len() - 2].clone());
                            (stack, closures) = mfirst3a3a5bA2c20List5bA5d5d(stack, closures);
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = mfirst3a3a5bB2c20List5bB5d5d(stack, closures);
                            (stack, closures) = mpair3a3a5bA2c20B5d(stack, closures);
                            (stack, closures) = mprefix3a3a5bPair5bA5d5bB5d5d(stack, closures);
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn minit3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        (stack, closures) = convertOption(
            a.split_last().map(|(_, i)| List(i.to_vec())),
            stack,
            closures,
        );
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mnone3a3a5bA5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn msome3a3a5bA5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(1, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mbi3a3a5bList5bT5d2c20List5bT5d2c20List5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 3].clone());
    (stack, closures) = mcall(stack, closures);
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mfold_left3a3a5bList5bT5d2c20List5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mtail_head3a3a5bList5bT5d5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bList5bList5bT5d5d2c20List5bT5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mfold_left3a3a5bList5bT5d2c20List5bT5d5d(stack, closures);
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mprepend3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap3a3a5bList5bT5d2c20List5bT5d5d(stack, closures);
    (stack, closures) = mappend3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mreplicate3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(Int(0));
    (stack, closures) = m3c3d3a3a5bInt5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(Int(1));
                (stack, closures) = m2d3a3a5bInt5d(stack, closures);
                (stack, closures) = mreplicate3a3a5bT5d(stack, closures);
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = msuffix3a3a5bT5d(stack, closures);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mlength3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Int(0));
    stack.push(Name(m_3a3alength3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfold_left3a3a5bInt2c20T5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mlast3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        (stack, closures) = convertOption(a.last().cloned(), stack, closures);
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mhead3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        (stack, closures) = convertOption(a.first().cloned(), stack, closures);
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mtail3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        (stack, closures) = convertOption(
            a.split_first().map(|(_, t)| List(t.to_vec())),
            stack,
            closures,
        );
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mprefix3a3a5bPair5bA5d5bB5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = monce3a3a5bPair5bA5d5bB5d5d(stack, closures);
    (stack, closures) = mprepend3a3a5bPair5bA5d5bB5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mpair3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mfirst3a3a5bB2c20List5bB5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair3a3a5bB2c20List5bB5d5d(stack, closures);
    (stack, closures) = mdrop3a3a5bList5bB5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mfirst3a3a5bA2c20List5bA5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair3a3a5bA2c20List5bA5d5d(stack, closures);
    (stack, closures) = mdrop3a3a5bList5bA5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mzip3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mhead_tail3a3a5bA5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mhead_tail3a3a5bB5d(stack, closures);
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                            let v = vec![];
                            stack.push(List(v));
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 2].clone());
                            (stack, closures) = msecond3a3a5bA2c20List5bA5d5d(stack, closures);
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = msecond3a3a5bB2c20List5bB5d5d(stack, closures);
                            (stack, closures) = mzip3a3a5bA2c20B5d(stack, closures);
                            stack.push(locals[locals.len() - 2].clone());
                            (stack, closures) = mfirst3a3a5bA2c20List5bA5d5d(stack, closures);
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = mfirst3a3a5bB2c20List5bB5d5d(stack, closures);
                            (stack, closures) = mpair3a3a5bA2c20B5d(stack, closures);
                            (stack, closures) = mprefix3a3a5bPair5bA5d5bB5d5d(stack, closures);
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn msecond3a3a5bB2c20List5bB5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair3a3a5bB2c20List5bB5d5d(stack, closures);
    (stack, closures) = mswap3a3a5bB2c20List5bB5d5d(stack, closures);
    (stack, closures) = mdrop3a3a5bB5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn msecond3a3a5bA2c20List5bA5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair3a3a5bA2c20List5bA5d5d(stack, closures);
    (stack, closures) = mswap3a3a5bA2c20List5bA5d5d(stack, closures);
    (stack, closures) = mdrop3a3a5bA5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mhead_tail3a3a5bB5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3ahead_tail3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(Name(m_3a3ahead_tail3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mbi3a3a5bList5bB5d2c20Optional5bB5d2c20Optional5bList5bB5d5d5d(stack, closures);
    stack.push(Name(m_3a3ahead_tail3a3alambda2));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mlift_optional_23a3a5bB2c20List5bB5d2c20Pair5bB5d5bList5bB5d5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mhead_tail3a3a5bA5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3ahead_tail3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(Name(m_3a3ahead_tail3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mbi3a3a5bList5bA5d2c20Optional5bA5d2c20Optional5bList5bA5d5d5d(stack, closures);
    stack.push(Name(m_3a3ahead_tail3a3alambda2));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mlift_optional_23a3a5bA2c20List5bA5d2c20Pair5bA5d5bList5bA5d5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mpair3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mdrop3a3a5bA5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let _ = stack.pop().unwrap();
    (stack, closures)
}
#[must_use]
#[inline]
fn mdrop3a3a5bB5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let _ = stack.pop().unwrap();
    (stack, closures)
}
#[must_use]
#[inline]
fn mfold_left3a3a5bBool2c20Bool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mtail_head3a3a5bBool5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bList5bBool5d2c20Bool5d(stack, closures);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mfold_left3a3a5bBool2c20Bool5d(stack, closures);
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mdrop3a3a5bBool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let _ = stack.pop().unwrap();
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bBool2c20Bool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mappend3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        let x = stack.pop();
        if let Some(List(b)) = x {
            let mut new_vec = b.clone();
            new_vec.extend(a.into_iter());
            stack.push(List(new_vec));
        } else {
            panic!("Expected `Some(List(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mdrop3a3a5b28R2e2e2e202d3eR2e2e2e2c20Bool202bP295d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let _ = stack.pop().unwrap();
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bBool2c2028R2e2e2e202d3eR2e2e2e2c20Bool202bP295d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mright3a3a5bA2c20C5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(1, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mleft3a3a5bA2c20C5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bBool2c20Bool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mfold_left3a3a5bT2c20T5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mtail_head3a3a5bT5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bList5bT5d2c20T5d(stack, closures);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mfold_left3a3a5bT2c20T5d(stack, closures);
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bList5bT5d2c20T5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bDouble2c20Double5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mreduce_left3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mtail_head3a3a5bT5d(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3areduce_left3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mmap_optional3a3a5bPair5bList5bT5d5d5bT5d2c20T5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap_optionally3a3a5bEither5bA5d5bB5d2c20A5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail3a3a5bEither5bA5d5bB5d5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) =
                    munpair3a3a5bEither5bA5d5bB5d2c20List5bEither5bA5d5bB5d5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                            stack.push(locals[locals.len() - 2].clone());
                            stack.push(locals[locals.len() - 3].clone());
                            (stack, closures) =
                                mmap_optionally3a3a5bEither5bA5d5bB5d2c20A5d(stack, closures);
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 3].clone());
                            stack.push(locals[locals.len() - 4].clone());
                            (stack, closures) =
                                mmap_optionally3a3a5bEither5bA5d5bB5d2c20A5d(stack, closures);
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = mprefix3a3a5bA5d(stack, closures);
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mprefix3a3a5bB5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = monce3a3a5bB5d(stack, closures);
    (stack, closures) = mprepend3a3a5bB5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap_optionally3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail3a3a5bA5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bA2c20List5bA5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                            stack.push(locals[locals.len() - 2].clone());
                            stack.push(locals[locals.len() - 3].clone());
                            (stack, closures) = mmap_optionally3a3a5bA2c20B5d(stack, closures);
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 3].clone());
                            stack.push(locals[locals.len() - 4].clone());
                            (stack, closures) = mmap_optionally3a3a5bA2c20B5d(stack, closures);
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = mprefix3a3a5bB5d(stack, closures);
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bA2c20List5bA5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mhead_tail3a3a5bA5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3ahead_tail3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(Name(m_3a3ahead_tail3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mbi3a3a5bList5bA5d2c20Optional5bA5d2c20Optional5bList5bA5d5d5d(stack, closures);
    stack.push(Name(m_3a3ahead_tail3a3alambda2));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mlift_optional_23a3a5bA2c20List5bA5d2c20Pair5bA5d5bList5bA5d5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mpair3a3a5bT2c20List5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mset3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3aset3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mmap_index3a3a5bT2c20T5d(stack, closures);
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mget3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(Int(a)) = x {
        let x = stack.pop();
        if let Some(List(b)) = x {
            (stack, closures) = convertOption(b.get(a as usize).cloned(), stack, closures);
        } else {
            panic!("Expected `Some(List(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(Int(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mprefix3a3a5bB5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = monce3a3a5bB5d(stack, closures);
    (stack, closures) = mprepend3a3a5bB5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mprefix3a3a5bA5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = monce3a3a5bA5d(stack, closures);
    (stack, closures) = mprepend3a3a5bA5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn munzip3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mhead_tail3a3a5bPair5bA5d5bB5d5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) =
                    munpair3a3a5bPair5bA5d5bB5d2c20List5bPair5bA5d5bB5d5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = munpair3a3a5bA2c20B5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = munzip3a3a5bA2c20B5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mprefix3a3a5bA5d(stack, closures);
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mprefix3a3a5bB5d(stack, closures);
                locals.pop();
                locals.pop();
                locals.pop();
                locals.pop();
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bPair5bA5d5bB5d2c20List5bPair5bA5d5bB5d5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mhead_tail3a3a5bPair5bA5d5bB5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3ahead_tail3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(Name(m_3a3ahead_tail3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mbi3a3a5bList5bPair5bA5d5bB5d5d2c20Optional5bPair5bA5d5bB5d5d2c20Optional5bList5bPair5bA5d5bB5d5d5d5d(stack, closures);
    stack.push(Name(m_3a3ahead_tail3a3alambda2));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mlift_optional_23a3a5b20Pair5bA5d5bB5da2c20List5bPair5bA5d5bB5d5da2c20Pair5bPair5bA5d5bB5d5d5bList5bPair5bA5d5bB5d5d5d205d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mpair3a3a5bT2c20List5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mpair3a3a5bList5bT5d2c20T5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn monce3a3a5bChar5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    let v = vec![stack.pop().unwrap()];
    stack.push(List(v));
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mpair3a3a5bList5bT5d2c20T5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bB2c20A5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bString2c20String5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mrights3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3arights3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mmap_optionally3a3a5bEither5bA5d5bB5d2c20B5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mlefts3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3alefts3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mmap_optionally3a3a5bEither5bA5d5bB5d2c20A5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mdrop3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let _ = stack.pop().unwrap();
    (stack, closures)
}
#[must_use]
#[inline]
fn many3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mtrue(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3aany3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfold_left3a3a5bBool2c20T5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bT2c20List5bT5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bInt2c20Int5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mfold_right3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = minit_last3a3a5bA5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bList5bA5d2c20A5d(stack, closures);
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mfold_right3a3a5bA2c20B5d(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bList5bA5d2c20A5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn minit_last3a3a5bA5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3ainit_last3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(Name(m_3a3ainit_last3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mbi3a3a5bList5bA5d2c20Optional5bList5bA5d5d2c20Optional5bA5d5d(stack, closures);
    stack.push(Name(m_3a3ainit_last3a3alambda2));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mlift_optional_23a3a5bList5bA5d2c20A2c20Pair5bList5bA5d5d5bA5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mfold_left3a3a5bBool2c20T5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mtail_head3a3a5bT5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bList5bT5d2c20T5d(stack, closures);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mfold_left3a3a5bBool2c20T5d(stack, closures);
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mfilter_out3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3afilter_out3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfilter_in3a3a5bT5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mappend3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        let x = stack.pop();
        if let Some(List(b)) = x {
            let mut new_vec = b.clone();
            new_vec.extend(a.into_iter());
            stack.push(List(new_vec));
        } else {
            panic!("Expected `Some(List(b))`, but found `{:?}`", x);
        }
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn monce3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    let v = vec![stack.pop().unwrap()];
    stack.push(List(v));
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mget_left3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                (stack, closures) = msome3a3a5bA5d(stack, closures);
            }
            _ => {
                (stack, closures) = mnone3a3a5bA5d(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mdrop3a3a5bA5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let _ = stack.pop().unwrap();
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap_optional3a3a5bPair5bList5bT5d5d5bT5d2c20T5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = msome3a3a5bT5d(stack, closures);
                locals.pop();
            }
            _ => {
                (stack, closures) = mnone3a3a5bT5d(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn minit_last3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3ainit_last3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(Name(m_3a3ainit_last3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mbi3a3a5bList5bT5d2c20Optional5bList5bT5d5d2c20Optional5bT5d5d(stack, closures);
    stack.push(Name(m_3a3ainit_last3a3alambda2));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mlift_optional_23a3a5bList5bT5d2c20T2c20Pair5bList5bT5d5d5bT5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mmax3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mreduce_left3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mtail_head3a3a5bT5d(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3areduce_left3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mmap_optional3a3a5bPair5bList5bT5d5d5bT5d2c20T5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap3a3a5bA2c20T5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail3a3a5bT5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bT2c20List5bT5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mmap3a3a5bT2c20A5d(stack, closures);
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = mprefix3a3a5bA5d(stack, closures);
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mhead3a3a5bChar5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        (stack, closures) = convertOption(a.first().cloned(), stack, closures);
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mnone3a3a5bChar5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
#[must_use]
#[inline]
fn mlength3a3a5bChar5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Int(0));
    stack.push(Name(m_3a3alength3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfold_left3a3a5bInt2c20Char5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mfold_left3a3a5bInt2c20T5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mtail_head3a3a5bT5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bList5bT5d2c20T5d(stack, closures);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mfold_left3a3a5bInt2c20T5d(stack, closures);
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5b28R2e2e2e2c20A2c20B202d3eS2e2e2e202bP292c2028R2e2e2e2c20B2c20A202d3eR2e2e2e2c20A2c20B202bP295d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mreduce_right3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = minit_last3a3a5bT5d(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3areduce_right3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mmap_optional3a3a5bPair5bList5bT5d5d5bT5d2c20T5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap3a3a5bA2c20T5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail3a3a5bT5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bT2c20List5bT5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mmap3a3a5bT2c20A5d(stack, closures);
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = mprefix3a3a5bA5d(stack, closures);
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mprefix3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = monce3a3a5bT5d(stack, closures);
    (stack, closures) = mprepend3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap3a3a5bA2c20T5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail3a3a5bT5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bT2c20List5bT5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mmap3a3a5bT2c20A5d(stack, closures);
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = mprefix3a3a5bA5d(stack, closures);
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bA2c20List5bA5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mhead_tail3a3a5bA5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3ahead_tail3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(Name(m_3a3ahead_tail3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mbi3a3a5bList5bA5d2c20Optional5bA5d2c20Optional5bList5bA5d5d5d(stack, closures);
    stack.push(Name(m_3a3ahead_tail3a3alambda2));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mlift_optional_23a3a5bA2c20List5bA5d2c20Pair5bA5d5bList5bA5d5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mfilter_out3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3afilter_out3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfilter_in3a3a5bT5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bInt2c20List5bInt5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mhead_tail3a3a5bInt5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3ahead_tail3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(Name(m_3a3ahead_tail3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mbi3a3a5bList5bInt5d2c20Optional5bInt5d2c20Optional5bList5bInt5d5d5d(stack, closures);
    stack.push(Name(m_3a3ahead_tail3a3alambda2));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mlift_optional_23a3a5bInt2c20List5bInt5d2c20Pair5bInt5d5bList5bInt5d5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mlength3a3a5bInt5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Int(0));
    stack.push(Name(m_3a3alength3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mfold_left3a3a5bInt2c20Int5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mhead3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(List(a)) = x {
        (stack, closures) = convertOption(a.first().cloned(), stack, closures);
    } else {
        panic!("Expected `Some(List(a))`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mfilter_in3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mtail_head3a3a5bT5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bList5bT5d2c20T5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mfilter_in3a3a5bT5d(stack, closures);
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                let x = stack.pop();
                if let Some(a) = x {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            stack.push(locals[locals.len() - 2].clone());
                            (stack, closures) = mprefix3a3a5bT5d(stack, closures);
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    panic!("Expected `Some(a)`, but found `{:?}`", x);
                }
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mprefix3a3a5bT5d(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = monce3a3a5bT5d(stack, closures);
    (stack, closures) = mprepend3a3a5bT5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn m_3a3amap_index3a3ahelper3a3a5bA2c20T5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail3a3a5bT5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bT2c20List5bT5d5d(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                stack.push(locals[locals.len() - 4].clone());
                stack.push(Int(1));
                (stack, closures) = m2b3a3a5bInt5d(stack, closures);
                (stack, closures) = m_3a3amap_index3a3ahelper3a3a5bT2c20A5d(stack, closures);
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 4].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = mprefix3a3a5bA5d(stack, closures);
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bA2c20List5bA5d5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mhead_tail3a3a5bA5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3ahead_tail3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(Name(m_3a3ahead_tail3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mbi3a3a5bList5bA5d2c20Optional5bA5d2c20Optional5bList5bA5d5d5d(stack, closures);
    stack.push(Name(m_3a3ahead_tail3a3alambda2));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mlift_optional_23a3a5bA2c20List5bA5d2c20Pair5bA5d5bList5bA5d5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mreduce_left3a3a5bT5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mtail_head3a3a5bT5d(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3a3areduce_left3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) = mmap_optional3a3a5bPair5bList5bT5d5d5bT5d2c20T5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mmap23a3a5bA2c20B2c20T5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mzip3a3a5bA2c20B5d(stack, closures);
    stack.push(Name(m_3a3amap23a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mcompose(stack, closures);
    (stack, closures) = mmap3a3a5bPair5bA5d5bB5d2c20T5d(stack, closures);
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mswap3a3a5bBool2c20Bool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mfold_left3a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mtail_head3a3a5bB5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bList5bB5d2c20B5d(stack, closures);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mfold_left3a3a5bA2c20B5d(stack, closures);
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn munpair3a3a5bList5bB5d2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    (stack, closures)
}
#[must_use]
#[inline]
fn mtail_head3a3a5bB5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3a3atail_head3a3alambda0));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    stack.push(Name(m_3a3atail_head3a3alambda1));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mbi3a3a5bList5bB5d2c20Optional5bList5bB5d5d2c20Optional5bB5d5d(stack, closures);
    stack.push(Name(m_3a3atail_head3a3alambda2));
    let x = stack.pop();
    if let Some(Name(n)) = x {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        panic!("Expected `Some(Name(n))`, but found `{:?}`", x);
    }
    (stack, closures) =
        mlift_optional_23a3a5bList5bB5d2c20B2c20Pair5bList5bB5d5d5bB5d5d(stack, closures);
    (stack, closures)
}
#[must_use]
#[inline]
fn mfold_left3a3a5bBool2c20Bool5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mtail_head3a3a5bBool5d(stack, closures);
    let x = stack.pop();
    if let Some(a) = x {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair3a3a5bList5bBool5d2c20Bool5d(stack, closures);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mfold_left3a3a5bBool2c20Bool5d(stack, closures);
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        panic!("Expected `Some(a)`, but found `{:?}`", x);
    }
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
#[must_use]
#[inline]
fn mdrop23a3a5bA2c20B5d(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mdrop3a3a5bB5d(stack, closures);
    (stack, closures) = mdrop3a3a5bA5d(stack, closures);
    (stack, closures)
}
