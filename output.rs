#![feature(destructuring_assignment)]
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
fn mnip2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mdrop2(stack, closures);
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    (stack, closures)
}
fn mor(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mtrue(stack, closures);
    stack.push(Name(m_3A3Aor3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mfold_left(stack, closures);
    (stack, closures)
}
fn m_3A3Aassert3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures)
}
fn m3E3A3A5BBool5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m263A3A5BBool5D(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn mexp(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Float(a)) = stack.pop() {
        stack.push(Float(a.exp()));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mid(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures)
}
fn m3D3A3A5BInt5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Int(a)), Some(Int(b))) = (stack.pop(), stack.pop()) {
        if a == b {
            (stack, closures) = mtrue(stack, closures);
        } else {
            (stack, closures) = mfalse(stack, closures);
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mfold_left(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mtail_head(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair(stack, closures);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mfold_left(stack, closures);
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn masin(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Float(a)) = stack.pop() {
        stack.push(Float(a.asin()));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m_3A3Aset3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = m3D3A3A5BInt5D(stack, closures);
    if let Some(a) = stack.pop() {
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
        unreachable!()
    };
    locals.pop();
    locals.pop();
    (stack, closures)
}
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
fn m7E3A3A5BBool5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = mnot3A3A5BBool5D(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mnot3A3A5BInt5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Int(a)) = stack.pop() {
        stack.push(Int(!a));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mmap_reduce_left2(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mmap2(stack, closures);
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mreduce_left(stack, closures);
    locals.pop();
    (stack, closures)
}
fn mfalse(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
fn mempty(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(List(a)) = stack.pop() {
        if a.is_empty() {
            (stack, closures) = mtrue(stack, closures);
        } else {
            (stack, closures) = mfalse(stack, closures);
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m2D3A3A5BInt5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Int(a)), Some(Int(b))) = (stack.pop(), stack.pop()) {
        stack.push(Int(a - b));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m_3A3Amap_index3A3Ahelper(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                stack.push(locals[locals.len() - 4].clone());
                stack.push(Int(1));
                (stack, closures) = m2B3A3A5BInt5D(stack, closures);
                (stack, closures) = m_3A3Amap_index3A3Ahelper(stack, closures);
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 4].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = mprefix(stack, closures);
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn mfind(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mfilter_in(stack, closures);
    (stack, closures) = mhead(stack, closures);
    (stack, closures)
}
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
fn m3D3A3A5BList5BInt5D5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mlength(stack, closures);
    stack.push(locals[locals.len() - 2].clone());
    (stack, closures) = mlength(stack, closures);
    (stack, closures) = m3C3E3A3A5BInt5D(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = mhead_tail(stack, closures);
                if let Some(a) = stack.pop() {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                            (stack, closures) = mtrue(stack, closures);
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            (stack, closures) = munpair(stack, closures);
                            locals.push(stack.pop().unwrap());
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 4].clone());
                            (stack, closures) = mhead_tail(stack, closures);
                            if let Some(a) = stack.pop() {
                                match a {
                                    Algebraic(0, fields) => {
                                        stack.extend(fields);
                                        (stack, closures) = mtrue(stack, closures);
                                    }
                                    Algebraic(1, fields) => {
                                        stack.extend(fields);
                                        (stack, closures) = munpair(stack, closures);
                                        locals.push(stack.pop().unwrap());
                                        locals.push(stack.pop().unwrap());
                                        stack.push(locals[locals.len() - 3].clone());
                                        stack.push(locals.last().unwrap().clone());
                                        (stack, closures) = m3C3E3A3A5BInt5D(stack, closures);
                                        if let Some(a) = stack.pop() {
                                            match a {
                                                Algebraic(0, fields) => {
                                                    stack.extend(fields);
                                                    stack.push(locals[locals.len() - 4].clone());
                                                    stack.push(locals[locals.len() - 2].clone());
                                                    (stack, closures) =
                                                        m3D3A3A5BList5BInt5D5D(stack, closures);
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
                                            unreachable!()
                                        };
                                        locals.pop();
                                        locals.pop();
                                    }
                                    _ => {
                                        (stack, closures) = mabort(stack, closures);
                                    }
                                };
                            } else {
                                unreachable!()
                            };
                            locals.pop();
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    unreachable!()
                };
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
        unreachable!()
    };
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn m_3A3Ainsert_nth3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = m3D3A3A5BInt5D(stack, closures);
    if let Some(a) = stack.pop() {
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
        unreachable!()
    };
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn m_pointer(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
fn m3C3E3A3A5BInt5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3D3A3A5BInt5D(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn m_3A3Apartition3A3Alambda1(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mfilter_out(stack, closures);
    (stack, closures)
}
fn m_3A3Aeach23A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair(stack, closures);
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures)
}
fn m_3A3Acurry23A3Alambda0(
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
fn mwrite_file(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Text(a)), Some(Text(b))) = (stack.pop(), stack.pop()) {
        use std::io::Write;
        let mut file = std::fs::File::create(b).unwrap();
        file.write_all(a.as_bytes()).unwrap();
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mmap(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mmap(stack, closures);
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = mprefix(stack, closures);
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    locals.pop();
    (stack, closures)
}
fn m2A3A3A5BInt5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Int(a)), Some(Int(b))) = (stack.pop(), stack.pop()) {
        stack.push(Int(a * b));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn msqrt(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Float(a)) = stack.pop() {
        stack.push(Float(a.sqrt()));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mmap_reduce_right(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mmap(stack, closures);
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mreduce_right(stack, closures);
    locals.pop();
    (stack, closures)
}
fn mswapped(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3A3Aswapped3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mswap(stack, closures);
    (stack, closures) = mcompose(stack, closures);
    (stack, closures)
}
fn mlength(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Int(0));
    stack.push(Name(m_3A3Alength3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mfold_left(stack, closures);
    (stack, closures)
}
fn m3C3D3A3A5BBool5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3E3A3A5BBool5D(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn mzero3A3A5BInt5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Int(0));
    (stack, closures)
}
fn mread3A3A5BChar5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mchars(stack, closures);
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mlength(stack, closures);
    stack.push(Int(1));
    (stack, closures) = m3D3A3A5BInt5D(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                (stack, closures) = mnone(stack, closures);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = mhead(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    locals.pop();
    (stack, closures)
}
fn mwhile(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mcall(stack, closures);
    if let Some(a) = stack.pop() {
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
        unreachable!()
    };
    locals.pop();
    (stack, closures)
}
fn m3E3D3A3A5BList5BInt5D5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3C3A3A5BList5BInt5D5D(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn mmap_reduce_left(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mmap(stack, closures);
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mreduce_left(stack, closures);
    locals.pop();
    (stack, closures)
}
fn m_3A3Amaximum3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mmax(stack, closures);
    (stack, closures)
}
fn monce(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    let v = vec![stack.pop().unwrap()];
    stack.push(List(v));
    locals.pop();
    (stack, closures)
}
fn m3C3D3A3A5BDouble5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3E3A3A5BDouble5D(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn mcosh(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Float(a)) = stack.pop() {
        stack.push(Float(a.cosh()));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mreduce_right(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = minit_last(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3A3Areduce_right3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mmap_optional(stack, closures);
    locals.pop();
    (stack, closures)
}
fn mnip(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mdrop(stack, closures);
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    (stack, closures)
}
fn meither(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    if let Some(a) = stack.pop() {
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
        unreachable!()
    };
    locals.pop();
    locals.pop();
    (stack, closures)
}
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
fn m_3A3Alefts3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mget_left(stack, closures);
    (stack, closures)
}
fn mfix(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3A3Afix3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mcall(stack, closures);
    locals.pop();
    (stack, closures)
}
fn msuffix(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = monce(stack, closures);
    (stack, closures) = mappend(stack, closures);
    (stack, closures)
}
fn m3C3D3A3A5BString5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3E3A3A5BString5D(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn mappend_file(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Text(a)), Some(Text(b))) = (stack.pop(), stack.pop()) {
        use std::io::Write;
        let mut file = std::fs::File::open(b).unwrap();
        file.write_all(a.as_bytes()).unwrap();
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mis_left(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(a) = stack.pop() {
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
        unreachable!()
    };
    (stack, closures)
}
fn mremove(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3A3Aremove3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mfilter_out(stack, closures);
    locals.pop();
    (stack, closures)
}
fn many(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mtrue(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3A3Aany3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mfold_left(stack, closures);
    locals.pop();
    (stack, closures)
}
fn m3D3A3A5BChar5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Char(a)), Some(Char(b))) = (stack.pop(), stack.pop()) {
        if a == b {
            (stack, closures) = mtrue(stack, closures);
        } else {
            (stack, closures) = mfalse(stack, closures);
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m_3A3Aand3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m263A3A5BBool5D(stack, closures);
    (stack, closures)
}
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
fn m3E3A3A5BDouble5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Float(a)), Some(Float(b))) = (stack.pop(), stack.pop()) {
        if a > b {
            (stack, closures) = mtrue(stack, closures);
        } else {
            (stack, closures) = mfalse(stack, closures);
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mIO(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Closure(n, rs)) = stack.pop() {
        closures.push(rs);
        (stack, closures) = n(stack, closures);
        closures.pop();
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn macosh(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Float(a)) = stack.pop() {
        stack.push(Float(a.acosh()));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mfold_right(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = minit_last(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals[locals.len() - 2].clone());
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair(stack, closures);
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mfold_right(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn m7C3A3A5BInt5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Int(a)), Some(Int(b))) = (stack.pop(), stack.pop()) {
        stack.push(Int(a | b));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mtrue(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![];
    v.reverse();
    stack.push(Algebraic(1, v));
    (stack, closures)
}
fn m3E3A3A5BString5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Text(a)), Some(Text(b))) = (stack.pop(), stack.pop()) {
        if a > b {
            (stack, closures) = mtrue(stack, closures);
        } else {
            (stack, closures) = mfalse(stack, closures);
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m3C3A3A5BInt5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap(stack, closures);
    (stack, closures) = m3E3A3A5BInt5D(stack, closures);
    (stack, closures)
}
fn macos(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Float(a)) = stack.pop() {
        stack.push(Float(a.acos()));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn munpair(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mfrom_right(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    if let Some(a) = stack.pop() {
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
        unreachable!()
    };
    locals.pop();
    (stack, closures)
}
fn m3C3E3A3A5BBool5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3D3A3A5BBool5D(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn m_3A3Aremove3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures)
}
fn m2F3A3A5BDouble5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Float(a)), Some(Float(b))) = (stack.pop(), stack.pop()) {
        stack.push(Float(a / b));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn melem(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap(stack, closures);
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3A3Aelem3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = many(stack, closures);
    locals.pop();
    (stack, closures)
}
fn m_3A3Aany3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures) = m7C3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn mdup(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals.last().unwrap().clone());
    locals.pop();
    (stack, closures)
}
fn mis_some(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = mdrop(stack, closures);
                (stack, closures) = mtrue(stack, closures);
            }
            _ => {
                (stack, closures) = mfalse(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mdrop(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let _ = stack.pop().unwrap();
    (stack, closures)
}
fn mpartition_eithers(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mlefts(stack, closures);
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mrights(stack, closures);
    locals.pop();
    (stack, closures)
}
fn m3C3A3A5BString5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap(stack, closures);
    (stack, closures) = m3E3A3A5BString5D(stack, closures);
    (stack, closures)
}
fn mabs(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    if let Some(a) = stack.pop() {
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
        unreachable!()
    };
    locals.pop();
    (stack, closures)
}
fn m_3A3Acartesian_with3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mswap(stack, closures);
    stack.push(closures.last().unwrap().clone()[1].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures)
}
fn m3E3D3A3A5BChar5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3C3A3A5BChar5D(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn m_3A3Ainit_last3A3Alambda2(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mpair(stack, closures);
    (stack, closures)
}
fn m7E3A3A5BInt5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Int(a)), Some(Int(b))) = (stack.pop(), stack.pop()) {
        stack.push(Int(a ^ b));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mshow3A3A5BChar5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = monce(stack, closures);
    (stack, closures) = mfrom_chars(stack, closures);
    (stack, closures)
}
fn m_3A3Afilter_out3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn m_3A3Atail_head3A3Alambda2(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mpair(stack, closures);
    (stack, closures)
}
fn m_3A3Ahead_tail3A3Alambda2(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mpair(stack, closures);
    (stack, closures)
}
fn munzip(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mhead_tail(stack, closures);
    if let Some(a) = stack.pop() {
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
                (stack, closures) = munpair(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = munpair(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = munzip(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mprefix(stack, closures);
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mprefix(stack, closures);
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
        unreachable!()
    };
    (stack, closures)
}
fn mmodify(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 3].clone());
    (stack, closures) = mget(stack, closures);
    if let Some(a) = stack.pop() {
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
                (stack, closures) = mset(stack, closures);
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn m_3A3Alast_init3A3Alambda2(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mpair(stack, closures);
    (stack, closures)
}
fn m3E3A3A5BInt5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Int(a)), Some(Int(b))) = (stack.pop(), stack.pop()) {
        if a > b {
            (stack, closures) = mtrue(stack, closures);
        } else {
            (stack, closures) = mfalse(stack, closures);
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m_3A3Aall3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures) = m263A3A5BBool5D(stack, closures);
    (stack, closures)
}
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
fn m_3A3Aassert_eq3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    stack.push(closures.last().unwrap().clone()[1].clone());
    (stack, closures)
}
fn mmap_optionally(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                if let Some(a) = stack.pop() {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                            stack.push(locals[locals.len() - 2].clone());
                            stack.push(locals[locals.len() - 3].clone());
                            (stack, closures) = mmap_optionally(stack, closures);
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 3].clone());
                            stack.push(locals[locals.len() - 4].clone());
                            (stack, closures) = mmap_optionally(stack, closures);
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = mprefix(stack, closures);
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    unreachable!()
                };
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    locals.pop();
    (stack, closures)
}
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
fn mfloor(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Float(a)) = stack.pop() {
        stack.push(Float(a.floor()));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mlefts(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3A3Alefts3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mmap_optionally(stack, closures);
    (stack, closures)
}
fn mmaximum(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3A3Amaximum3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mreduce_left(stack, closures);
    (stack, closures)
}
fn m3C3A3A5BDouble5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap(stack, closures);
    (stack, closures) = m3E3A3A5BDouble5D(stack, closures);
    (stack, closures)
}
fn mtail(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(List(a)) = stack.pop() {
        if let Some((_, t)) = a.split_first() {
            stack.push(List(t.to_vec()));
            (stack, closures) = msome(stack, closures);
        } else {
            (stack, closures) = mnone(stack, closures);
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mtanh(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Float(a)) = stack.pop() {
        stack.push(Float(a.tanh()));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mceil(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Float(a)) = stack.pop() {
        stack.push(Float(a.ceil()));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mchars(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Text(a)) = stack.pop() {
        stack.push(List(a.chars().map(Char).collect::<Vec<_>>()));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m_3A3Aany23A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair(stack, closures);
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures)
}
fn m_3A3Areduce_left3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair(stack, closures);
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mfold_left(stack, closures);
    (stack, closures)
}
fn m3C3A3A5BBool5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures) = m263A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn m253A3A5BDouble5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Float(a)), Some(Float(b))) = (stack.pop(), stack.pop()) {
        stack.push(Float(a % b));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mmap_right(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                (stack, closures) = mleft(stack, closures);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = mright(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    locals.pop();
    (stack, closures)
}
fn m2D2D2D3E(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                (stack, closures) = mcall(stack, closures);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = mdrop(stack, closures);
                (stack, closures) = mtrue(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mshow3A3A5BInt5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Int(a)) = stack.pop() {
        stack.push(Text(format!("{:?}", a)));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m_3A3Aconcat3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mappend(stack, closures);
    (stack, closures)
}
fn m7C3A3A5BBool5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = mdrop(stack, closures);
                (stack, closures) = mtrue(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m3E3D3A3A5BInt5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3C3A3A5BInt5D(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn m2B3A3A5BInt5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Int(a)), Some(Int(b))) = (stack.pop(), stack.pop()) {
        stack.push(Int(a + b));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mand(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mtrue(stack, closures);
    stack.push(Name(m_3A3Aand3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mfold_left(stack, closures);
    (stack, closures)
}
fn mdrop2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mdrop(stack, closures);
    (stack, closures) = mdrop(stack, closures);
    (stack, closures)
}
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
fn m_3A3Acartesian3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mpair(stack, closures);
    (stack, closures)
}
fn mzip(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mhead_tail(stack, closures);
    if let Some(a) = stack.pop() {
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
                (stack, closures) = mhead_tail(stack, closures);
                if let Some(a) = stack.pop() {
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
                            (stack, closures) = msecond(stack, closures);
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = msecond(stack, closures);
                            (stack, closures) = mzip(stack, closures);
                            stack.push(locals[locals.len() - 2].clone());
                            (stack, closures) = mfirst(stack, closures);
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = mfirst(stack, closures);
                            (stack, closures) = mpair(stack, closures);
                            (stack, closures) = mprefix(stack, closures);
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    unreachable!()
                };
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn m_3A3Acurry3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    stack.push(closures.last().unwrap().clone()[1].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures)
}
fn mzero3A3A5BDouble5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Float(0.0));
    (stack, closures)
}
fn m_3A3Atail_head3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mtail(stack, closures);
    (stack, closures)
}
fn m_3A3Ahead_tail3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mhead(stack, closures);
    (stack, closures)
}
fn minit(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(List(a)) = stack.pop() {
        if let Some((_, i)) = a.split_last() {
            stack.push(List(i.to_vec()));
            (stack, closures) = msome(stack, closures);
        } else {
            (stack, closures) = mnone(stack, closures);
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn muntil(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    if let Some(a) = stack.pop() {
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
        unreachable!()
    };
    locals.pop();
    (stack, closures)
}
fn m_3A3Alast_init3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mlast(stack, closures);
    (stack, closures)
}
fn mnone(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
fn mpad_head(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mlength(stack, closures);
    stack.push(locals[locals.len() - 2].clone());
    (stack, closures) = m3C3A3A5BInt5D(stack, closures);
    if let Some(a) = stack.pop() {
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
                (stack, closures) = mlength(stack, closures);
                (stack, closures) = m2D3A3A5BInt5D(stack, closures);
                (stack, closures) = mreplicate(stack, closures);
                (stack, closures) = mprepend(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn mjoin(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    let v = vec![];
    stack.push(List(v));
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3A3Ajoin3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mfold_left(stack, closures);
    locals.pop();
    (stack, closures)
}
fn mfunction(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3A3Afunction3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    locals.pop();
    (stack, closures)
}
fn mmin(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    if let Some(a) = stack.pop() {
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
        unreachable!()
    };
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn mcall(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Closure(n, rs)) = stack.pop() {
        closures.push(rs);
        (stack, closures) = n(stack, closures);
        closures.pop();
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn matan(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Float(a)) = stack.pop() {
        stack.push(Float(a.atan()));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m2A3A3A5BDouble5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Float(a)), Some(Float(b))) = (stack.pop(), stack.pop()) {
        stack.push(Float(a * b));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mFail(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Closure(n, rs)) = stack.pop() {
        closures.push(rs);
        (stack, closures) = n(stack, closures);
        closures.pop();
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mpartition(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3A3Apartition3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3A3Apartition3A3Alambda1));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mbi(stack, closures);
    locals.pop();
    (stack, closures)
}
fn m3C3E3A3A5BString5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3D3A3A5BString5D(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn mget_left(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                (stack, closures) = msome(stack, closures);
            }
            _ => {
                (stack, closures) = mnone(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mfail(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Text(a)) = stack.pop() {
        panic!("{}", a);
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn matanh(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures)
}
fn m_3A3Ainit_last3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = minit(stack, closures);
    (stack, closures)
}
fn m3C3E3A3A5BDouble5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3D3A3A5BDouble5D(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn mshow3A3A5BString5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures)
}
fn meach2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mzip(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3A3Aeach23A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = meach(stack, closures);
    locals.pop();
    (stack, closures)
}
fn m2D2D3E3A3A5BBool5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = mdrop(stack, closures);
                (stack, closures) = mtrue(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mshow3A3A5BDouble5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Float(a)) = stack.pop() {
        stack.push(Text(format!("{:?}", a)));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m_3A3Afunction3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures)
}
fn m253A3A5BInt5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Int(a)), Some(Int(b))) = (stack.pop(), stack.pop()) {
        stack.push(Int(a % b));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mappend3(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mappend(stack, closures);
    (stack, closures) = mappend(stack, closures);
    (stack, closures)
}
fn m2B3A3A5BDouble5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Float(a)), Some(Float(b))) = (stack.pop(), stack.pop()) {
        stack.push(Float(a + b));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m263A3A5BBool5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = mdrop(stack, closures);
                (stack, closures) = mfalse(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mfrom_some(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    if let Some(a) = stack.pop() {
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
        unreachable!()
    };
    locals.pop();
    (stack, closures)
}
fn masinh(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Float(a)) = stack.pop() {
        stack.push(Float(a.asinh()));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mcurry2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 3].clone());
    stack.push(Name(m_3A3Acurry23A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn mmap_optional(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = msome(stack, closures);
                locals.pop();
            }
            _ => {
                (stack, closures) = mnone(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn m2B3A3A5BString5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Text(a)), Some(Text(b))) = (stack.pop(), stack.pop()) {
        let mut new_string = b.clone();
        new_string.push_str(&a);
        stack.push(Text(new_string));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m_3A3Ajoin3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mglue(stack, closures);
    (stack, closures)
}
fn mmap_index(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Int(0));
    (stack, closures) = m_3A3Amap_index3A3Ahelper(stack, closures);
    (stack, closures)
}
fn m3C3A3A5BChar5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap(stack, closures);
    (stack, closures) = m3E3A3A5BChar5D(stack, closures);
    (stack, closures)
}
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
fn mis_right(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(a) = stack.pop() {
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
        unreachable!()
    };
    (stack, closures)
}
fn m_3A3Apartition3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mfilter_in(stack, closures);
    (stack, closures)
}
fn mfib(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Int(2));
    (stack, closures) = m3C3A3A5BInt5D(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                stack.push(Int(2));
                (stack, closures) = m2D3A3A5BInt5D(stack, closures);
                (stack, closures) = mfib(stack, closures);
                stack.push(locals.last().unwrap().clone());
                stack.push(Int(1));
                (stack, closures) = m2D3A3A5BInt5D(stack, closures);
                (stack, closures) = mfib(stack, closures);
                (stack, closures) = m2B3A3A5BInt5D(stack, closures);
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
        unreachable!()
    };
    locals.pop();
    (stack, closures)
}
fn m3E3D3A3A5BString5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3C3A3A5BString5D(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn m3E3D3A3A5BDouble5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3C3A3A5BDouble5D(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn mmax(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    if let Some(a) = stack.pop() {
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
        unreachable!()
    };
    locals.pop();
    locals.pop();
    (stack, closures)
}
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
fn m_3A3Aor3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m7C3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn mread3A3A5BInt5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Text(a)) = stack.pop() {
        if let Ok(i) = a.parse() {
            stack.push(Int(i));
            (stack, closures) = msome(stack, closures);
        } else {
            (stack, closures) = mnone(stack, closures);
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn msinh(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Float(a)) = stack.pop() {
        stack.push(Float(a.sinh()));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mlog(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Float(a)) = stack.pop() {
        stack.push(Float(a.log10()));
    } else {
        unreachable!()
    };
    (stack, closures)
}
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
fn msome(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(1, v));
    (stack, closures)
}
fn mconcat(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let v = vec![];
    stack.push(List(v));
    stack.push(Name(m_3A3Aconcat3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mfold_left(stack, closures);
    (stack, closures)
}
fn mcartesian(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3A3Acartesian3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mcartesian_with(stack, closures);
    (stack, closures)
}
fn mreduce_left(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mtail_head(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3A3Areduce_left3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mmap_optional(stack, closures);
    locals.pop();
    (stack, closures)
}
fn m3C3E3A3A5BList5BInt5D5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3D3A3A5BList5BInt5D5D(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn msin(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Float(a)) = stack.pop() {
        stack.push(Float(a.sin()));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn many2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mzip(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3A3Aany23A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = many(stack, closures);
    locals.pop();
    (stack, closures)
}
fn m_3A3Aget_all3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mswap(stack, closures);
    (stack, closures) = mget(stack, closures);
    (stack, closures)
}
fn mlast_init(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3A3Alast_init3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    stack.push(Name(m_3A3Alast_init3A3Alambda1));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mbi(stack, closures);
    stack.push(Name(m_3A3Alast_init3A3Alambda2));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mlift_optional_2(stack, closures);
    (stack, closures)
}
fn mhead_tail(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3A3Ahead_tail3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    stack.push(Name(m_3A3Ahead_tail3A3Alambda1));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mbi(stack, closures);
    stack.push(Name(m_3A3Ahead_tail3A3Alambda2));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mlift_optional_2(stack, closures);
    (stack, closures)
}
fn mmap_concat(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mmap(stack, closures);
    (stack, closures) = mconcat(stack, closures);
    (stack, closures)
}
fn mcos(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Float(a)) = stack.pop() {
        stack.push(Float(a.cos()));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn minit_last(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3A3Ainit_last3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    stack.push(Name(m_3A3Ainit_last3A3Alambda1));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mbi(stack, closures);
    stack.push(Name(m_3A3Ainit_last3A3Alambda2));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mlift_optional_2(stack, closures);
    (stack, closures)
}
fn mis_none(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(a) = stack.pop() {
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
        unreachable!()
    };
    (stack, closures)
}
fn mfrom_optional(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
            }
            _ => {
                stack.push(locals.last().unwrap().clone());
            }
        };
    } else {
        unreachable!()
    };
    locals.pop();
    (stack, closures)
}
fn m2F3A3A5BInt5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Int(a)), Some(Int(b))) = (stack.pop(), stack.pop()) {
        stack.push(Int(a / b));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m26263A3A5BBool5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                (stack, closures) = mdrop(stack, closures);
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
        unreachable!()
    };
    (stack, closures)
}
fn mcurry(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(Name(m_3A3Acurry3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn m7C7C3A3A5BBool5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                (stack, closures) = mcall(stack, closures);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = mdrop(stack, closures);
                (stack, closures) = mtrue(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m_3A3Acompose3A3Alambda0(
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
fn mget_line(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    stack.push(Text(buf));
    (stack, closures)
}
fn mtail_head(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3A3Atail_head3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    stack.push(Name(m_3A3Atail_head3A3Alambda1));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mbi(stack, closures);
    stack.push(Name(m_3A3Atail_head3A3Alambda2));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mlift_optional_2(stack, closures);
    (stack, closures)
}
fn m3C3E3A3A5BChar5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3D3A3A5BChar5D(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn meach(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mhead_tail(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = meach(stack, closures);
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    locals.pop();
    (stack, closures)
}
fn mmap2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mzip(stack, closures);
    stack.push(Name(m_3A3Amap23A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mcompose(stack, closures);
    (stack, closures) = mmap(stack, closures);
    locals.pop();
    (stack, closures)
}
fn mshow3A3A5BOptional5BT5D5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(a) = stack.pop() {
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
                (stack, closures) = m2B3A3A5BString5D(stack, closures);
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mget(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Int(a)), Some(List(b))) = (stack.pop(), stack.pop()) {
        if let Some(gotten) = b.get(a as usize) {
            stack.push(gotten.clone());
            (stack, closures) = msome(stack, closures);
        } else {
            (stack, closures) = mnone(stack, closures);
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m3E3D3A3A5BBool5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3C3A3A5BBool5D(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn mappend(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(List(a)), Some(List(b))) = (stack.pop(), stack.pop()) {
        let mut new_vec = b.clone();
        new_vec.extend(a.into_iter());
        stack.push(List(new_vec));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mshow3A3A5BBool5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(a) = stack.pop() {
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
        unreachable!()
    };
    (stack, closures)
}
fn m_3A3Arights3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mget_right(stack, closures);
    (stack, closures)
}
fn moptional(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    if let Some(a) = stack.pop() {
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
        unreachable!()
    };
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn m_3A3Aremove_nth3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = m3D3A3A5BInt5D(stack, closures);
    if let Some(a) = stack.pop() {
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
        unreachable!()
    };
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn mtrunc(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Float(a)) = stack.pop() {
        stack.push(Float(a.trunc()));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mminimum(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3A3Aminimum3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mreduce_left(stack, closures);
    (stack, closures)
}
fn m_3A3Aall23A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair(stack, closures);
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures)
}
fn mneg3A3A5BInt5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(Int(0));
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = m2D3A3A5BInt5D(stack, closures);
    locals.pop();
    (stack, closures)
}
fn msurround(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mprepend(stack, closures);
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mappend(stack, closures);
    locals.pop();
    (stack, closures)
}
fn munit(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
fn mconcat_optionals(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let v = vec![];
    stack.push(List(v));
    stack.push(Name(m_3A3Aconcat_optionals3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mfold_left(stack, closures);
    (stack, closures)
}
fn mmap_reduce_right2(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mmap2(stack, closures);
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mreduce_right(stack, closures);
    locals.pop();
    (stack, closures)
}
fn m3C3D3A3A5BInt5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3E3A3A5BInt5D(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn mfilter_out(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3A3Afilter_out3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mfilter_in(stack, closures);
    locals.pop();
    (stack, closures)
}
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
fn mask(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mprint(stack, closures);
    use std::io::Write;
    std::io::stdout().flush().unwrap();
    (stack, closures) = mget_line(stack, closures);
    (stack, closures)
}
fn m_3A3Aconcat_optionals3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = msuffix(stack, closures);
            }
            _ => {}
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mall(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mtrue(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3A3Aall3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mfold_left(stack, closures);
    locals.pop();
    (stack, closures)
}
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
fn mprint(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Text(a)) = stack.pop() {
        println!("{}", a);
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m_3A3Amap23A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair(stack, closures);
    (stack, closures)
}
fn mcartesian_with(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(Name(m_3A3Acartesian_with3A3Alambda1));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mmap_concat(stack, closures);
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn mmap_pair(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = munpair(stack, closures);
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 4].clone());
    (stack, closures) = mcall(stack, closures);
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 5].clone());
    (stack, closures) = mcall(stack, closures);
    (stack, closures) = mpair(stack, closures);
    locals.pop();
    locals.pop();
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn m3C3D3A3A5BList5BInt5D5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3E3A3A5BList5BInt5D5D(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn mget_right(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = msome(stack, closures);
            }
            _ => {
                (stack, closures) = mnone(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m_3A3Aminimum3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mmin(stack, closures);
    (stack, closures)
}
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
fn m_3A3Acartesian_with3A3Alambda1(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(closures.last().unwrap().clone()[0].clone());
    stack.push(locals.last().unwrap().clone());
    stack.push(closures.last().unwrap().clone()[1].clone());
    stack.push(Name(m_3A3Acartesian_with3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mmap(stack, closures);
    locals.pop();
    (stack, closures)
}
fn mreplicate(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(Int(0));
    (stack, closures) = m3C3D3A3A5BInt5D(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 2].clone());
                stack.push(Int(1));
                (stack, closures) = m2D3A3A5BInt5D(stack, closures);
                (stack, closures) = mreplicate(stack, closures);
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = msuffix(stack, closures);
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
        unreachable!()
    };
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn massert_eq(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals[locals.len() - 3].clone());
    stack.push(Name(m_3A3Aassert_eq3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = massert(stack, closures);
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn mpi(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Float(3.141592653589793));
    (stack, closures)
}
fn mread3A3A5BBool5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Text("true".to_owned()));
    (stack, closures) = m3D3A3A5BString5D(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                stack.push(Text("false".to_owned()));
                (stack, closures) = m3D3A3A5BString5D(stack, closures);
                if let Some(a) = stack.pop() {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                            (stack, closures) = mnone(stack, closures);
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            (stack, closures) = mfalse(stack, closures);
                            (stack, closures) = msome(stack, closures);
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    unreachable!()
                };
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = mtrue(stack, closures);
                (stack, closures) = msome(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    locals.pop();
    (stack, closures)
}
fn matan2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Float(a)), Some(Float(b))) = (stack.pop(), stack.pop()) {
        stack.push(Float(a.atan2(b)));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mhead(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(List(a)) = stack.pop() {
        if let Some(h) = a.first() {
            stack.push(h.clone());
            (stack, closures) = msome(stack, closures);
        } else {
            (stack, closures) = mnone(stack, closures);
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mneg3A3A5BDouble5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(Float(0.0));
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = m2D3A3A5BDouble5D(stack, closures);
    locals.pop();
    (stack, closures)
}
fn mget_all(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap(stack, closures);
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3A3Aget_all3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mmap(stack, closures);
    locals.pop();
    (stack, closures)
}
fn msecond(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair(stack, closures);
    (stack, closures) = mswap(stack, closures);
    (stack, closures) = mdrop(stack, closures);
    (stack, closures)
}
fn mprefix(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = monce(stack, closures);
    (stack, closures) = mprepend(stack, closures);
    (stack, closures)
}
fn mfilter_in(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mtail_head(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals.last().unwrap().clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mfilter_in(stack, closures);
                stack.push(locals[locals.len() - 2].clone());
                stack.push(locals[locals.len() - 3].clone());
                (stack, closures) = mcall(stack, closures);
                if let Some(a) = stack.pop() {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            stack.push(locals[locals.len() - 2].clone());
                            (stack, closures) = mprefix(stack, closures);
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    unreachable!()
                };
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    locals.pop();
    (stack, closures)
}
fn mExit(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Closure(n, rs)) = stack.pop() {
        closures.push(rs);
        (stack, closures) = n(stack, closures);
        closures.pop();
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mcompose(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(Name(m_3A3Acompose3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn m3E3A3A5BList5BInt5D5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mhead_tail(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                (stack, closures) = mfalse(stack, closures);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair(stack, closures);
                locals.push(stack.pop().unwrap());
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 4].clone());
                (stack, closures) = mhead_tail(stack, closures);
                if let Some(a) = stack.pop() {
                    match a {
                        Algebraic(0, fields) => {
                            stack.extend(fields);
                            (stack, closures) = mtrue(stack, closures);
                        }
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            (stack, closures) = munpair(stack, closures);
                            locals.push(stack.pop().unwrap());
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 3].clone());
                            stack.push(locals.last().unwrap().clone());
                            (stack, closures) = m3E3A3A5BInt5D(stack, closures);
                            if let Some(a) = stack.pop() {
                                match a {
                                    Algebraic(0, fields) => {
                                        stack.extend(fields);
                                        stack.push(locals[locals.len() - 3].clone());
                                        stack.push(locals.last().unwrap().clone());
                                        (stack, closures) = m3C3A3A5BInt5D(stack, closures);
                                        if let Some(a) = stack.pop() {
                                            match a {
                                                Algebraic(0, fields) => {
                                                    stack.extend(fields);
                                                    stack.push(locals[locals.len() - 4].clone());
                                                    stack.push(locals[locals.len() - 2].clone());
                                                    (stack, closures) =
                                                        m3E3A3A5BList5BInt5D5D(stack, closures);
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
                                            unreachable!()
                                        };
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
                                unreachable!()
                            };
                            locals.pop();
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) = mabort(stack, closures);
                        }
                    };
                } else {
                    unreachable!()
                };
                locals.pop();
                locals.pop();
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn mnot3A3A5BBool5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(a) = stack.pop() {
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
        unreachable!()
    };
    (stack, closures)
}
fn m263A3A5BInt5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Int(a)), Some(Int(b))) = (stack.pop(), stack.pop()) {
        stack.push(Int(a & b));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mmap_left(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = mright(stack, closures);
            }
            Algebraic(0, fields) => {
                stack.extend(fields);
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = mcall(stack, closures);
                (stack, closures) = mleft(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    locals.pop();
    (stack, closures)
}
fn mprepend(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap(stack, closures);
    (stack, closures) = mappend(stack, closures);
    (stack, closures)
}
fn mexit(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Int(i)) = stack.pop() {
        std::process::exit(i as i32);
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mreverse(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mhead_tail(stack, closures);
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                let v = vec![];
                stack.push(List(v));
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
                (stack, closures) = munpair(stack, closures);
                (stack, closures) = mreverse(stack, closures);
                (stack, closures) = mswap(stack, closures);
                (stack, closures) = msuffix(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m2D2D3E3A3A5BInt5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap(stack, closures);
    (stack, closures) = mnot3A3A5BInt5D(stack, closures);
    (stack, closures) = m7C3A3A5BInt5D(stack, closures);
    (stack, closures)
}
fn m_3A3Aelem3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures)
}
fn mright(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(1, v));
    (stack, closures)
}
fn m3D3A3A5BBool5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(0, fields) => {
                stack.extend(fields);
                (stack, closures) = mnot3A3A5BBool5D(stack, closures);
            }
            Algebraic(1, fields) => {
                stack.extend(fields);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m_3A3Aswapped3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap(stack, closures);
    (stack, closures)
}
fn m3D3A3A5BString5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Text(a)), Some(Text(b))) = (stack.pop(), stack.pop()) {
        if a == b {
            (stack, closures) = mtrue(stack, closures);
        } else {
            (stack, closures) = mfalse(stack, closures);
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mleft(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
fn m_3A3Alength3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mdrop(stack, closures);
    stack.push(Int(1));
    (stack, closures) = m2B3A3A5BInt5D(stack, closures);
    (stack, closures)
}
fn mprintln(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mprint(stack, closures);
    (stack, closures) = mnewline(stack, closures);
    (stack, closures)
}
fn m2D3A3A5BDouble5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Float(a)), Some(Float(b))) = (stack.pop(), stack.pop()) {
        stack.push(Float(a - b));
    } else {
        unreachable!()
    };
    (stack, closures)
}
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
fn mrights(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Name(m_3A3Arights3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mmap_optionally(stack, closures);
    (stack, closures)
}
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
fn m3E3A3A5BChar5D(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Char(a)), Some(Char(b))) = (stack.pop(), stack.pop()) {
        if a > b {
            (stack, closures) = mtrue(stack, closures);
        } else {
            (stack, closures) = mfalse(stack, closures);
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mtan(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Float(a)) = stack.pop() {
        stack.push(Float(a.tan()));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn m_3A3Ainit_last3A3Alambda1(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mlast(stack, closures);
    (stack, closures)
}
fn mread_file(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Text(a)) = stack.pop() {
        stack.push(Text(std::fs::read_to_string(a).unwrap()));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mflip(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair(stack, closures);
    (stack, closures) = mswap(stack, closures);
    (stack, closures) = mpair(stack, closures);
    (stack, closures)
}
fn mall2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    (stack, closures) = mzip(stack, closures);
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3A3Aall23A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mall(stack, closures);
    locals.pop();
    (stack, closures)
}
fn mlift_optional_2(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    if let Some(a) = stack.pop() {
        match a {
            Algebraic(1, fields) => {
                stack.extend(fields);
                locals.push(stack.pop().unwrap());
                stack.push(locals[locals.len() - 3].clone());
                if let Some(a) = stack.pop() {
                    match a {
                        Algebraic(1, fields) => {
                            stack.extend(fields);
                            locals.push(stack.pop().unwrap());
                            stack.push(locals[locals.len() - 2].clone());
                            stack.push(locals.last().unwrap().clone());
                            stack.push(locals[locals.len() - 5].clone());
                            (stack, closures) = mcall(stack, closures);
                            (stack, closures) = msome(stack, closures);
                            locals.pop();
                        }
                        _ => {
                            (stack, closures) = mnone(stack, closures);
                        }
                    };
                } else {
                    unreachable!()
                };
                locals.pop();
            }
            _ => {
                (stack, closures) = mnone(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn m_3A3Atail_head3A3Alambda1(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mhead(stack, closures);
    (stack, closures)
}
fn m_3A3Ahead_tail3A3Alambda1(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mtail(stack, closures);
    (stack, closures)
}
fn mremove_nth(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3A3Aremove_nth3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mmap_index(stack, closures);
    (stack, closures) = mconcat(stack, closures);
    locals.pop();
    (stack, closures)
}
fn m3D3A3A5BDouble5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let (Some(Float(a)), Some(Float(b))) = (stack.pop(), stack.pop()) {
        if a == b {
            (stack, closures) = mtrue(stack, closures);
        } else {
            (stack, closures) = mfalse(stack, closures);
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mglue(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mprepend(stack, closures);
    (stack, closures) = mappend(stack, closures);
    (stack, closures)
}
fn mshow3A3A5BEither5BA5D5BB5D5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(a) = stack.pop() {
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
        unreachable!()
    };
    (stack, closures)
}
fn m_3A3Alast_init3A3Alambda1(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = minit(stack, closures);
    (stack, closures)
}
fn mnot_elem(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = melem(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn main() -> () {
    let mut stack = Vec::new();
    let mut locals: Vec<Rep> = Vec::new();
    let mut closures = Vec::new();
    stack.push(Int(15));
    (stack, closures) = mfib(stack, closures);
    (stack, closures) = mprint(stack, closures);
}
fn mround(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Float(a)) = stack.pop() {
        stack.push(Float(a.round()));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mfirst(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair(stack, closures);
    (stack, closures) = mdrop(stack, closures);
    (stack, closures)
}
fn m3C3A3A5BList5BInt5D5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mswap(stack, closures);
    (stack, closures) = m3E3A3A5BList5BInt5D5D(stack, closures);
    (stack, closures)
}
fn mdrop3(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = mdrop(stack, closures);
    (stack, closures) = mdrop(stack, closures);
    (stack, closures) = mdrop(stack, closures);
    (stack, closures)
}
fn mfrom_chars(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(List(a)) = stack.pop() {
        stack.push(Text(
            a.iter()
                .filter_map(|e| if let Char(c) = e { Some(c) } else { None })
                .collect::<String>(),
        ));
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn massert(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(Name(m_3A3Aassert3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    if let Some(Closure(n, rs)) = stack.pop() {
        closures.push(rs);
        (stack, closures) = n(stack, closures);
        closures.pop();
    } else {
        unreachable!()
    };
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    if let Some(a) = stack.pop() {
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
        unreachable!()
    };
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn mpair(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    let mut v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
    v.reverse();
    stack.push(Algebraic(0, v));
    (stack, closures)
}
fn mnewline(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Text("\n".to_owned()));
    (stack, closures) = mprint(stack, closures);
    (stack, closures)
}
fn m3C3D3A3A5BChar5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = m3E3A3A5BChar5D(stack, closures);
    (stack, closures) = mnot3A3A5BBool5D(stack, closures);
    (stack, closures)
}
fn m_3A3Afix3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mfix(stack, closures);
    (stack, closures)
}
fn m_3A3Areduce_right3A3Alambda0(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = munpair(stack, closures);
    stack.push(closures.last().unwrap().clone()[0].clone());
    (stack, closures) = mfold_right(stack, closures);
    (stack, closures)
}
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
fn minsert_nth(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3A3Ainsert_nth3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mmap_index(stack, closures);
    (stack, closures) = mconcat(stack, closures);
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn mabort(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    stack.push(Text("abort called".to_owned()));
    (stack, closures) = mfail(stack, closures);
    (stack, closures)
}
fn mread3A3A5BDouble5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(Text(a)) = stack.pop() {
        if let Ok(i) = a.parse() {
            stack.push(Float(i));
            (stack, closures) = msome(stack, closures);
        } else {
            (stack, closures) = mnone(stack, closures);
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mlast(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    if let Some(List(a)) = stack.pop() {
        if let Some(l) = a.last() {
            stack.push(l.clone());
            (stack, closures) = msome(stack, closures);
        } else {
            (stack, closures) = mnone(stack, closures);
        };
    } else {
        unreachable!()
    };
    (stack, closures)
}
fn mset(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals[locals.len() - 2].clone());
    stack.push(locals.last().unwrap().clone());
    stack.push(Name(m_3A3Aset3A3Alambda0));
    if let Some(Name(n)) = stack.pop() {
        let v = vec![stack.pop().unwrap(), stack.pop().unwrap()];
        stack.push(Closure(n, v));
    } else {
        unreachable!()
    };
    (stack, closures) = mmap_index(stack, closures);
    locals.pop();
    locals.pop();
    (stack, closures)
}
fn mread3A3A5BString5D(
    mut stack: Vec<Rep>,
    mut closures: Vec<Vec<Rep>>,
) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    (stack, closures) = msome(stack, closures);
    (stack, closures)
}
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
fn mfrom_left(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    if let Some(a) = stack.pop() {
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
        unreachable!()
    };
    locals.pop();
    (stack, closures)
}
fn mpad_tail(mut stack: Vec<Rep>, mut closures: Vec<Vec<Rep>>) -> (Vec<Rep>, Vec<Vec<Rep>>) {
    let mut locals: Vec<Rep> = Vec::new();
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    locals.push(stack.pop().unwrap());
    stack.push(locals.last().unwrap().clone());
    (stack, closures) = mlength(stack, closures);
    stack.push(locals[locals.len() - 2].clone());
    (stack, closures) = m3C3A3A5BInt5D(stack, closures);
    if let Some(a) = stack.pop() {
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
                (stack, closures) = mlength(stack, closures);
                (stack, closures) = m2D3A3A5BInt5D(stack, closures);
                (stack, closures) = mreplicate(stack, closures);
                stack.push(locals.last().unwrap().clone());
                (stack, closures) = mprepend(stack, closures);
            }
            _ => {
                (stack, closures) = mabort(stack, closures);
            }
        };
    } else {
        unreachable!()
    };
    locals.pop();
    locals.pop();
    locals.pop();
    (stack, closures)
}
