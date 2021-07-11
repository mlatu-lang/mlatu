% Copyright (C) 2021 (Caden Haustein) <mlatu@brightlysalty.33mail.com>
% This file is part of the Mlatu programming language.
%
% The Mlatu programming language is non-violent software: you can use, 
% redistribute, and/or modify it under the terms of the CNPLv6+ as found
% in the LICENSE file in the source code root directory or
% at <https://git.pixie.town/thufie/CNPL>.
%
% The Mlatu programming language comes with ABSOLUTELY NO WARRANTY, to the 
% extent permitted by applicable law.  See the CNPL for details.

:- module infer.

:- interface.

:- import_module map.

:- import_module ast.
:- import_module context.

:- type ins ---> ins(names :: map(m_name, m_spec)).

:- type inferred ---> ok(spec_term) ; err(infer_err).

:- type infer_err ---> resolve(m_name, context) ; unify(m_spec, m_spec).

:- pred infer_main(term::in, inferred::out) is det.

:- implementation.

:- import_module list.
:- import_module pair.

:- pred infer_int(context::in, int::in, ins::in, ins::out, inferred::out) is det.
infer_int(Context, Num, X, X, ok(mt_int(Context, m_spec([], [mv_int]), Num))).

:- func drop_diff(list(m_value), list(m_value)) = list(m_value). 
drop_diff(A, B) = Result :- ((
    A = [], 
    Result = []
  ) ; (
    A = [mv_int|ATail], 
    ((
      B = [mv_int|BTail], 
      Result = drop_diff(ATail, BTail)
    ) ; (
      B = [], 
      Result = [mv_int|ATail]
    ))
  )).

:- pred composed_spec(m_spec, m_spec, m_spec).
:- mode composed_spec(in, in, out) is det.
composed_spec(m_spec(F, M1), m_spec(M2, T), m_spec(F ++ drop_diff(M2, M1), T ++ drop_diff(M1, M2))).

% 0 1 0 1 -> 0 2 (F, T + M1 - M2)
% 1 0 1 0 -> 2 0 (F + M2 - M1, T)

:- pred infer_compose(term::in, term::in, ins::in, ins::out, inferred::out) is det.
infer_compose(Term1, Term2, In, Out, Result) :- 
  infer_term(Term1, In, Mid, Inferred1), 
  ((
    Inferred1 = ok(STerm1),
    infer_term(Term2, Mid, Out, Inferred2),
    ((
      Inferred2 = ok(STerm2),
      composed_spec(term_spec(STerm1), term_spec(STerm2), Composed),
      Result = ok(mt_compose(Composed, STerm1, STerm2))
    ) ; (
      Inferred2 = err(Err),
      Result = err(Err)
    ))
  ) ; ( 
    Inferred1 = err(Err), 
    Out = Mid,
    Result = err(Err)
  )).

:- func map_from_list(list(pair(K, V))) = map.map(K, V).
map_from_list(List) = map.optimize(
  map.det_insert_from_assoc_list(map.init, List)
).

:- func initial_map = map.map(m_name, m_spec). 
initial_map = map_from_list([
  pair(".", m_spec([mv_int], [])),
  pair("drop", m_spec([mv_int], [])),
  pair("dup", m_spec([mv_int], [mv_int, mv_int])),
  pair("swap", m_spec([mv_int, mv_int], [mv_int, mv_int])),
  pair("+", m_spec([mv_int, mv_int], [mv_int])),
  pair("-", m_spec([mv_int, mv_int], [mv_int])),
  pair("*", m_spec([mv_int, mv_int], [mv_int])),
  pair("/", m_spec([mv_int, mv_int], [mv_int]))
]).

:- pred infer_call(context::in, m_name::in, ins::in, ins::out, inferred::out) is det.
infer_call(Context, Name, X, X, Result) :- (
  if map.search(X ^ names, Name, Spec)
  then Result = ok(mt_call(Context, Spec, Name))
  else Result = err(resolve(Name, Context))).

:- pred infer_def(context::in, m_name::in, term::in, ins::in, ins::out, inferred::out) is det.
infer_def(Context, Name, Inner, In, Out, Result) :- 
  infer_term(Inner, In, Mid, Inferred), 
  ((
    Inferred = ok(SpecTerm),
    Out = Mid ^ names := map.set(Mid ^ names, Name, term_spec(SpecTerm)),
    Result = ok(mt_def(Context, m_spec([], []), Name, SpecTerm))
  ) ; (
    Inferred = err(Err),
    Out = Mid,
    Result = err(Err)
  )).

:- pred infer_term(term::in, ins::in, ins::out, inferred::out) is det.
infer_term(mt_int(Context, {}, Num), In, Out, Result) :- infer_int(Context, Num, In, Out, Result).
infer_term(mt_compose({}, Term1, Term2), In, Out, Result) :- 
  infer_compose(Term1, Term2, In, Out, Result).
infer_term(mt_call(Context, {}, Name), In, Out, Result) :- infer_call(Context, Name, In, Out, Result).
infer_term(mt_def(Context, {}, Name, Inner), In, Out, Result) :- 
  infer_def(Context, Name, Inner, In, Out, Result).

infer_main(In, Out) :- 
  infer_term(In, ins(initial_map), _, Inferred), 
  ((
    Inferred = ok(SpecTerm),
    Expected = m_spec([], []),
    term_spec(SpecTerm) = Spec,
    (if Spec = Expected
    then Out = ok(SpecTerm)
    else Out = err(unify(Expected, Spec)))
   ) ; (
    Inferred = err(Err),
    Out = err(Err)
  )).