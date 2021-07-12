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

:- import_module assoc_list.

:- import_module ast.
:- import_module context.

:- type ins ---> ins(names :: assoc_list(m_name, m_spec)).

:- type inferred ---> ok(spec_term) ; err(infer_err).

:- type infer_err ---> resolve(m_name, context) ; unify(m_spec, m_spec).

:- pred infer_main(term, inferred).
:- mode infer_main(in, out) is det.

:- implementation.

:- import_module list.
:- import_module pair.

:- pred infer_int(context, int, ins, ins, inferred).
:- mode infer_int(in, in, in, out, out) is det.
infer_int(Context, Num, X, X, ok(mt_int(Context, m_spec([], [mv_int]), Num))).

:- pred drop_diff(list(m_value), list(m_value), list(m_value)).
:- mode drop_diff(in, in, out) is det. 
drop_diff([], _, []).
drop_diff([mv_int|A], [mv_int|B], Result) :- drop_diff(A, B, Result).
drop_diff([mv_int|Tail], [], [mv_int|Tail]).

:- pred composed_spec(m_spec, m_spec, m_spec).
:- mode composed_spec(in, in, out) is det.
composed_spec(m_spec(F, M1), m_spec(M2, T), m_spec(F ++ FromAdd, T ++ ToAdd)) :- 
  drop_diff(M2, M1, FromAdd),
  drop_diff(M1, M2, ToAdd).

% 0 1 0 1 -> 0 2 (F, T + M1 - M2)
% 1 0 1 0 -> 2 0 (F + M2 - M1, T)

:- pred infer_compose(term, term, ins, ins, inferred).
:- mode infer_compose(in, in, in, out, out) is det.
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

:- pred initial_map(assoc_list(m_name, m_spec)). 
:- mode initial_map(out) is det.
initial_map([
  pair(".", m_spec([mv_int], [])),
  pair("drop", m_spec([mv_int], [])),
  pair("dup", m_spec([mv_int], [mv_int, mv_int])),
  pair("swap", m_spec([mv_int, mv_int], [mv_int, mv_int])),
  pair("+", m_spec([mv_int, mv_int], [mv_int])),
  pair("-", m_spec([mv_int, mv_int], [mv_int])),
  pair("*", m_spec([mv_int, mv_int], [mv_int])),
  pair("/", m_spec([mv_int, mv_int], [mv_int]))
]).

:- pred infer_call(context, m_name, ins, ins, inferred).
:- mode infer_call(in, in, in, out, out) is det.
infer_call(Context, Name, X, X, Result) :- (
  if assoc_list.search(X ^ names, Name, Spec)
  then Result = ok(mt_call(Context, Spec, Name))
  else Result = err(resolve(Name, Context))).

:- pred infer_def(context, m_name, term, ins, ins, inferred).
:- mode infer_def(in, in, in, in, out, out) is det.
infer_def(Context, Name, Inner, In, Out, Result) :- 
  infer_term(Inner, In, Mid, Inferred), 
  ((
    Inferred = ok(SpecTerm),
    Out ^ names = [pair(Name, term_spec(SpecTerm))|Mid ^ names],
    Result = ok(mt_def(Context, m_spec([], []), Name, SpecTerm))
  ) ; (
    Inferred = err(Err),
    Out = Mid,
    Result = err(Err)
  )).

:- pred infer_term(term, ins, ins, inferred).
:- mode infer_term(in, in, out, out) is det.
infer_term(mt_int(Context, {}, Num), In, Out, Result) :- infer_int(Context, Num, In, Out, Result).
infer_term(mt_compose({}, Term1, Term2), In, Out, Result) :- 
  infer_compose(Term1, Term2, In, Out, Result).
infer_term(mt_call(Context, {}, Name), In, Out, Result) :- infer_call(Context, Name, In, Out, Result).
infer_term(mt_def(Context, {}, Name, Inner), In, Out, Result) :- 
  infer_def(Context, Name, Inner, In, Out, Result).

infer_main(In, Out) :- 
  initial_map(Map),
  infer_term(In, ins(Map), _, Inferred), 
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