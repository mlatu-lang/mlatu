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

:- type inferred(X) ---> ok(X) ; err(infer_err).

:- type infer_err ---> resolve(m_name, context) ; unify(m_spec, m_spec).

:- pred infer_main(terms, inferred(spec_terms)).
:- mode infer_main(in, out) is det.

:- implementation.

:- import_module list.
:- import_module pair.

:- pred infer_int(context, int, inferred(spec_term)).
:- mode infer_int(in, in, out) is det.
infer_int(Context, Num, ok(mt_int(Context, m_spec([], [mv_int]), Num))).

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

:- pred infer_call(context, m_name, ins, ins, inferred(spec_term)).
:- mode infer_call(in, in, in, out, out) is det.
infer_call(Context, Name, X, X, Result) :- (
  if assoc_list.search(X ^ names, Name, Spec)
  then Result = ok(mt_call(Context, Spec, Name))
  else Result = err(resolve(Name, Context))).

:- pred infer_def(context, m_name, terms, ins, ins, inferred(spec_term)).
:- mode infer_def(in, in, in, in, out, out) is det.
infer_def(Context, Name, Inner, In, Out, Result) :- 
  infer_terms(Inner, In, Mid, Inferred), 
  ((
    Inferred = ok(SpecTerms),
    Out ^ names = [pair(Name, terms_spec(SpecTerms))|Mid ^ names],
    Result = ok(mt_def(Context, m_spec([], []), Name, SpecTerms))
  ) ; (
    Inferred = err(Err),
    Out = Mid,
    Result = err(Err)
  )).

:- pred infer_term(term, ins, ins, inferred(spec_term)).
:- mode infer_term(in, in, out, out) is det.
infer_term(mt_int(Context, {}, Num), In, Out, Result) :- infer_int(Context, Num, Result), In = Out.
infer_term(mt_call(Context, {}, Name), In, Out, Result) :- infer_call(Context, Name, In, Out, Result).
infer_term(mt_def(Context, {}, Name, Inner), In, Out, Result) :- 
  infer_def(Context, Name, Inner, In, Out, Result).

:- pred infer_list(list(term), ins, ins, inferred(list(spec_term))).
:- mode infer_list(in, in, out, out) is det.
infer_list([], X, X, ok([])).
infer_list([Head|Tail], In, Out, Result) :- 
  infer_term(Head, In, Mid, InferredHead),
  ((
    InferredHead = ok(SpecTerm),
    infer_list(Tail, Mid, Out, InferredTail),
    ((
      InferredTail = ok(SpecTerms),
      Result = ok([SpecTerm|SpecTerms])
    ) ; (
      InferredTail = err(Err),
      Result = err(Err)
    ))
  ) ; (
    InferredHead = err(Err),
    In = Out,
    Result = err(Err)
  )).

:- pred infer_terms(terms, ins, ins, inferred(spec_terms)).
:- mode infer_terms(in, in, out, out) is det.
infer_terms(mts(Context, {}, List), In, Out, Result) :- 
  infer_list(List, In, Out, Inferred), 
  ((
    Inferred = ok(SpecTermList),
    foldl(
      (pred(SpecTerm::in, Acc::in, NewAcc::out) is det :- composed_spec(Acc, term_spec(SpecTerm), NewAcc))
    , SpecTermList, m_spec([], []), Spec), 
    Result = ok(mts(Context, Spec, SpecTermList))
  ) ; (
    Inferred = err(Err),
    Result = err(Err)
  )).

infer_main(In, Out) :- 
  initial_map(Map),
  infer_terms(In, ins(Map), _, Inferred), 
  ((
    Inferred = ok(SpecTerms),
    Expected = m_spec([], []),
    terms_spec(SpecTerms) = Spec,
    (if Spec = Expected
    then Out = ok(SpecTerms)
    else Out = err(unify(Expected, Spec)))
   ) ; (
    Inferred = err(Err),
    Out = err(Err)
  )).
