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

:- import_module ast.
:- import_module context.

:- type inferred ---> ok(spec_term) ; err(infer_err).

:- type infer_err ---> resolve(m_name, context) ; unify(m_spec, m_spec).

:- pred infer_main(term::in, inferred::out) is det.

:- implementation.

:- use_module map.
:- import_module int.
:- import_module list.
:- import_module pair.

:- pred infer_int(context, int, inferred).
:- mode infer_int(in, in, out) is det.
infer_int(Context, Num, ok(mt_int(Context, m_spec(0, 1), Num))).

:- func min_zero(int, int) = int. 
min_zero(A, B) = Result :-
  Sub = A - B, 
  (if Sub < 0 then Result = 0 else Result = Sub).

:- pred composed_spec(m_spec, m_spec, m_spec).
:- mode composed_spec(in, in, out) is det.
composed_spec(m_spec(F, M1), m_spec(M2, T), m_spec(F + min_zero(M2, M1), T + min_zero(M1, M2))).

% 0 1 0 1 -> 0 2 (F, T + M1 - M2)
% 1 0 1 0 -> 2 0 (F + M2 - M1, T)

:- pred infer_compose(term, term, inferred).
:- mode infer_compose(in, in, out) is det.
infer_compose(Term1, Term2, Out) :- 
  infer_term(Term1, Inferred1), 
  infer_term(Term2, Inferred2),
  (if Inferred1 = ok(STerm1), term_spec(STerm1, Spec1)
  then
    (if Inferred2 = ok(STerm2), term_spec(STerm2, Spec2)
    then 
      composed_spec(Spec1, Spec2, Composed),
      Out = ok(mt_compose(Composed, STerm1, STerm2))
    else Out = Inferred2)
  else Out = Inferred1).

:- func map_from_list(list(pair(K, V))) = map.map(K, V).
map_from_list(List) = map.optimize(
  map.det_insert_from_assoc_list(map.init, List)
).

:- func type_map = map.map(m_name, m_spec). 
type_map = map_from_list([
  pair(".", m_spec(1, 0)),
  pair("drop", m_spec(1, 0)),
  pair("dup", m_spec(1, 2)),
  pair("swap", m_spec(2, 2)),
  pair("+", m_spec(2, 1)),
  pair("-", m_spec(2, 1)),
  pair("*", m_spec(2, 1)),
  pair("/", m_spec(2, 1))
]).

:- pred infer_call(context, m_name, inferred).
:- mode infer_call(in, in, out) is det.
infer_call(Context, Name, Out) :- (
  if map.search(type_map, Name, Spec)
  then Out = ok(mt_call(Context, Spec, Name))
  else Out = err(resolve(Name, Context))).

:- pred infer_term(term::in, inferred::out) is det.
infer_term(In, Out) :- ((
  In = mt_int(Context, {}, Num), infer_int(Context, Num, Out)
  ) ; (
  In = mt_compose({}, Term1, Term2), infer_compose(Term1, Term2, Out)
  ) ; (
  In = mt_call(Context, {}, Name), infer_call(Context, Name, Out))).

infer_main(In, Out) :- 
  infer_term(In, Inferred), 
  (if Inferred = ok(SpecTerm), term_spec(SpecTerm, Spec) 
  then
    Expected = m_spec(0, 0),
    (if Spec = Expected
    then Out = ok(SpecTerm)
    else Out = err(unify(Expected, Spec)))
  else Out = Inferred).