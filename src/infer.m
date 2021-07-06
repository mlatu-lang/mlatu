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

:- pred infer_term(term, spec_term).
:- mode infer_term(in, out) is det.

:- implementation.

:- import_module uint.

:- import_module context.

:- pred infer_int(context, int, spec_term).
:- mode infer_int(in, in, out) is det.
infer_int(Context, Num, Out) :- Out = mt_int(Context, m_spec(0u, 1u), Num).

:- pred find_minimum(uint, uint, uint, uint, uint, uint).
:- mode find_minimum(in, in, in, in, out, out) is det.
find_minimum(A1, A2, A3, A4, NewIn, NewOut) :- 
  if A2 > A3 
  then find_minimum(A1, A2, A3 + 1u, A4 + 1u, NewIn, NewOut)
  else if A3 > A2 
  then find_minimum(A1 + 1u, A2 + 1u, A3, A4, NewIn, NewOut)
  else NewIn = A1, NewOut = A4.

:- pred infer_compose(term, term, spec_term).
:- mode infer_compose(in, in, out) is det.
infer_compose(Term1, Term2, Out) :- 
  infer_term(Term1, STerm1), term_spec(STerm1, Spec1), 
  infer_term(Term2, STerm2), term_spec(STerm2, Spec2),
  (if Spec1 = m_spec(F1, T1), Spec2 = m_spec(F2, T2)
  then 
    find_minimum(F2, T2, F1, T1, NewIn, NewOut),
    Out = mt_compose(m_spec(NewIn, NewOut), STerm1, STerm2)
  else Out = mt_compose(Spec1, STerm1, STerm2)).

:- pred infer_call(context, m_name, spec_term).
:- mode infer_call(in, in, out) is det.
infer_call(Context, Name, mt_call(Context, Spec, Name)) :- (
  if "." = Name ; "drop" = Name then Spec = m_spec(1u, 0u)
  else if "dup" = Name then Spec = m_spec(1u, 2u)
  else if "swap" = Name then Spec = m_spec(2u, 2u)
  else if "+" = Name ; "-" = Name ; "*" = Name ; "/" = Name then Spec = m_spec(2u, 1u)
  else Spec = ms_err(Name)).


infer_term(In, Out) :- ((
  In = mt_int(Context, {}, Num), infer_int(Context, Num, Out)
  ) ; (
  In = mt_compose({}, Term1, Term2), infer_compose(Term1, Term2, Out)
  ) ; (
  In = mt_call(Context, {}, Name), infer_call(Context, Name, Out))).