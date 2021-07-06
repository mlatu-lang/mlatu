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

:- module ast.

:- interface.

:- import_module string.

:- import_module context.

:- type m_name == string. 

:- type m_term(Info) ---> mt_call(context, Info, m_name) ; mt_compose(Info, m_term(Info), m_term(Info)) ; mt_int(context, Info, int). 

:- type m_spec ---> m_spec(from :: uint, to :: uint) ; ms_err(m_name).

:- type term == m_term({}).

:- type spec_term == m_term(m_spec).

:- pred term_string(m_term(Info), string).
:- mode term_string(in, out) is det.

:- pred term_context(m_term(Info)::in, context::out) is det.

:- pred term_spec(spec_term, m_spec).
:- mode term_spec(in, out) is det.

:- pred spec_string(m_spec, string).
:- mode spec_string(in, out) is det.

:- implementation.

:- import_module list.
:- import_module uint.

term_string(Term, Result) :- (
  mt_call(_, _, Name) = Term, Result = Name
  ) ; (
  mt_compose(_, Term1, Term2) = Term, 
  term_string(Term1, String1),
  term_string(Term2, String2),
  Result = String1 ++ " " ++ String2
  ) ; (
  mt_int(_, _, Num) = Term, Result = string.format("%i", [i(Num)])).

term_context(Term, Context) :- ((
  mt_call(Context, _, _) = Term
  ) ; (
  mt_compose(_, A, _) = Term, term_context(A, Context)
  ) ; (
  mt_int(Context, _, _) = Term)).

term_spec(Term, Spec) :- (
  mt_call(_, Spec, _) = Term 
  ) ; (
  mt_compose(Spec, _, _) = Term
  ) ; (
  mt_int(_, Spec, _) = Term).

:- func replicate(uint, string) = string.
replicate(Num, String) = Result :- (
  if Num = 0u
  then Result = ""
  else Result = String ++ replicate(Num - 1u, String)).

spec_string(Spec, String) :-
  (m_spec(From, To) = Spec, String = "(" ++ replicate(From, "x ") ++ "->" ++ replicate(To, " x") ++ ")") ; 
  (ms_err(Name) = Spec, String = "ERROR: Unresolved name `" ++ Name ++ "`").