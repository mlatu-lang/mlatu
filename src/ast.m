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

:- import_module int.
:- import_module string.

:- import_module context.

:- type m_name == string. 

:- type m_term ---> mt_call(m_name, context) ; mt_compose(m_term, m_term, context) ; mt_int(int, context). 

:- func term_string(m_term) = string.

:- pred term_context(m_term::in, context::out) is det.

:- implementation.

:- import_module list.

term_string(Term) = Result :- ((
  mt_call(Name, _) = Term, Result = Name
  ) ; (
  mt_compose(Term1, Term2, _) = Term, 
  Result = append_list([term_string(Term2), " ", term_string(Term1)])
  ) ; (
  mt_int(Num, _) = Term, Result = string.format("%i", [i(Num)]))).

term_context(Term, Context) :- ((
  mt_call(_, Context) = Term
  ) ; (
  mt_compose(_, _, Context) = Term
  ) ; (
  mt_int(_, Context) = Term)).