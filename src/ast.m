:- module ast.

:- interface.

:- import_module int.
:- import_module string.

:- import_module context.

:- type m_name == string. 

:- type m_term ---> mt_call(m_name, context) ; mt_compose(m_term, m_term, context) ; mt_def(m_name, m_term, context) ; mt_int(int, context). 

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
  mt_def(Name, Body, _) = Term, 
  Result = append_list(["def ", Name, " { ", term_string(Body), " }"])
  ) ; (
  mt_int(Num, _) = Term, Result = string.format("%i", [i(Num)]))).

term_context(Term, Context) :- ((
  mt_call(_, Context) = Term
  ) ; (
  mt_compose(_, _, Context) = Term
  ) ; (
  mt_def(_, _, Context) = Term
  ) ; (
  mt_int(_, Context) = Term)).