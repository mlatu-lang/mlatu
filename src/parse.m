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

:- module parse.

:- interface.

:- import_module char.
:- import_module list.

:- import_module ast.
:- import_module context.

:- type parse_result(A) ---> pr_ok(A, string, context) ; pr_err(parse_error).

:- type parse_error ---> pe_incomplete ; pe_expected(context, string, string).

:- type parser(A) == (pred(string, context, parse_result(A))).

:- pred map_pr(parse_result(A), func(A) = B, parse_result(B)).
:- mode map_pr(in, in, out) is det.

:- pred any(string, context, parse_result(char)).
:- mode any(in, in, out) is det.

:- pred satisfy(pred(char), string, context, parse_result(char)).
:- mode satisfy(pred(in) is semidet, in, in, out) is det.

:- pred digit(string, context, parse_result(char)).
:- mode digit(in, in, out) is det.

:- pred pure(A, string, context, parse_result(A)).
:- mode pure(in, in, in, out) is det.

:- pred map_p(parser(A), func(A) = B, string, context, parse_result(B)).
:- mode map_p(pred(in, in, out) is det, in, in, in, out) is det.

:- pred char(char, string, context, parse_result(char)).
:- mode char(in, in, in, out) is det.

:- pred or(parser(A), parser(A), string, context, parse_result(A)).
:- mode or(pred(in, in, out) is det, pred(in, in, out) is det, in, in, out) is det.

:- pred ignore_left(parser(A), parser(B), string, context, parse_result(B)).
:- mode ignore_left(pred(in, in, out) is det, pred(in, in, out) is det, in, in, out) is det.

:- pred ignore_right(parser(A), parser(B), string, context, parse_result(A)).
:- mode ignore_right(pred(in, in, out) is det, pred(in, in, out) is det, in, in, out) is det.

:- pred many(parser(A), string, context, parse_result(list(A))).
:- mode many(pred(in, in, out) is det, in, in, out) is det.

:- pred some(parser(A), string, context, parse_result(list(A))).
:- mode some(pred(in, in, out) is det, in, in, out) is det.

:- pred label(parser(A), string, string, context, parse_result(A)).
:- mode label(pred(in, in, out) is det, in, in, in, out) is det.

:- pred between(parser(A), parser(B), parser(C), string, context, parse_result(B)).
:- mode between(pred(in, in, out) is det, pred(in, in, out) is det, pred(in, in, out) is det, in, in, out) is det.

:- pred ignore(parser(A), string, context, parse_result( {} )).
:- mode ignore(pred(in, in, out) is det, in, in, out) is det.

:- pred bracketed(parser(A), string, context, parse_result(A)).
:- mode bracketed(pred(in, in, out) is det, in, in, out) is det.

:- pred space(string, context, parse_result( {} )).
:- mode space(in, in, out) is det.

:- pred identifier(string, context, parse_result(m_name)).
:- mode identifier(in, in, out) is det.

:- pred parse_int(string, context, parse_result(term)).
:- mode parse_int(in, in, out) is det.

:- pred parse_call(string, context, parse_result(term)).
:- mode parse_call(in, in, out) is det.

:- pred parse_lower_term(string, context, parse_result(term)).
:- mode parse_lower_term(in, in, out) is det.

:- pred sep_end_by(parser(A), parser(B), string, context, parse_result(list(A))).
:- mode sep_end_by(pred(in, in, out) is det, pred(in, in, out) is det, in, in, out) is det.

:- pred sep_end_by1(parser(A), parser(B), string, context, parse_result(list(A))).
:- mode sep_end_by1(pred(in, in, out) is det, pred(in, in, out) is det, in, in, out) is det.

:- pred parse_term(string, context, parse_result(term)).
:- mode parse_term(in, in, out) is det.

:- implementation.

:- import_module string.

map_pr(Old, Mapper, New) :- (
    Old = pr_err(Err), 
    New = pr_err(Err)
  ) ; (
    Old = pr_ok(Result, Rest, Context), 
    New = pr_ok(Mapper(Result), Rest, Context)).

any(String, OldContext, Result) :-
  if first_char(String, Char, Rest)
  then 
    after_char(Char, OldContext, NewContext), 
    Result = pr_ok(Char, Rest, NewContext)
  else Result = pr_err(pe_incomplete).

satisfy(Tester, String, Context, Result) :-
  any(String, Context, AnyResult), ((
    pr_ok(Char, _, NewContext) = AnyResult, (
    if Tester(Char) 
    then Result = AnyResult 
    else Result = pr_err(pe_expected(NewContext, "a character satisfying a predicate", from_char_list([Char]))))
  ) ; (
  pr_err(Err) = AnyResult, Result = pr_err(Err))).

digit(String, Context, Result) :- 
  label(satisfy(is_digit), "digit", String, Context, Result).

pure(Value, String, Context, pr_ok(Value, String, Context)).

map_p(Parser, Mapper, String, Context, MappedResult) :- 
    Parser(String, Context, Result), 
    map_pr(Result, Mapper, MappedResult).

char(Char, String, Context, Result) :- label(
  satisfy(pred(TestChar::in) is semidet :- TestChar = Char), from_char_list([Char]), 
  String, Context, Result).

or(Parser1, Parser2, String, Context, Result) :- 
    Parser1(String, Context, FirstResult), ((
    pr_ok(_, _, _) = FirstResult, 
    Result = FirstResult 
    ) ; (
    pr_err(_) = FirstResult, 
    Parser2(String, Context, Result))).

ignore_left(Parser1, Parser2, String, Context, Result) :- 
  Parser1(String, Context, FirstResult), 
  ((
    FirstResult = pr_ok(_, NewString, NewContext),
    Parser2(NewString, NewContext, Result)
  ) ; (
    FirstResult = pr_err(Err),
    Result = pr_err(Err)
  )).

ignore_right(Parser1, Parser2, String, Context, Result) :- 
  Parser1(String, Context, FirstResult),
   ((
    FirstResult = pr_ok(R, NewString, NewContext),
    map_p(Parser2, func(_) = R, NewString, NewContext, Result)
  ) ; (
    FirstResult = pr_err(Err),
    Result = pr_err(Err)
  )).

many(Parser, String, Context, Result) :- 
  or(some(Parser), pure([]), String, Context, Result).

some(Parser, String, Context, Result) :-
  Parser(String, Context, FirstResult),
   ((
    FirstResult = pr_ok(X, NewString, NewContext),
    map_p(many(Parser), func(Xs) = [X|Xs], NewString, NewContext, Result)
  ) ; (
    FirstResult = pr_err(Err),
    Result = pr_err(Err)
  )).

label(Parser, Label, String, Context, Result) :- 
  Parser(String, Context, FirstResult), (
  if pr_err(pe_expected(Context, _, Actual)) = FirstResult
  then Result = pr_err(pe_expected(Context, Label, Actual))
  else Result = FirstResult).

between(First, Middle, Last, String, Context, Result) :- 
  ignore_left(First, ignore_right(Middle, Last), String, Context, Result). 

ignore(Parser, String, Context, Result) :- map_p(Parser, func(_) = {}, String, Context, Result).

bracketed(Middle, String, Context, Result) :- 
  between(char('{'), Middle, char('}'), String, Context, Result).

:- pred is_space(char).
:- mode is_space(in) is semidet.
is_space(C) :- from_int(Int, C), 
  (Int = 9 ; Int = 10 ; Int = 11 ; Int = 12 ; Int = 13 ; Int = 32).

space(String, Context, Result) :- label(
  ignore(satisfy(is_space)), "space", String, Context, Result).

identifier(String, Context, Result) :- map_p(
  some(satisfy(pred(C::in) is semidet :-
    is_alnum_or_underscore(C) ; (from_int(Int, C), (Int = 42 ; Int = 43 ; Int = 45 ;  Int = 46 ; Int = 47)))), 
  from_char_list, String, Context, Result).

parse_int(String, Context, Result) :- label(
  map_p(some(digit), func(Cs) = R :- 
    (if to_int(from_char_list(Cs), Num)  
    then R = mt_int(Context, {}, Num) 
    else R = mt_int(Context, {}, 0))), 
  "number", String, Context, Result).

parse_call(String, Context, Result) :- label(
  (pred(S::in, C::in, R::out) is det :- 
    identifier(S, C, FR), ((
      FR = pr_ok(CallName, NewS, NewC), 
      (if CallName = "def" 
      then label(between(some(space), identifier, some(space)), "name", NewS, NewC, SR), ((
          SR = pr_ok(Name, NewNewS, NewNewC),
          map_p(bracketed(parse_term), func(Term) = mt_def(Context, {}, Name, Term), NewNewS, NewNewC, R)
        ) ; (
          SR = pr_err(Err),
          R = pr_err(Err)
        ))
      else R = pr_ok(mt_call(Context, {}, CallName), NewS, NewC))
    ) ; (
      FR = pr_err(Err),
      R = pr_err(Err)
    ))), "word", String, Context, Result).

parse_lower_term(String, Context, Result) :- 
  or(parse_int, parse_call, String, Context, Result).

sep_end_by(Parser, Sep, String, Context, Result) :- or(
  sep_end_by1(Parser, Sep), 
  pure([]), String, Context, Result).

sep_end_by1(Parser, Sep, String, Context, Result) :- 
  ignore_right(Parser, Sep, String, Context, FirstResult), 
  ((
    pr_ok(X, NewString, NewContext) = FirstResult, 
    map_p(sep_end_by(Parser, Sep), func(Xs) = [X|Xs], NewString, NewContext, Result)
  ) ; (
    pr_err(Err) = FirstResult, 
    pr_err(Err) = Result
  )).

parse_term(String, Context, Result) :- label(ignore_left(many(space), map_p(sep_end_by1(parse_lower_term, some(space)), 
  func(Ts) = foldl(
    func(L, Acc) = R :- (
      if mt_call(builtin_context, {}, "") = Acc 
      then R = L 
      else R = mt_compose({}, Acc, L)), 
    Ts, 
    mt_call(builtin_context, {}, "")
  ))), "term", " " ++ String ++ " ", Context, Result).