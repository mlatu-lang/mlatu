:- module parse.

:- interface.

:- import_module string.
:- import_module context.
:- import_module char.
:- import_module bool.

:- type parse_result(A) ---> pr_ok(A, string, context) ; pr_err(parse_error).

:- type parse_error ---> pe_incomplete ; pe_expected(string, string).

:- type parser(A) == (func(string, context) = parse_result(A)).

:- func map_pr(parse_result(A), func(A) = B) = parse_result(B).

:- func any(string, context) = parse_result(char).

:- func satisfy(func(char) = bool) = parser(char).

:- func char(char) = parser(char).

:- func map(parser(A), func(A) = B) = parser(B).

:- func or(parser(A), parser(A)) = parser(A).

:- implementation.

:- import_module list.


map_pr(Old, Mapper) = New :- (Old = pr_err(Err), New = pr_err(Err)) ; (Old = pr_ok(Result, Rest, Context), New = pr_ok(Mapper(Result), Rest, Context)).

any(String, OldContext) = Result :-
  if first_char(String, Char, Rest)
    then after_char(Char, OldContext, NewContext), Result = pr_ok(Char, Rest, NewContext)
    else Result = pr_err(pe_incomplete).

satisfy(Tester) = 
  (func(String, OldContext) = Result :-
    any(String, OldContext) =AnyResult, 
    (if ( pr_ok(Char, _, _) = AnyResult, no = Tester(Char)) 
      then Result = pr_err(pe_expected("a character satisfying a predicate", from_char_list([Char])))
      else Result = AnyResult)
  ).

map(Parser, Mapper) = (func(String, OldContext) = Result :- 
    Parser(String, OldContext) = OldResult, 
    Result = map_pr(OldResult, Mapper)).

char(Char) = satisfy(func(TestChar) = (if TestChar = Char then yes else no)).

or(Parser1, Parser2) = (func(String, OldContext) = Result :- 
    FirstResult = Parser1(String, OldContext),
    (if (pr_ok(_, _, _) = FirstResult) 
    then Result = FirstResult 
    else Result = Parser2(String, OldContext)
    )).