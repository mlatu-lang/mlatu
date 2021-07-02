:- module parse.

:- interface.

:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module string.

:- import_module ast.
:- import_module context.

:- type parse_result(A) ---> pr_ok(A, string, context) ; pr_err(parse_error).

:- type parse_error ---> pe_incomplete ; pe_expected(context, string, string).

:- type parser(A) == (func(string, context) = parse_result(A)).

:- func map_pr(parse_result(A), func(A) = B) = parse_result(B).

:- func any(string, context) = parse_result(char).

:- func satisfy(func(char) = bool, string, context) = parse_result(char).

:- func char(char, string, context) = parse_result(char).

:- func digit(string, context) = parse_result(char).

:- func map_p(parser(A), func(A) = B, string, context) = parse_result(B).

:- func or(parser(A), parser(A), string, context) = parse_result(A).

:- func then(parser(A), func(A) = parser(B), string, context) = parse_result(B).

:- func ignore_left(parser(A), parser(B), string, context) = parse_result(B).

:- func ignore_right(parser(A), parser(B), string, context) = parse_result(A).

:- func many(parser(A), string, context) = parse_result(list(A)).

:- func some(parser(A), string, context) = parse_result(list(A)).

:- func label(parser(A), string, string, context) = parse_result(A).

:- func pure(A, string, context) = parse_result(A).

:- func string(string, string, context) = parse_result( {} ).

:- func between(parser(A), parser(B), parser(C), string, context) = parse_result(B).

:- func bracketed(parser(A), string, context) = parse_result(A).

:- func ignore(parser(A), string, context) = parse_result( {} ).

:- func space(string, context) = parse_result( {} ).

:- func identifier(string, context) = parse_result(m_name).

:- func get_context(string, context) = parse_result(context).

:- func parse_int(string, context) = parse_result(m_term).

:- func parse_call(string, context) = parse_result(m_term).

:- func parse_def(string, context) = parse_result(m_term).

:- func parse_term(string, context) = parse_result(m_term).

:- func parse_lower_term(string, context) = parse_result(m_term).

:- func sep_end_by(parser(A), parser(B), string, context) = parse_result(list(A)).

:- func sep_end_by1(parser(A), parser(B), string, context) = parse_result(list(A)).

:- implementation.

:- import_module exception.

map_pr(Old, Mapper) = New :- (
    Old = pr_err(Err), 
    New = pr_err(Err)
  ) ; (
    Old = pr_ok(Result, Rest, Context), 
    New = pr_ok(Mapper(Result), Rest, Context)).

any(String, OldContext) = Result :-
  if first_char(String, Char, Rest)
  then 
    after_char(Char, OldContext, NewContext), 
    Result = pr_ok(Char, Rest, NewContext)
  else Result = pr_err(pe_incomplete).

satisfy(Tester, String, Context) = Result :-
  any(String, Context) = AnyResult, ((
    pr_ok(Char, _, NewContext) = AnyResult, (
    if Tester(Char) = yes
    then Result = AnyResult 
    else Result = pr_err(pe_expected(NewContext, "a character satisfying a predicate", from_char_list([Char]))))
  ) ; (
  pr_err(Err) = AnyResult, Result = pr_err(Err))).

digit(String, Context) = label(
  satisfy(func(C) = (if is_digit(C) then yes else no)), 
  "digit", String, Context).

pure(Value, String, Context) = pr_ok(Value, String, Context).

map_p(Parser, Mapper, String, Context) = MappedResult :- 
    Parser(String, Context) = Result, 
    MappedResult = map_pr(Result, Mapper).

char(Char, String, Context) = label(
  satisfy(func(TestChar) = (if TestChar = Char then yes else no)), from_char_list([Char]), 
  String, Context).

or(Parser1, Parser2, String, Context) = Result :- 
    FirstResult = Parser1(String, Context),
    (if (pr_ok(_, _, _) = FirstResult) 
    then Result = FirstResult 
    else Result = Parser2(String, Context)
    ).

then(Parser1, Parser2, String, Context) = Result :- 
    FirstResult = Parser1(String, Context), ((
      pr_ok(X, NewString, NewContext) = FirstResult, 
      Result = Parser2(X)(NewString, NewContext)
    ) ; (
      pr_err(Err) = FirstResult, 
      Result = pr_err(Err) )).

ignore_left(Parser1, Parser2, String, Context) = 
  then(Parser1, func(_) = Parser2, String, Context).

ignore_right(Parser1, Parser2, String, Context) = 
  then(Parser1, 
    func(Result) = map_p(Parser2, func(_) = Result), String, Context).

many(Parser, String, Context) = or(some(Parser), pure([]), String, Context).

some(Parser, String, Context) = 
  then(Parser, 
  func(X) = map_p(many(Parser), func(Xs) = [X|Xs]), String, Context).

label(Parser, Label, String, Context) = Result :- 
  FirstResult = Parser(String, Context), (
  if pr_err(pe_expected(Context, _, Actual)) = FirstResult
  then Result = pr_err(pe_expected(Context, Label, Actual))
  else Result = FirstResult).

string(Message, String, Context) = ignore(
  label(
    foldl(
      func(Elem, Acc) = then(Acc, func(Xs) = map_p(Elem, func(X) = [X|Xs])), 
      map(
        func(C) = (func(S, Ctxt) = char(C, S, Ctxt)), 
        to_char_list(Message)), 
      pure([])), 
    Message), String, Context).

between(First, Middle, Last, String, Context) = 
  ignore_left(First, ignore_right(Middle, Last), String, Context). 

ignore(Parser, String, Context) = map_p(Parser, func(_) = {}, String, Context).

bracketed(Middle, String, Context) = 
  between(char('{'), Middle, char('}'), String, Context).

space(String, Context) = label(
  ignore(
    satisfy(func(C) = ( if C = ' ' ; C = '\t'; C = '\n' then yes else no))), 
  "space", String, Context).

get_context(String, Context) = pr_ok(Context, String, Context).

identifier(String, Context) = map_p(
  some(satisfy(func(C) = (
    if is_alnum_or_underscore(C) ; (from_int(Int, C), (Int = 43 ; Int = 46 ; Int = 43 ; Int = 45 ; Int = 42))
    then yes 
    else no))), 
  from_char_list, String, Context).

parse_int(String, Context) = label(
  then(get_context, (func(Ctxt) = 
    map_p(some(digit), func(Cs) = Result :- 
    ((to_int(from_char_list(Cs), Num), 
      Result = mt_int(Num, Ctxt) 
    ) ; exception.throw("Should never occur"))))), 
  "number", String, Context).

parse_call(String, Context) = label(
  then(get_context, func(Ctxt) = 
    map_p(identifier, func(I) = mt_call(I, Ctxt))), 
  "word", String, Context).

parse_def(String, Context) = then(
  get_context, 
  func(Ctxt) = 
    ignore_left(
      string("def"), 
      then(
        between(some(space), identifier, some(space)), 
        func(Name) = 
            map_p(bracketed(between(many(space), parse_term, many(space))), 
            func(Term) = mt_def(Name, Term, Ctxt)))), String, Context).

parse_lower_term(String, Context) = or(
  parse_def, 
  or(parse_int, parse_call), String, Context).

sep_end_by(Parser, Sep, String, Context) = or(
  sep_end_by1(Parser, Sep), 
  pure([]), String, Context).

sep_end_by1(Parser, Sep, String, Context) = then(
  ignore_right(Parser, Sep), func(X) = 
  map_p(sep_end_by(Parser, Sep), func(Xs) = [X|Xs]), String, Context).

parse_term(String, Context) = label(map_p(sep_end_by1(parse_lower_term, some(space)), 
  func(Ts) = foldl(
    func(L, Acc) = Result :- (
      if mt_call("id", builtin_context) = Acc 
      then Result = L 
      else term_context(L, Ctxt), Result = mt_compose(L, Acc, Ctxt)), 
    Ts, 
    mt_call("id", builtin_context)
  )), "term", String, Context).