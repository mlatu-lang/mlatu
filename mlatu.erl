-module(mlatu). -export([interact/1]). 
spawn_rewrite(List, IfOk, IfNone) -> Self = self(), spawn(fun() -> inner_rewrite(List, Self) end), receive {ok, NewRest} -> IfOk(NewRest); {none} -> IfNone() end.
no_rewrite(Pid) -> Pid ! {none}.
rewrite_to(Pid, New) -> Pid ! {ok, New}.
inner_rewrite(List, Parent) -> case List of 
  [] -> no_rewrite(Parent);
  [{word, u}, {quote, List} | Rest] -> rewrite_to(Parent, List ++ Rest);
  [{word, q}, A | Rest] -> rewrite_to(Parent, [{quote, [A]} | Rest]);
  [{word, c}, {quote, List1}, {quote, List2} | Rest] -> rewrite_to(Parent, [{quote, List1 ++  List2} | Rest]);
  [{word, d}, A | Rest] -> rewrite_to(Parent, [A, A | Rest]);
  [{word, r}, _ | Rest] -> rewrite_to(Parent, Rest);
  [{word, s}, A, B | Rest] -> rewrite_to(Parent, [B, A | Rest]);
  [First | Rest] -> spawn_rewrite(Rest, fun(NewRest) -> spawn(fun() -> inner_rewrite([First | NewRest], Parent) end) end, fun() -> no_rewrite(Parent) end)
    end.
print_term({word, Atom}) -> atom_to_list(Atom);
print_term({quote, List}) -> "(" ++ print_terms(List) ++ ")".
print_terms(MTerms) -> string:trim(lists:foldl(fun(Term, S) -> print_term(Term) ++ " " ++ S end, "", MTerms)). 
interact(Original) ->
    spawn_rewrite(Original, fun(New) -> print_terms(New) end, fun() -> print_terms(Original) end).
