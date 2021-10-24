-module(mlatu). -export([interact/1]). 
driver([], Prefix) -> Prefix;
driver([Head|Tail], Prefix) ->
  case find_redex([Head|Tail]) of  
    {redex_found, New} -> driver(lists:reverse(Prefix, New), []); 
    {redex_not_found} -> driver(Tail, [Head|Prefix])
end.
find_redex(List) -> case List of 
[{word, 'unquote'}|Rest] -> {redex_found,[{word, 'u'}|Rest]};
[{word, 'remove'}|Rest] -> {redex_found,[{word, 'r'}|Rest]};
[{word, 'quote'}|Rest] -> {redex_found,[{word, 'q'}|Rest]};
[{word, 'concat'}|Rest] -> {redex_found,[{word, 'c'}|Rest]};
[{word, 'dup'}|Rest] -> {redex_found,[{word, 'd'}|Rest]};
[{word, 'swap'}|Rest] -> {redex_found,[{word, 's'}|Rest]};

  [{word, 'u'}, {quotation, A} | Rest] -> {redex_found, A ++ Rest};
  [{word, 'q'}, A | Rest] -> {redex_found, [{quotation, [A]} | Rest]};
  [{word, 'c'}, {quotation, List1}, {quotation, List2} | Rest] -> 
   {redex_found, [{quotation, List1 ++ List2} | Rest]};
  [{word, 'd'}, A | Rest] -> {redex_found, [A, A | Rest]};
  [{word, 'r'}, _ | Rest] -> {redex_found, Rest};
  [{word, 's'}, A, B | Rest] -> {redex_found, [B, A | Rest]};
  _ -> {redex_not_found}
end.
print_term({word, Atom}) -> atom_to_list(Atom);
print_term({quotation, List}) -> "(" ++ print_terms(List) ++ ")".
print_terms([]) -> " ";
print_terms([Term|List]) -> print_term(Term) ++ " " ++ print_terms(List). 
interact(List) -> print_terms(driver(List, [])).