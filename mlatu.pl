:- module(mlatu, [rewrite/2, user_equiv/2]).
:- dynamic([user_equiv/2], [multifile, discontiguous]).

equiv(Xs, Other) :-
    user_equiv(Xs, Other) ;
    builtin_equiv(Xs, Other).

builtin_equiv(['d',X|Xs], Other) :-
    equiv([X,X|Xs], Other).

builtin_equiv(['r',_|Xs], Other) :-
    equiv(Xs, Other).

builtin_equiv(['u',Quote|Xs], Other) :-
    is_list(Quote),
    append(Quote, Xs, NewXs),
    equiv(NewXs, Other).

builtin_equiv(['q',X|Xs], Other) :-
    equiv([[X]|Xs], Other).

builtin_equiv(['c',Quote1,Quote2|Xs], Other) :-
    is_list(Quote1), is_list(Quote2),
    append(Quote1, Quote2, NewQuote),
    equiv([NewQuote|Xs], Other).

builtin_equiv(['s',A,B|Xs], Other) :-
    equiv([B,A|Xs],Other).

builtin_equiv([], []).
builtin_equiv([X|Xs], Other) :-
    equiv(Xs, New),
    (Xs \= New ->
        equiv([X|New], Other) ;
        Other = [X|Xs]).

rewrite(List, Other) :-
    equiv(List, Other),
    List \= Other.
