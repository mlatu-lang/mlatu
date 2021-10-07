:- module(mlatu, [rewrite/2, user_equiv/2]).
:- dynamic([user_equiv/2], [multifile, discontiguous]).

equiv(Xs, Other) :- user_equiv(Xs, Other).

equiv(['d',X|Xs], Other) :-
    equiv([X,X|Xs], Other).

equiv(['r',_|Xs], Other) :-
    equiv(Xs, Other).

equiv(['u',[]|Xs], Other) :- 
    equiv(Xs, Other).
equiv(['u',[First|Rest]|Xs], Other) :-
    append([First|Rest], Xs, NewXs), equiv(NewXs, Other)
    .

equiv(['q',X|Xs], Other) :-
    equiv([[X]|Xs], Other).

equiv(['c',[],[]|Xs], Other) :-
    equiv([[]|Xs], Other).
equiv(['c',[],[First|Rest]|Xs], Other) :-
    equiv([[First|Rest]|Xs], Other).
equiv(['c',[First|Rest],[]|Xs], Other) :-
    equiv([[First|Rest]|Xs], Other).
equiv(['c',[AFirst|ARest],[BFirst|BRest]|Xs], Other) :-
    append([AFirst|ARest], [BFirst|BRest], NewQuote),
    equiv([NewQuote|Xs], Other).

equiv(['s',A,B|Xs], Other) :-
    equiv([B,A|Xs],Other).

equiv([], []).
equiv([X|Xs], Other) :-
    equiv(Xs, New),
    (Xs = New ->
         Other = [X|Xs] ;
    equiv([X|New], Other)).

rewrite(List, Other) :-
    equiv(List, Other), List \= Other, !.