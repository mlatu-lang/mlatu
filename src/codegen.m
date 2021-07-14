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

:- module codegen.

:- interface.

:- import_module list.
:- import_module map.
:- import_module bool.

:- import_module ast.

:- type gs ---> gs(builder :: list(instruction), names :: map(m_name, string)).

:- type instruction ---> a2(ar2) ; a1(ar1) ; a0(ar0) ; push(int).

:- type ar2 ---> add ; sub ; mul ; divide ; swap.

:- type ar1 ---> print ; drop ; dup.

:- type ar0 ---> custom(string) ; fun(string, list(instruction)) ; call_fun(string).

:- pred codegen(spec_terms, bool, string).
:- mode codegen(in, in, out) is det.

:- implementation.

:- import_module int.
:- import_module string.

:- func count_calls(list(instruction), string) = int.
count_calls([], _) = 0.
count_calls([Head|Tail], Name) = Count :-
  if Head = a0(call_fun(Name))
  then Count = 1 + count_calls(Tail, Name)
  else if Head = a0(fun(_, Body)), BodyCount = count_calls(Body, Name)
  then Count = BodyCount + count_calls(Tail, Name)
  else Count = count_calls(Tail, Name).

:- pred should_inline(list(instruction), string).
:- mode should_inline(in, in) is semidet.
should_inline(In, Name) :- count_calls(In, Name) =< 2.

:- pred inline(list(instruction), string, list(instruction), list(instruction)).
:- mode inline(in, in, in, out) is det.
inline([], _, _, []).
inline([Head|Tail], Name, Body, Out) :- 
  if Head = a0(call_fun(Name))
  then inline(Body ++ Tail, Name, Body, Out)
  else if Head = a0(fun(FunName, FunBody)), inline(FunBody, Name, Body, NewBody)
  then 
    inline(Tail, Name, Body, NewTail), Out = [a0(fun(FunName, NewBody))|NewTail]
  else inline(Tail, Name, Body, NewTail), Out = [Head|NewTail].

:- pred optimize(list(instruction), list(instruction)).
:- mode optimize(in, out) is det.
optimize([], []).
optimize([Head|Tail], Result) :- 
  (if Head = push(Num1), Tail = [push(Num2)|[a2(A)|Rest]] 
  then 
    (A = add, optimize([push(Num1 + Num2)|Rest], Result)
    ) ; (A = sub, optimize([push(Num1 - Num2)|Rest], Result)
    ) ; (A = mul, optimize([push(Num1 * Num2)|Rest], Result)
    ) ; (A = divide, optimize([push(Num1 / Num2)|Rest], Result)
    ) ; (A = swap, optimize([push(Num2)|[push(Num1)|Rest]], Result))
  else if Head = push(Num), Tail = [a1(A)|Rest]
  then 
    ((A = drop, optimize(Rest, Result)) ; (
      A = print, 
      optimize(Rest, OptRest), 
      Result = [a0(custom("writeln(" ++ from_int(Num) ++ ");"))|OptRest]
    ) ; (A = dup, optimize([push(Num)|[push(Num)|Rest]], Result)))
  else (
    Head = a0(A),
    (((A = call_fun(_) ; A = custom(_)),
    optimize(Tail, Rest),
    Result = [a0(A)|Rest]
    ) ; (
      A = fun(Name, Body),
      (if should_inline(Tail, Name)
      then 
        inline(Tail, Name, Body, Inlined), optimize(Inlined, Result)
      else 
        optimize(Body, OptBody),
        optimize(Tail, Rest),
        Result = [a0(fun(Name, OptBody))|Rest])))
  ) ; ( 
    ((Head = push(_) ; Head = a2(_) ; Head = a1(_)),
    optimize(Tail, OptTail),
    Result = [Head|OptTail])
  )).

:- pred all_custom(list(instruction), string).
:- mode all_custom(in, out) is semidet.
all_custom([], "").
all_custom([a0(custom(S))|Tail], NewS) :- 
  all_custom(Tail, TailS), NewS = "    " ++ S ++ "\n" ++ TailS.

:- func to_d(instruction) = list(string).
to_d(a2(A)) = Result :- 
  ((A = add, Op = ["stack.insert(first + second);"]) ; 
  (A = sub, Op = ["stack.insert(first - second);"]) ; 
  (A = divide, Op = ["stack.insert(first / second);"]) ; 
  (A = mul, Op = ["stack.insert(first * second);"]) ; 
  (A = swap, Op = ["stack.insert(second);", "stack.insert(first);"])),
  Result = [
  "{",
  "    elem second = stack.front;",
  "    stack.removeFront();",
  "    elem first = stack.front;",
  "    stack.removeFront();"] ++ map(indent, Op) ++ ["}"].
to_d(a1(A)) = Result :- (
    A = dup, 
    Result = ["stack.insert(stack.front);"]
  ) ; (
    A = drop, 
    Result = ["stack.removeFront();"]
  ) ; (
    A = print,
    Result = ["writeln(stack.front);", "stack.removeFront();"]
  ).
to_d(push(Num)) = ["stack.insert(" ++ string.from_int(Num) ++ ");"].
to_d(a0(fun(Name, Inner))) = 
  ["static void " ++ Name ++ "(ref SList!elem stack)", 
  "{"] ++ map(indent, condense(map(to_d, Inner))) ++ 
  ["}\n"].
to_d(a0(call_fun(Name))) = [Name ++ "(stack);"].
to_d(a0(custom(X))) = [X].

:- func indent(string) = string.
indent(S) = "    " ++ S.

:- pred insert(instruction, gs, gs). 
:- mode insert(in, in, out) is det.
insert(Instruction, In, In ^ builder := [Instruction|In ^ builder]).

:- pred gen_int(int, gs, gs).
:- mode gen_int(in, in, out) is det.
gen_int(Num, In, Out) :- insert(push(Num), In, Out).

:- pred gen_call(m_name, gs, gs).
:- mode gen_call(in, in, out) is det.
gen_call(Name, In, Out) :- (
  if Name = "." then insert(a1(print), In, Out)
  else if Name = "dup" then insert(a1(dup), In, Out)
  else if Name = "drop" then insert(a1(drop), In, Out)
  else if Name = "swap" then insert(a2(swap), In, Out)
  else if Name = "+" then insert(a2(add), In, Out)
  else if Name = "-" then insert(a2(sub), In, Out)
  else if Name = "*" then insert(a2(mul), In, Out)
  else if Name = "/" then insert(a2(divide), In, Out)
  else map.lookup(In ^ names, Name, FunName), insert(a0(call_fun(FunName)), In, Out)
).

:- pred gen_def(m_name, spec_terms, gs, gs).
:- mode gen_def(in, in, in, out) is det.
gen_def(Name, Inner, In, Mid ^ names := map.set(In ^ names, Name, Name)) :- 
  insert(a0(fun(Name, reverse(InnerOut ^ builder))), In, Mid), 
  gen_terms(Inner, gs([], In ^ names), InnerOut).

:- pred gen_term(spec_term, gs, gs).
:- mode gen_term(in, in, out) is det.
gen_term(mt_call(_, _, Name), In, Out) :- gen_call(Name, In, Out).
gen_term(mt_int(_, _, Num), In, Out) :- gen_int(Num, In, Out).
gen_term(mt_def(_, _, Name, Inner), In, Out) :- gen_def(Name, Inner, In, Out).

:- pred gen_terms(spec_terms, gs, gs).
:- mode gen_terms(in, in, out) is det.
gen_terms(mts(_, _, List), In, Out) :- foldl(gen_term, List, In, Out).

codegen(Terms, ShouldOpt, Out) :- 
  gen_terms(Terms, gs([], map.init), OutGs), 
  ((ShouldOpt = yes, optimize(reverse(OutGs ^ builder), Is)) ;
  (ShouldOpt = no, Is = reverse(OutGs ^ builder))),
  (if all_custom(Is, String)
  then Out = join_list("\n", [
    "import std.stdio;",
    "void main() {", 
    String ++ "}"])
  else Out = join_list("\n", [
      "import std.stdio;",
      "import std.container.slist;\n",
      "alias elem = int;",
      "void main()",
      "{"] ++
      map(indent, 
        ["SList!elem stack = SList!elem();"|condense(map(to_d, Is))]
      ) ++ ["}"])
  ).