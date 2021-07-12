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

:- import_module ast.

:- type gs ---> gs(builder :: list(instruction), names :: map(m_name, spec_term)).

:- type instruction ---> a(arith) ; drop ; swap ; dup ; print ; push(int) ; custom(string).

:- type arith ---> add ; sub ; mul ; divide.

:- pred codegen(spec_term, string).
:- mode codegen(in, out) is det.

:- implementation.

:- import_module int.
:- import_module string.

:- pred optimize(list(instruction), list(instruction)).
:- mode optimize(in, out) is det.
optimize([], []).
optimize([Head|Tail], Result) :- (
  if Head = a(A), Tail = [push(Num1)|[push(Num2)|Rest]]
  then optimize(Rest, OptRest), ((
    A = add, 
    Result = [push(Num2+Num1)|OptRest]
    ) ; (
    A = sub, Result = [push(Num2-Num1)|OptRest]
    ) ; (
    A = mul, Result = [push(Num2*Num1)|OptRest]
    ) ; (
    A = divide, Result = [push(Num2/Num1)|OptRest]
    ))
  else 
    if Head = dup, Tail = [push(Num)|Rest]
    then optimize(Rest, OptRest), Result = [push(Num)|[push(Num)|OptRest]]
  else 
    if Head = drop, Tail = [push(_)|Rest]
    then optimize(Rest, Result)
  else 
    if Head = swap, Tail = [push(Num1)|[push(Num2)|Rest]]
    then optimize(Rest, OptRest), Result = [push(Num2)|[push(Num1)|OptRest]]
  else 
    if Head = print, Tail = [push(Num)|Rest]
    then optimize(Rest, OptRest), Result = [custom("puts(\"" ++ string.from_int(Num) ++ "\");")|OptRest]  
  else 
    optimize(Tail, NewTail),
    (if Tail = NewTail
    then Result = [Head|Tail]
    else optimize([Head|NewTail], Result))).

:- pred all_custom(list(instruction), string).
:- mode all_custom(in, out) is semidet.
all_custom([], "").
all_custom([custom(S)|Tail], NewS) :- all_custom(Tail, TailS), NewS = S ++ "\n" ++ TailS.

:- pred instruction_to_c(instruction, string).
:- mode instruction_to_c(in, out) is det. 
instruction_to_c(a(Arith), Result) :- 
  ((Arith = add, Op = "+") ; (Arith = sub, Op = "-") ; (Arith = divide, Op = "/") ; (Arith = mul, Op = "*")),
  Result = join_list("\n", [
  " {",
  "  int first = array[used - 2];",
  "  int second = array[used - 1];",
  "  used--;",
  ("  array[used - 1] = first" ++ Op ++ "second;"),
  " };"]).
instruction_to_c(dup, Result) :- 
  alloc_check(AllocCheck), 
  Result = join_list("\n", [
    " {",
    AllocCheck,
    "  int top = array[used - 1];",
    "  used++;",
    "  array[used - 1] = top;",
    " };"]).
instruction_to_c(drop, " used--;\n").
instruction_to_c(swap, join_list("\n", [
  " {",
  "   int temp = array[used - 2];",
  "   array[used - 2] = array[used - 1];",
  "   array[used - 1] = temp;",
  " };"])).
instruction_to_c(push(Num), Result) :- 
  alloc_check(AllocCheck),
  Result = join_list("\n", [
    AllocCheck,
    " used++;",
    (" array[used - 1] = " ++ string.from_int(Num) ++ ";")]
  ).
instruction_to_c(print, join_list("\n", [
  " printf(\"%d\\n\", array[used - 1]);",
  " used--;"])).
instruction_to_c(custom(X), X).

:- pred alloc_check(string).
:- mode alloc_check(out) is det.
alloc_check(join_list("\n", [
  "  if (used == size) {",
  "   size = (size * 3) / 2 + 8;",
  "   int* newArray = realloc(array, size * sizeof(int));",
  "   if (newArray == NULL) {",
  "    printf(\"\\nExiting!!\n\");",
  "    free(array);",
  "    return 1;",
  "   } else {",
  "    array = newArray;",
  "   }",
  "  };"])).

:- pred insert(instruction, gs, gs). 
:- mode insert(in, in, out) is det.
insert(Instruction, In, In ^ builder := [Instruction|In ^ builder]).

:- pred gen_int(int, gs, gs).
:- mode gen_int(in, in, out) is det.
gen_int(Num, In, Out) :- insert(push(Num), In, Out).

:- pred gen_call(m_name, gs, gs).
:- mode gen_call(in, in, out) is det.
gen_call(Name, In, Out) :- (
  if Name = "." then insert(print, In, Out)
  else if Name = "dup" then insert(dup, In, Out)
  else if Name = "drop" then insert(drop, In, Out)
  else if Name = "swap" then insert(swap, In, Out)
  else if Name = "+" then insert(a(add), In, Out)
  else if Name = "-" then insert(a(sub), In, Out)
  else if Name = "*" then insert(a(mul), In, Out)
  else if Name = "/" then insert(a(divide), In, Out)
  else map.lookup(In ^ names, Name, Term), gen_term(Term, In, Out)
).

:- pred gen_def(m_name, spec_term, gs, gs).
:- mode gen_def(in, in, in, out) is det.
gen_def(Name, Inner, In, Out) :- 
  Out = In ^ names := map.set(In ^ names, Name, Inner).

:- pred gen_term(spec_term, gs, gs).
:- mode gen_term(in, in, out) is det.
gen_term(mt_call(_, _, Name), In, Out) :- gen_call(Name, In, Out).
gen_term(mt_int(_, _, Num), In, Out) :- gen_int(Num, In, Out).
gen_term(mt_compose(_, Term1, Term2), In, Out) :- 
  gen_term(Term1, In, Mid), gen_term(Term2, Mid, Out).
gen_term(mt_def(_, _, Name, Inner), In, Out) :- gen_def(Name, Inner, In, Out).

codegen(Term, Out) :- 
  gen_term(Term, gs([], map.init), OutGs), 
  optimize(OutGs ^ builder, Opt),
  Is = reverse(Opt),
  (if all_custom(Is, String)
  then Out = join_list("\n", [
    "#include <stdio.h>",
    "int main() {", 
    String,
    " return 0;",
    "}"])
  else Out = join_list("\n", [
      "#include <stdio.h>",
      "#include <stdlib.h>",
      "#define INITIAL 8",
      "int main() {",
      " size_t size = INITIAL;",
      " int* array = calloc(size, sizeof(int));", 
      " size_t used = 0;"] ++
      map(func(Instruction) = CString :- instruction_to_c(Instruction, CString), Is) ++
      [" free(array);",
      " return 0;",
      "}"])
  ).