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

:- pred codegen(spec_term::in, string::out) is det.

:- implementation.

:- import_module int.
:- import_module string.

:- func fold_constants(list(instruction)) = list(instruction).
fold_constants([]) = [].
fold_constants([Head|Tail]) = Result :- 
  if Head = a(A), Tail = [push(Num1)|[push(Num2)|Rest]] 
  then (
    A = add, Result = [push(Num2+Num1)|fold_constants(Rest)]
    ) ; (
    A = sub, Result = [push(Num2-Num1)|fold_constants(Rest)]
    ) ; (
    A = mul, Result = [push(Num2*Num1)|fold_constants(Rest)]
    ) ; (
    A = divide, Result = [push(Num2/Num1)|fold_constants(Rest)]
    )
  else if Head = dup, Tail = [push(Num)|Rest] 
  then Result = [push(Num)|[push(Num)|fold_constants(Rest)]]
  else if Head = drop, Tail = [push(_)|Rest]
  then Result = fold_constants(Rest)
  else if Head = swap, Tail = [push(Num1)|[push(Num2)|Rest]]
  else if Head = print, Tail = [push(Num)|Rest]
  then Result = [custom("printf(\"" ++ string.from_int(Num) ++ "\");")|fold_constants(Rest)]
  then Result = [push(Num2)|[push(Num1)|Rest]]
  
  else
    NewTail = fold_constants(Tail),
    (if Tail = NewTail 
    then Result = [Head|Tail]
    else Result = fold_constants([Head|NewTail])).

:- pred all_custom(list(instruction)::in, string::out) is semidet.
all_custom([], "").
all_custom([custom(S)|Tail], NewS) :- all_custom(Tail, TailS), NewS = S ++ "\n" ++ TailS.

:- func instruction_to_c(instruction) = string. 
instruction_to_c(a(Arith)) = Result :- 
  ((Arith = add, Op = "+") ; (Arith = sub, Op = "-") ; (Arith = divide, Op = "/") ; (Arith = mul, Op = "*")),
  Result = join_list("\n", [
  " {",
  "  int first = array[used - 2];",
  "  int second = array[used - 1];",
  "  used--;",
  ("  array[used - 1] = first" ++ Op ++ "second;"),
  " };"]).
instruction_to_c(dup) = join_list("\n", [
  " {",
  alloc_check,
  "  int top = array[used - 1];",
  "  used++;",
  "  array[used - 1] = top;",
  " };"]).
instruction_to_c(drop) = " used--;\n".
instruction_to_c(swap) = join_list("\n", [
  " {",
  "   int temp = array[used - 2];",
  "   array[used - 2] = array[used - 1];",
  "   array[used - 1] = temp;",
  " };"]).
instruction_to_c(push(Num)) = join_list("\n", [
  alloc_check,
  " used++;",
  (" array[used - 1] = " ++ string.from_int(Num) ++ ";")]).
instruction_to_c(print) = join_list("\n", [
  " printf(\"%d\\n\", array[used - 1]);",
  " used--;"]).
instruction_to_c(custom(X)) = X.

:- func alloc_check = string.
alloc_check = join_list("\n", [
  "  if (used == size) {",
  "   size = (size * 3) / 2 + 8;",
  "   int* newArray = realloc(array, size * sizeof(int));",
  "   if (newArray == NULL) {",
  "    printf(\"\\nExiting!!\");",
  "    free(array);",
  "    return 1;",
  "   } else {",
  "    array = newArray;",
  "   }",
  "  };"]).

:- pred insert(instruction::in, gs::in, gs::out) is det. 
insert(Instruction, In, In ^ builder := [Instruction|In ^ builder]).

:- pred gen_int(int::in, gs::in, gs::out) is det.
gen_int(Num, In, Out) :- insert(push(Num), In, Out).

:- pred gen_call(m_name::in, gs::in, gs::out) is det.
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

:- pred gen_def(m_name::in, spec_term::in, gs::in, gs::out) is det.
gen_def(Name, Inner, In, Out) :- 
  Out = In ^ names := map.set(In ^ names, Name, Inner).

:- pred gen_term(spec_term::in, gs::in, gs::out) is det.
gen_term(mt_call(_, _, Name), In, Out) :- gen_call(Name, In, Out).
gen_term(mt_int(_, _, Num), In, Out) :- gen_int(Num, In, Out).
gen_term(mt_compose(_, Term1, Term2), In, Out) :- 
  gen_term(Term1, In, Mid), gen_term(Term2, Mid, Out).
gen_term(mt_def(_, _, Name, Inner), In, Out) :- gen_def(Name, Inner, In, Out).

codegen(Term, Out) :- 
  gen_term(Term, gs([], map.init), OutGs), 
  Is = reverse(fold_constants(OutGs ^ builder)),
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
      map(instruction_to_c, Is) ++
      [" free(array);",
      " return 0;",
      "}"])
  ).