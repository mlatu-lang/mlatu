:- module codegen.

:- interface.

:- import_module string.

:- import_module ast.

:- type gs ---> gs(builder :: string).

:- pred codegen(m_term::in, string::out) is semidet.

:- implementation.

:- import_module list.

:- pred add_line(string::in, gs::in, gs::out) is det. 
add_line(String, In, Out) :- Out = (In ^ builder := In ^ builder ++ String ++ "\n").

:- pred add_lines(list(string)::in, gs::in, gs::out) is det.
add_lines(Strings, In, Out) :- add_line(join_list("\n", Strings), In, Out).

:- func alloc_check = string.
alloc_check = join_list("\n", [
  " if (used == size) {",
  "  size = (size * 3) / 2 + 8;",
  "  int* newArray = realloc(array, size * sizeof(int));",
  "  if (newArray == NULL) {",
  "   printf(\"\\nExiting!!\");",
  "   free(array);",
  "   exit(0);",
  "  } else {",
  "   array = newArray;",
  "  }",
  " };"]).

:- pred gen_int(int::in, gs::in, gs::out) is det.
gen_int(Num, In, Out) :- 
  add_lines([
    alloc_check,
    " used++;",format(
    " array[used - 1] = %i;", [i(Num)])
    ], In, Out).

:- pred gen_call(m_name::in, gs::in, gs::out) is det.
gen_call(Name, In, Out) :- (
  if Name = "id" 
  then In = Out 
  else if Name = "." 
  then add_lines([
    " printf(\"%d\\n\", array[used - 1]);",
    " used--;"
    ], In, Out)
  else if Name = "dup"
  then add_lines([
    alloc_check,
    " int top = array[used - 1];",
    " used++;",
    " array[used - 1] = top;"], In, Out)
  else if Name = "drop" 
  then add_lines([
    " used--;"], In, Out)
  else if Name = "swap"
  then add_lines([
    " int temp = array[used - 2];",
    " array[used - 2] = array[used - 1];",
    " array[used - 1] = temp;"], In, Out)
  else if Name = "+"
  then add_lines([
    " int first = array[used - 2];",
    " int second = array[used - 1];",
    " used--;",
    " array[used - 1] = first + second;"], In, Out)
  else if Name = "-"
  then add_lines([
    " int first = array[used - 2];",
    " int second = array[used - 1];",
    " used--;",
    " array[used - 1] = first - second;"], In, Out)
  else if Name = "*"
  then add_lines([
    " int first = array[used - 2];",
    " int second = array[used - 1];",
    " used--;",
    " array[used - 1] = first * second;"], In, Out)
  else if Name = "/"
  then add_lines([
    " int first = array[used - 2];",
    " int second = array[used - 1];",
    " used--;",
    " array[used - 1] = first / second;"], In, Out)
  else add_line(" " ++ Name ++ "(&array, used, size);", In, Out)
).

:- pred gen_term(m_term::in, gs::in, gs::out) is semidet.
gen_term(Term, In, Out) :- 
  (Term = mt_call(Name, _), gen_call(Name, In, Out)) ; 
  (Term = mt_int(Num, _), gen_int(Num, In, Out)) ; 
  (Term = mt_compose(Term1, Term2, _), gen_term(Term2, In, Mid), gen_term(Term1, Mid, Out))
.

:- func before = string.
before= join_list("\n", [
  "#include <stdio.h>",
  "#include <stdlib.h>",
  "#define INITIAL 8"]).

codegen(Term, Out) :- 
  gen_term(Term, gs(""), OutGs), 
  Out = join_list("\n", [before,
      "int main() {",
      " size_t size = INITIAL;",
      " int* array = calloc(size, sizeof(int));", 
      " size_t used = 0;",
      OutGs ^ builder,
      " free(array);",
      " return 0;",
      "}"]).