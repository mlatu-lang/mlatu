:- module codegen.

:- interface.

:- import_module list.
:- import_module pair.
:- import_module string.

:- import_module ast.

:- type gs ---> gs(labels :: list(pair(string, string)), builder :: string).

:- pred codegen(m_term::in, string::out) is semidet.

:- implementation.

:- import_module assoc_list.

:- pred add_line(string::in, gs::in, gs::out) is det. 
add_line(String, In, Out) :- Out = (In ^ builder := In ^ builder ++ String ++ "\n").

:- pred add_lines(list(string)::in, gs::in, gs::out) is det.
add_lines(Strings, In, Out) :- add_line(join_list("\n", Strings), In, Out).

:- pred gen_def(m_name::in, m_term::in, gs::in, gs::out) is semidet.
gen_def(Name, Term, In, Out) :- 
  gen_term(Term, In, Mid),
  Out = In ^ labels := [pair(Name, Mid ^ builder)|(Mid ^ labels)].

:- pred gen_int(int::in, gs::in, gs::out) is det.
gen_int(Num, In, Out) :- 
  add_line(format("  root = newStackNode(%i, root);", [i(Num)]), In, Out).

:- pred gen_call(m_name::in, gs::in, gs::out) is det.
gen_call(Name, In, Out) :- (
  if Name = "id" 
  then In = Out 
  else if Name = "." 
  then add_lines([
      "  if (root) {",
      "    printf(\"%d\\n\", root->data);",
      "    root = root->next;",
      "   }"], In, Out)
  else if Name = "dup"
  then add_lines([
    "  if(root) {",
    "    root = newStackNode(root->data, root);",
    "  };"], In, Out)
  else if Name = "drop" 
  then add_lines([
    "  if (root) {",
    "    root = root->next;",
    "  };"], In, Out)
  else if Name = "swap"
  then add_lines([
    "  if (root && root->next) {",
    "    int temp = root->next->data;",
    "    root->next->data = root->data;",
    "    root->data = temp;",
    "  }"], In, Out)
  else if Name = "+"
  then add_lines([
    "  if (root && root->next) {",
    "    int temp = root->data;",
    "    root = root->next;",
    "    root->data += temp;",
    "  }"], In, Out)
  else if Name = "-"
  then add_lines([
    "  if (root && root->next) {",
    "    int temp = root->data;",
    "    root = root->next;",
    "    root->data -= temp;",
    "  }"], In, Out)
  else if Name = "*"
  then add_lines([
    "  if (root && root->next) {",
    "    int temp = root->data;",
    "    root = root->next;",
    "    root->data *= temp;",
    "  }"], In, Out)
  else if Name = "/"
  then add_lines([
    "  if (root && root->next) {",
    "    int temp = root->data;",
    "    root = root->next;",
    "    root->data /= temp;",
    "  }"], In, Out)
  else add_line("  " ++ Name ++ "(root);", In, Out)
).

:- pred gen_term(m_term::in, gs::in, gs::out) is semidet.
gen_term(Term, In, Out) :- 
  (Term = mt_call(Name, _), gen_call(Name, In, Out)) ; 
  (Term = mt_int(Num, _), gen_int(Num, In, Out)) ; 
  (Term = mt_compose(Term1, Term2, _), gen_term(Term2, In, Mid), gen_term(Term1, Mid, Out)) ; 
  (Term = mt_def(Name, Body, _), gen_def(Name, Body, In, Out))
.

:- func before = string.
before= join_list("\n", [
  "#include <stdio.h>",
  "#include <stdlib.h>",
  "struct StackNode {",
  "  int data;",
  "  struct StackNode* next;",
  "};",
  "typedef struct StackNode* Stack;",
  "Stack newStackNode(int data, Stack next) {",
  "  Stack stackNode = (Stack) malloc(sizeof(struct StackNode));",
  "  stackNode->data = data;",
  "  stackNode->next = next;",
  "  return stackNode;",
  "};\n\n"]).

codegen(Term, Out) :- 
  gen_def("main", Term, gs([], ""), OutGs), 
  foldl(pred(Name::in, String::in, Acc::in, NewAcc::out) is det :- (if Name = "main" then NewAcc = Acc ++ "int main() {\n  Stack root = NULL;\n" ++ String ++ "  return 0;\n}\n" else NewAcc = "void " ++ Name ++ "(Stack root) {\n" ++ String ++ "}\n" ++ Acc), 
    OutGs ^ labels, 
    OutGs ^ builder, 
    FirstOut
  ), 
  Out = before ++ FirstOut.