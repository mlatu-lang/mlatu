:- module context.

:- interface.

:- import_module string. 
:- import_module char.

:- type context ---> context(c_file :: string, c_line :: int, c_col :: int).
:- func context(string) = context. 
:- func context(string, int) = context. 

:- func nil_context = context. 

:- pred is_nil_context(context::in) is semidet.

:- func builtin_context = context. 

:- func command_line_context = context. 

:- func context_string(context) = string. 
:- func context_string(string, context) = string. 

:- pred after_char(char, context, context).
:- mode after_char(in, in, out) is det.
:- mode after_char(in, in, in) is semidet.

:- implementation. 

:- import_module list. 
:- import_module int.

context(Name) = context(Name, 0, 0).
context(Name, Line) = context(Name, Line, 0).

nil_context = context("").

is_nil_context(context("", _, _)).

builtin_context = context("builtin").

command_line_context = context("Command line").

context_string(Context) = context_string("", Context).
context_string(SourcePath, context(File0, Line, _)) = Pretty :- 
  (if append(SourcePath, File1, File0) then 
    File = File1 
  else 
    File = File0 
  ),
  ( if Line = 0 then 
    Pretty = File
  else 
    Pretty = format("%s:%d", [s(File), i(Line)])
  ).


:- pred is_newline(char). 
:- mode is_newline(in) is semidet.
:- mode is_newline(out) is semidet.
is_newline(Char) :- to_int(Char, 10).

:- pred is_tab(char).
:- mode is_tab(in) is semidet.
:- mode is_tab(out) is semidet.
is_tab(Char) :- to_int(Char, 9).

after_char(Char, context(File, Line, Col), context(File, NewLine, NewCol)) :- 
  if is_newline(Char) 
    then NewCol = 0, NewLine = Line + 1
    else if is_tab(Char)
      then NewCol = Col + 8 - ((Col - 1) mod 8), Line = NewLine
      else NewCol = Col + 1, Line = NewLine.
  