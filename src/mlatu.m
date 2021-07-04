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

:- module mlatu.

:- interface.

:- import_module io.
:- import_module ast.
:- import_module context.

:- pred main(io::di, io::uo) is det.

:- pred write_success(m_term::in, io::di, io::uo) is det.

:- pred write_incomplete(io::di, io::uo) is det.

:- pred write_expected(context::in, string::in, string::in, io::di, io::uo) is det. 

:- implementation.

:- import_module list.
:- import_module string.

:- import_module parse.
:- import_module codegen.

main(!IO) :-
    command_line_arguments(Args, !IO),
    (if Args = [Arg|_]
    then 
        ParseResult = between(many(space), parse_term, many(space), Arg, command_line_context)
        , ((
            pr_ok(Term, _, _) = ParseResult, write_success(Term, !IO)
            ) ; (
            pr_err(pe_incomplete) = ParseResult, write_incomplete(!IO)
            ) ; (
            pr_err(pe_expected(Context, Expected, Actual)) = ParseResult, write_expected(Context, Expected, Actual, !IO))
        )
    else io.write_string("Expected a command-line argument", !IO)).


write_success(Term, !IO) :- (
    if codegen(Term, Out) 
    then  io.open_output("output.c", Result, !IO),
    ((  Result = ok(Stream), 
        io.write_string(Stream, Out, !IO), 
        io.close_output(Stream, !IO),
        io.call_system("clang output.c -O3 -std=c99 -o output", _, !IO),
        io.write_string("Executable at `./output`\n", !IO)
    ) ; (
        Result = io.error(Error),
        io.error_message(Error, Message),
        io.format("IO Error: %s\n", [s(Message)], !IO)
    ))
    else io.write_string("Code generation failed for an undetermined reason\n", !IO) 
).

write_incomplete(!IO) :- io.write_string("Parsing failed, more input was expected\n", !IO).

write_expected(Context, Expected, Actual, !IO) :- io.format(" At %s, I expected %s but I found \"%s\"\n", [s(context_string(Context)), s(Expected), s(Actual)], !IO).