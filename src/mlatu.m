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

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module time.
:- import_module list.
:- import_module string.
:- import_module int.

:- import_module ast.
:- import_module codegen.
:- import_module context.
:- import_module infer.
:- import_module parse.

main(!IO) :-
    command_line_arguments(Args, !IO),
    handle_args(Args, !IO).

:- pred handle_args(list(string)::in, io::di, io::uo) is det.
handle_args([Command|[Input|_]], !IO) :- 
    (if Command = "check" 
    then 
        handle_parsing(handle_inference(
            pred(_::in, In::di, Out::uo) is det :- check_success(In, Out)
        ), Input, !IO)
    else if Command = "build" 
    then 
        handle_parsing(handle_inference(handle_building(build_success)), Input, !IO)
    else if Command = "run" 
    then 
        handle_parsing(handle_inference(handle_building(run)), Input, !IO)
    else if Command = "bench" 
    then 
        handle_parsing(handle_inference(handle_building(bench)), Input, !IO)
    else io.write_string(io.stdout_stream, "Unknown command.\n", !IO)).
handle_args([_|[]], !IO) :- 
    io.write_string(io.stdout_stream, "Expected a subcommand and then the input\n", !IO).
handle_args([], !IO) :- 
    io.write_string(io.stdout_stream, "Expected a subcommand and then the input\n", !IO).

:- pred check_success(io::di, io::uo) is det. 
check_success(!IO) :-
    io.write_string(io.stdout_stream, "Everything checks out\n", !IO), 
    io.set_exit_status(0, !IO).

:- pred build_success(io::di, io::uo) is det. 
build_success(!IO) :-
    io.write_string(io.stdout_stream, "Produced an executable named `./output`\n", !IO), 
    io.set_exit_status(0, !IO).

:- pred handle_parsing(pred(term, io, io), string, io, io).
:- mode handle_parsing(pred(in, di, uo) is det, in, di, uo) is det.
handle_parsing(After, Input, !IO) :- 
    ParseResult = parse_term(Input, command_line_context),
    ((
        pr_ok(Term, _, _) = ParseResult, 
        After(Term, !IO)
    ) ; (
        pr_err(pe_incomplete) = ParseResult, 
        io.write_string(io.stdout_stream, "Parsing failed, more input was expected\n", !IO),
        io.set_exit_status(1, !IO)
    ) ; (
        pr_err(pe_expected(Context, Expected, Actual)) = ParseResult, 
        io.format(io.stdout_stream, " At %s, I expected %s but I found \"%s\"\n", [s(context_string(Context)), s(Expected), s(Actual)], !IO),
        io.set_exit_status(1, !IO)
    )).

:- pred handle_inference(pred(spec_term, io, io), term, io, io).
:- mode handle_inference(pred(in, di, uo) is det, in, di, uo) is det.
handle_inference(After, Term, !IO) :- 
    infer_main(Term, Result),
    ((
        Result = ok(SpecTerm), 
        After(SpecTerm, !IO)
    ) ; (
        Result = err(Err), 
        infer_failure(Term, Err, !IO)
    )).

:- pred infer_failure(term::in, infer_err::in, io::di, io::uo) is det.
infer_failure(Term, Err, !IO) :- 
    io.stdout_stream(Stdout, !IO),
    io.format(Stdout, "There was an error while inferring the spec of `%s`.\n", [s(term_string(Term))], !IO),
    ((
        resolve(Name, Context) = Err,
        io.format(Stdout, "%s I'm not sure what name `%s` refers to. Did you misspell it?\n", [s(context_string(Context)), s(Name)], !IO)
    ) ; (
        unify(Expected, Actual) = Err, 
        io.format(Stdout, "I expected it to have the spec `%s` but it had the spec %s instead.\n", [s(spec_string(Expected)), s(spec_string(Actual))], !IO),
        (if Expected = m_spec(X, E), Actual = m_spec(X, A), length(E) + 1 = length(A) 
        then io.write_string(Stdout, "Perhaps you forgot to `drop` or `.` the result?\n", !IO)
        else if Expected = m_spec(E, X), Actual = m_spec(A, X), length(E) + 1 = length(A)
        then io.write_string(Stdout, "Perhaps you forgot to include your data in the beginning?\n", !IO)
        else io.write_string(Stdout, "The inferred spec differed significantly from the expected spec.\n", !IO)
        )
    )),
    io.set_exit_status(1, !IO).

:- pred handle_building(pred(io, io), spec_term, io, io).
:- mode handle_building(pred(di, uo) is det, in, di, uo) is det.
handle_building(After, Term, !IO) :-
    io.open_output("output.c", Result, !IO), 
    ((
        Result = ok(Stream),
        codegen(Term, Out),
        io.write_string(Stream, Out, !IO), 
        io.close_output(Stream, !IO),
        io.call_system("clang output.c -O3 -std=c99 -o output", Exit, !IO),
        (if Exit = ok(0) 
        then
            io.remove_file("output.c", Res, !IO),
           ((
               Res = io.ok, 
               After(!IO)
            ) ; (
                Res = io.error(_), 
                io.write_string(io.stdout_stream, "Could not delete `output.c`", !IO),
                io.set_exit_status(1, !IO)
            ))
        else 
            io.write_string(io.stdout_stream, "clang did not exit succssfully", !IO),
            io.set_exit_status(1, !IO))
    ) ; (
        Result = io.error(_), 
        io.write_string(io.stdout_stream, "Could not open `output.c`", !IO),
        io.set_exit_status(1, !IO)
    )).

:- pred run(io::di, io::uo) is det. 
run(!IO) :- 
    io.call_system("./output", Exit, !IO),
    (if Exit = ok(0) 
    then io.set_exit_status(0, !IO) 
    else io.set_exit_status(1, !IO)).

:- pred bench(io::di, io::uo) is det. 
bench(!IO) :- 
    time.clock(Time1, !IO),
    io.call_system("./output", Exit, !IO),
    time.clock(Time2, !IO),
    (if Exit = ok(0) 
    then 
        io.format(io.stdout_stream, "Time taken: %i clock ticks\n(There are %i clock ticks per second)\n", [i(Time2 - Time1), i(time.clocks_per_sec)], !IO),
        io.set_exit_status(0, !IO) 
    else io.set_exit_status(1, !IO)).