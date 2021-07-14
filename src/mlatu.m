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

:- import_module bool.
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
        handle_input(handle_parsing(handle_inference(
            pred(_::in, In::di, Out::uo) is det :- check_success(In, Out)
        )), Input, !IO)
    else if Command = "build" 
    then 
        handle_input(handle_parsing(handle_inference(handle_building(build_success))), Input, !IO)
    else if Command = "run" 
    then 
        handle_input(handle_parsing(handle_inference(handle_building(run))), Input, !IO)
    else if Command = "bench" 
    then 
        handle_input(handle_parsing(handle_inference(handle_building(bench))), Input, !IO)
    else io.write_string(io.stderr_stream, "Unknown subcommand. Known subcommands are `check`, `build`, `run`, and `bench`.\n", !IO)).
handle_args([_|[]], !IO) :- 
    io.write_string(io.stderr_stream, "Expected `check <input>`, `build <input>`, `run <input>`, or `bench <input>`, where input is Mlatu source or a filename ending in .mlt\n", !IO).
handle_args([], !IO) :- 
    io.write_string(io.stderr_stream, "Expected `check <input>`, `build <input>`, `run <input>`, or `bench <input>`, where input is Mlatu source or a filename ending in .mlt\n", !IO).

:- pred check_success(io::di, io::uo) is det. 
check_success(!IO) :-
    io.write_string(io.stdout_stream, "Everything checks out\n", !IO), 
    io.set_exit_status(0, !IO).

:- pred build_success(io::di, io::uo) is det. 
build_success(!IO) :-
    io.write_string(io.stdout_stream, "Produced an executable named `./output`\n", !IO), 
    io.set_exit_status(0, !IO).

:- pred handle_input(pred(string, context, io, io), string, io, io).
:- mode handle_input(pred(in, in, di, uo) is det, in, di, uo) is det.
handle_input(After, Input, !IO) :- 
    if string.remove_suffix(Input, ".mlt", _) 
    then 
        io.open_input(Input, Result, !IO),
        (( 
            Result = io.ok(Stream),
            io.read_file(Stream, OutResult, !IO), 
            ((OutResult = io.ok(CharList)) ; (OutResult = io.error(CharList, _))),
            io.close_input(Stream, !IO),
            After(string.from_char_list(CharList), context(Input), !IO)
        ) ; (
            Result = io.error(E),
            io.write_string(io.stderr_stream, io.error_message(E), !IO)
        ))
    else After(Input, command_line_context, !IO).

:- pred handle_parsing(pred(terms, io, io), string, context, io, io).
:- mode handle_parsing(pred(in, di, uo) is det, in, in, di, uo) is det.
handle_parsing(After, Input, Context, !IO) :- 
    parse_terms(Input, Context, ParseResult),
    ((
        pr_ok(Terms, _, _) = ParseResult, 
        After(Terms, !IO)
    ) ; (
        pr_err(pe_incomplete) = ParseResult, 
        io.write_string(io.stderr_stream, "Parsing failed, more input was expected\n", !IO),
        io.set_exit_status(1, !IO)
    ) ; (
        pr_err(pe_expected(C, Expected, Actual)) = ParseResult, 
        io.format(io.stderr_stream, "At %s, I expected %s but I found \"%s\"\n", [s(context_string(C)), s(Expected), s(Actual)], !IO),
        io.set_exit_status(1, !IO)
    )).

:- pred handle_inference(pred(spec_terms, io, io), terms, io, io).
:- mode handle_inference(pred(in, di, uo) is det, in, di, uo) is det.
handle_inference(After, Terms, !IO) :- 
    infer_main(Terms, Result),
    ((
        Result = ok(SpecTerms), 
        After(SpecTerms, !IO)
    ) ; (
        Result = err(Err), 
        infer_failure(Terms, Err, !IO)
    )).

:- pred infer_failure(terms::in, infer_err::in, io::di, io::uo) is det.
infer_failure(Terms, Err, !IO) :- 
    Stderr = io.stderr_stream,
    io.format(Stderr, "There was an error while inferring the spec of `%s`.\n", [s(terms_string(Terms))], !IO),
    ((
        resolve(Name, Context) = Err,
        io.format(Stderr, "%s I'm not sure what name `%s` refers to. Did you misspell it?\n", [s(context_string(Context)), s(Name)], !IO)
    ) ; (
        unify(Expected, Actual) = Err, 
        io.format(Stderr, "I expected it to have the spec `%s` but it had the spec %s instead.\n", [s(spec_string(Expected)), s(spec_string(Actual))], !IO),
        (if Expected = m_spec(X, E), Actual = m_spec(X, A), length(E) + 1 = length(A) 
        then io.write_string(Stderr, "Perhaps you forgot to `drop` or `.` the result?\n", !IO)
        else if Expected = m_spec(E, X), Actual = m_spec(A, X), length(E) + 1 = length(A)
        then io.write_string(Stderr, "Perhaps you forgot to include your data in the beginning?\n", !IO)
        else io.write_string(Stderr, "The inferred spec differed significantly from the expected spec.\n", !IO)
        )
    )),
    io.set_exit_status(1, !IO).

:- pred handle_building(pred(io, io), spec_terms, io, io).
:- mode handle_building(pred(di, uo) is det, in, di, uo) is det.
handle_building(After, Terms, !IO) :-
    Filename = "output.d",
    io.remove_file(Filename, _, !IO),
    io.open_output(Filename, Result, !IO), 
        ((
            Result = ok(Stream),
            codegen(Terms, yes, Out),
            io.write_string(Stream, Out, !IO), 
            io.close_output(Stream, !IO),
            handle_call((pred(I::di, O::uo) is det :- 
                io.remove_file(Filename, _, I, M), After(M, O)
            ), "dmd -of=output " ++ Filename, "dmd", !IO)
        ) ; (
            Result = io.error(E), 
            io.write_string(io.stderr_stream, io.error_message(E), !IO),
            io.set_exit_status(1, !IO)
    )).

:- pred run(io::di, io::uo) is det. 
run(!IO) :- 
    handle_call((pred(In::di, Out::uo) is det :- 
        io.remove_file("./output", _, In, Mid), 
        io.set_exit_status(0, Mid, Out)
    ), "./output", "The Mlatu program", !IO).

:- pred handle_call(pred(io, io), string, string, io, io).
:- mode handle_call(pred(di, uo) is det, in, in, di, uo) is det.
handle_call(After, Command, ShortName, !IO) :- 
    io.call_system_return_signal(Command, Exit, !IO),
    Stderr = io.stderr_stream,
    ((
        Exit = io.ok(exited(ExitCode)),
        (if ExitCode = 0 
        then After(!IO)
        else 
            io.write_string(Stderr, ShortName, !IO),
            io.format(Stderr, " exited unsuccessfully with exit code %i\n", [i(ExitCode)], !IO),
            io.set_exit_status(1, !IO))
    ) ; (
        Exit = io.ok(signalled(SignalCode)),
        io.write_string(Stderr, ShortName, !IO),
        io.format(Stderr, " was killed by signal %i\n", [i(SignalCode)], !IO),
        io.set_exit_status(1, !IO)
    ) ; (
        Exit = io.error(E),
        io.write_string(Stderr, io.error_message(E), !IO),
        io.set_exit_status(1, !IO)
    )).

:- pred bench(io::di, io::uo) is det. 
bench(!IO) :- 
    handle_call((pred(In::di, Out::uo) is det :- 
        io.remove_file("./output", _, In, Mid),
        io.set_exit_status(0, Mid, Out)
    ), "time ./output", "The Mlatu program", !IO).
