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

:- type m_build_type ---> mbt_check ; mbt_build ; mbt_run.

:- implementation.

:- import_module bool.
:- import_module list.
:- import_module string.

:- import_module ast.
:- import_module codegen.
:- import_module context.
:- import_module infer.
:- import_module parse.
:- import_module int.

main(!IO) :-
    command_line_arguments(Args, !IO),
    (if Args = [Command|[Input|_]], ((Command = "check", BuildType = mbt_check) ; (Command = "build", BuildType = mbt_build) ; (Command = "run", BuildType = mbt_run))
    then 
        ParseResult = parse_term(Input, command_line_context)
        , ((
                pr_ok(Term, _, _) = ParseResult, 
                go(Term, BuildType, !IO)
            ) ; (
                pr_err(pe_incomplete) = ParseResult, 
                io.stdout_stream(Stdout, !IO),
                io.write_string(Stdout, "Parsing failed, more input was expected\n", !IO)
            ) ; (
                pr_err(pe_expected(Context, Expected, Actual)) = ParseResult, 
                io.stdout_stream(Stdout, !IO),
                io.format(Stdout, " At %s, I expected %s but I found \"%s\"\n", [s(context_string(Context)), s(Expected), s(Actual)], !IO))
        )
    else 
        io.stdout_stream(Stdout, !IO),
        io.write_string(Stdout, "Usage: `mlatu <cmd> <program>` where <cmd> is `check`, `build`, or `run`", !IO)).

:- pred exit_success(io::di, io::uo) is det.
exit_success(!IO) :- io.set_exit_status(0, !IO).

:- pred exit_failure(io::di, io::uo) is det. 
exit_failure(!IO) :- io.set_exit_status(1, !IO).

:- pred go(term::in, m_build_type::in, io::di, io::uo) is det.
go(Term, BuildType, !IO) :- (
    BuildType = mbt_check, 
    infer_main(Term, Result),
    ((
        Result = ok(_), 
        io.stdout_stream(Stdout, !IO),
        io.write_string(Stdout, "Everything checks out\n", !IO), 
        exit_success(!IO)
    ) ; (
        Result = err(Err), 
        infer_failure(Term, Err, !IO)
    ))
) ; (
    BuildType = mbt_build, 
    infer_main(Term, Result),
    ((  
        Result = ok(SpecTerm),
        build(SpecTerm, BuildResult, !IO), 
        ((
            BuildResult = yes,
            io.stdout_stream(Stdout, !IO),
            io.write_string(Stdout, "Executable was successfully produced (./output)\n", !IO), exit_success(!IO)
        ) ; (BuildResult = no, exit_failure(!IO)))
    ) ; (
        Result = err(Err), 
        infer_failure(Term, Err, !IO)
    ))
) ; (
    BuildType = mbt_run, 
    infer_main(Term, Result),
    ((
        Result = ok(SpecTerm),
        build(SpecTerm, BuildResult, !IO),
        ((
            BuildResult = yes,
            io.call_system("./output", Exit, !IO), 
            (if Exit = ok(0) 
            then io.remove_file("./output", _, !IO), exit_success(!IO) 
            else exit_failure(!IO))
        ) ; (BuildResult = no, exit_failure(!IO)))
    ) ; (
        Result = err(Err), 
        infer_failure(Term, Err, !IO)
    ))
).
  

:- pred infer_failure(term::in, infer_err::in, io::di, io::uo) is det.
infer_failure(Term, Err, !IO) :- 
    term_string(Term, TermString),
    io.stdout_stream(Stdout, !IO),
    io.format(Stdout, "There was an error while inferring the spec of `%s`.\n", [s(TermString)], !IO),
    ((
        resolve(Name, Context) = Err,
        io.format(Stdout, "%s I'm not sure what name `%s` refers to. Did you misspell it?\n", [s(context_string(Context)), s(Name)], !IO)
    ) ; (
        unify(Expected, Actual) = Err, 
        spec_string(Expected, ExpectedString),
        spec_string(Actual, ActualString),
        io.format(Stdout, "I expected it to have the spec `%s` but it had the spec %s instead.\n", [s(ExpectedString), s(ActualString)], !IO),
        (if Expected = m_spec(X, E), Actual = m_spec(X, A), length(E) + 1 = length(A) 
        then io.write_string(Stdout, "Perhaps you forgot to `drop` or `.` the result?\n", !IO)
        else if Expected = m_spec(E, X), Actual = m_spec(A, X), length(E) + 1 = length(A)
        then io.write_string(Stdout, "Perhaps you forgot to include your data in the beginning?\n", !IO)
        else io.write_string(Stdout, "The inferred spec differed significantly from the expected spec.\n", !IO)
        )
    )),
    exit_failure(!IO).

:- pred build(spec_term::in, bool::out, io::di, io::uo) is det.
build(Term, Success, !IO) :- 
    io.open_output("output.c", Result, !IO), 
    ((
        Result = ok(Stream),
        (if codegen(Term, Out)
        then io.write_string(Stream, Out, !IO), 
            io.close_output(Stream, !IO),
            io.call_system("clang output.c -O3 -std=c99 -o output", Exit, !IO), 
            (if Exit = ok(0) 
            then
                io.remove_file("output.c", Res, !IO),
               ((
                   Res = io.ok, 
                   Success = yes
                ) ; (
                    Res = io.error(_), 
                    io.write_string(io.stdout_stream, "Could not delete `output.c`", !IO),
                    Success = no
                ))
            else 
                io.write_string(io.stdout_stream, "clang did not exit succssfully", !IO),
                Success = no)
        else 
            io.write_string(io.stdout_stream, "Error during code generation", !IO),
            Success = no)
    ) ; (
        Result = io.error(_), 
        io.write_string(io.stdout_stream, "Could not open `output.c`", !IO),
        Success = no
    )).
