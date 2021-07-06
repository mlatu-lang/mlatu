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

main(!IO) :-
    command_line_arguments(Args, !IO),
    (if Args = [Command|[Input|_]], ((Command = "check", BuildType = mbt_check) ; (Command = "build", BuildType = mbt_build) ; (Command = "run", BuildType = mbt_run))
    then 
        ParseResult = between(many(space), parse_term, many(space), " " ++ Input ++ " ", command_line_context)
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
    infer_term(Term, SpecTerm),
    term_spec(SpecTerm, Spec),
    (if Spec = m_spec(0u, 0u)
    then
        io.stdout_stream(Stdout, !IO),
        io.write_string(Stdout, "Everything checks out\n", !IO), 
        exit_success(!IO)
    else 
        check_failure(Term, Spec, !IO))
) ; (
    BuildType = mbt_build, 
    infer_term(Term, SpecTerm),
    term_spec(SpecTerm, Spec),
    (if Spec = m_spec(0u, 0u)
    then 
        build(SpecTerm, Result, !IO), 
        ((
            Result = yes,
            io.stdout_stream(Stdout, !IO),
            io.write_string(Stdout, "Executable was successfully produced (./output)\n", !IO), exit_success(!IO)
        ) ; ( 
            Result = no,
            build_failure(!IO)
        ))
    else 
        check_failure(Term, Spec, !IO))
) ; (
    BuildType = mbt_run, 
    infer_term(Term, SpecTerm),
    term_spec(SpecTerm, Spec),
    (if Spec = m_spec(0u, 0u)
    then 
        build(SpecTerm, Result, !IO),
        ((Result = yes,
        io.call_system("./output", Exit, !IO), 
            (if Exit = ok(0) 
            then io.remove_file("./output", _, !IO), exit_success(!IO) 
            else exit_failure(!IO))
        ) ; ( 
        Result = no, 
        build_failure(!IO)))
    else 
        check_failure(Term, Spec, !IO))
).
  

:- pred check_failure(term::in, m_spec::in, io::di, io::uo) is det.
check_failure(Term, Spec, !IO) :- 
    term_string(Term, TermString),
    io.stdout_stream(Stdout, !IO),
    io.format(Stdout, "There was an error while inferring the spec of `%s`.\n", [s(TermString)], !IO),
    ((
        Spec = m_spec(_, _), 
        spec_string(Spec, SpecString),
        io.format(Stdout, "I expected it to have spec `(->)` but instead it had spec `%s`.\n", [s(SpecString)], !IO) 
    ) ; (
        Spec = ms_err(Name),
        io.format(Stdout, "I'm not sure what name `%s` refers to. Did you misspell it?\n", [s(Name)], !IO)
    )),
    exit_failure(!IO).

:- pred build(spec_term::in, bool::out, io::di, io::uo) is det.
build(Term, Success, !IO) :- 
    io.open_output("output.c", Result, !IO), 
    ((Result = ok(Stream),
    (if codegen(Term, Out)
        then io.write_string(Stream, Out, !IO), 
            io.close_output(Stream, !IO),
            io.call_system("clang output.c -O3 -std=c99 -o output", Exit, !IO), 
            (if Exit = ok(0) 
            then 
                io.remove_file("output.c", Res, !IO),
                ((Res = io.ok, Success = yes) ; (Res = io.error(_), Success = no))
            else Success = no)
        else Success = no)
    ) ; (Result = io.error(_), Success = no)).

:- pred build_failure(io::di, io::uo) is det.
build_failure(!IO) :- 
    io.stdout_stream(Stdout, !IO),
    io.write_string(Stdout, "There was an error during code generation. Either a name couldn't be resolved or there was an IO error.\n", !IO), 
    exit_failure(!IO).