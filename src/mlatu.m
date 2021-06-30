:- module mlatu.

:- interface.

:- import_module io.

:- import_module context.
:- import_module parse.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.write_string("Hello, world from Mlatu!\n", !IO).
