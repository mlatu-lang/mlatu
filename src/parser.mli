open! BatteriesExceptionless

val line :
     ?encoding:[< Uutf.decoder_encoding]
  -> [`Channel of Stdlib.in_channel | `String of string]
  -> (Term.t Vect.t, string) Result.t

val file :
     ?encoding:[< Uutf.decoder_encoding]
  -> string
  -> ((Term.t Vect.t * Term.t Vect.t) list, string) Result.t
