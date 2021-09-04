open! Batteries

val line :
     ?encoding:[< Uutf.decoder_encoding]
  -> [`Channel of Stdlib.in_channel | `String of string]
  -> (Term.t list, string) Result.t

val file :
     ?encoding:[< Uutf.decoder_encoding]
  -> string
  -> (Term.t list Map.String.t, string) Result.t
