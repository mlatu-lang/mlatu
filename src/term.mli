open! BatteriesExceptionless

type t = Num of Z.t | Quote of t Vect.t | Word of string | Bool of bool

val display : t -> string
val display_terms : t Vect.t -> string
val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
