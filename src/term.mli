open! Batteries

type lit = Num of Z.t | Quote of t list

and t = Lit of lit | Word of string

val display : t -> string
val display_terms : t list -> string
