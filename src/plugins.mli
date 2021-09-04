open! Batteries

type checker =
  Term.t list Map.String.t -> Term.t list -> (unit, string) Result.t

val register_checker : checker -> unit
val get_checkers : unit -> checker list
