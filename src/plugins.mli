open! BatteriesExceptionless

type checker =
     (Term.t Vect.t * Term.t Vect.t) list
  -> Term.t Vect.t
  -> (unit, string) Result.t

val register_checker : checker -> unit
val get_checkers : unit -> checker list
