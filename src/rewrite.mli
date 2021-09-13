open! BatteriesExceptionless

type rules

val make_rules : (Term.t Vect.t * Term.t Vect.t) list -> rules
val rewrite : rules -> Term.t Vect.t -> Term.t Vect.t
