open! BatteriesExceptionless

type checker =
     (Term.t Vect.t * Term.t Vect.t) list
  -> Term.t Vect.t
  -> (unit, string) Result.t

let var : checker list ref = ref []
let register_checker f = var := f :: !var
let get_checkers () = !var
