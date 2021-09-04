open! Batteries

type checker =
  Term.t list Map.String.t -> Term.t list -> (unit, string) Result.t

let var : checker list ref = ref []
let register_checker f = var := f :: !var
let get_checkers () = !var
