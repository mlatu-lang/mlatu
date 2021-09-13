open! BatteriesExceptionless

type t = Num of Z.t | Quote of t Vect.t | Word of string | Bool of bool

let rec to_string = function
  | Num n -> Z.to_string n
  | Bool true -> "t"
  | Bool false -> "f"
  | Quote l ->
      let first = "(" in
      let last = ")" in
      terms_to_string ~first ~last l
  | Word s -> s

and terms_to_string ~first ~last l =
  Vect.to_list l |> List.map to_string
  |> List.interleave ~first ~last " "
  |> List.fold_left ( ^ ) ""

let display = to_string
let display_terms = terms_to_string ~first:"" ~last:""

let rec equal x y =
  match (x, y) with
  | Num a, Num b -> Z.equal a b
  | Quote a, Quote b -> Vect.equal equal a b
  | Word a, Word b -> String.equal a b
  | _, _ -> false

let hash = Stdlib.Hashtbl.hash
let compare = Stdlib.compare
