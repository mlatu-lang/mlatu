open! Batteries

type lit = Num of Z.t | Quote of t list

and t = Lit of lit | Word of string

let rec to_string = function
  | Lit (Num n) -> Z.to_string n
  | Lit (Quote l) ->
      let first = "(" in
      let last = ")" in
      terms_to_string ~first ~last l
  | Word s -> s

and terms_to_string ~first ~last l =
  List.map to_string l
  |> List.interleave ~first ~last " "
  |> List.fold_left ( ^ ) ""

let display = to_string
let display_terms = terms_to_string ~first:"" ~last:""
