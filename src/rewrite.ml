open! BatteriesExceptionless

type matcher = (Term.t, rules) Hashtbl.t * (Term.t -> rules option) option

and rules = {mutable value: Term.t Vect.t option; matcher: matcher}

let make_table () = Hashtbl.create 2
let no_match () : matcher = (make_table (), None)

let apply_match ((table, func) : matcher) key =
  match Option.bind func (fun f -> f key)  with
  | Some v -> Some v
  | None -> Hashtbl.find table key

let add_match ((table, _) : matcher) key value = Hashtbl.add table key value

let primitives () : rules =
  { value= None
  ; matcher=
      ( make_table ()
      , Some
          (function
          | Term.Quote q1 ->
              Some
                { value= None
                ; matcher=
                    ( make_table ()
                    , Some
                        (function
                        | Quote q2 ->
                            Some
                              { value= None
                              ; matcher=
                                  ( make_table ()
                                  , Some
                                      (function
                                      | Word "cons" ->
                                          Some
                                            { value=
                                                Some
                                                  (Vect.singleton
                                                     (Term.Quote
                                                        (Vect.prepend
                                                           (Term.Quote q1) q2 )
                                                     ) )
                                            ; matcher= no_match () }
                                      | Word "dip" ->
                                          Some
                                            { value=
                                                Some
                                                  (Vect.append (Term.Quote q1)
                                                     q2 )
                                            ; matcher= no_match () }
                                      | _ -> None ) ) }
                        | Word "i" -> Some {value= Some q1; matcher= no_match ()}
                        | Word "dup" ->
                            Some
                              { value=
                                  Some
                                    (Vect.of_list
                                       [Term.Quote q1; Term.Quote q1] )
                              ; matcher= no_match () }
                        | Word "pop" ->
                            Some {value= Some Vect.empty; matcher= no_match ()}
                        | _ -> None ) ) }
          | Term.Bool b ->
              Some
                { value= None
                ; matcher=
                    ( make_table ()
                    , Some
                        (function
                        | Quote q2 ->
                            Some
                              { value= None
                              ; matcher=
                                  ( make_table ()
                                  , Some
                                      (function
                                      | Word "cons" ->
                                          Some
                                            { value=
                                                Some
                                                  (Vect.singleton
                                                     (Term.Quote
                                                        (Vect.prepend
                                                           (Term.Bool b) q2 ) ) )
                                            ; matcher= no_match () }
                                      | Word "dip" ->
                                          Some
                                            { value=
                                                Some
                                                  (Vect.append (Term.Bool b) q2)
                                            ; matcher= no_match () }
                                      | _ -> None ) ) }
                        | Word "dup" ->
                            Some
                              { value=
                                  Some (Vect.of_list [Term.Bool b; Term.Bool b])
                              ; matcher= no_match () }
                        | Word "pop" ->
                            Some {value= Some Vect.empty; matcher= no_match ()}
                        | _ -> None ) ) }
          | Term.Num n1 ->
              Some
                { value= None
                ; matcher=
                    ( make_table ()
                    , Some
                        (function
                        | Quote q2 ->
                            Some
                              { value= None
                              ; matcher=
                                  ( make_table ()
                                  , Some
                                      (function
                                      | Word "cons" ->
                                          Some
                                            { value=
                                                Some
                                                  (Vect.singleton
                                                     (Term.Quote
                                                        (Vect.prepend
                                                           (Term.Num n1) q2 ) ) )
                                            ; matcher= no_match () }
                                      | Word "dip" ->
                                          Some
                                            { value=
                                                Some
                                                  (Vect.append (Term.Num n1) q2)
                                            ; matcher= no_match () }
                                      | _ -> None ) ) }
                        | Word "dup" ->
                            Some
                              { value=
                                  Some (Vect.of_list [Term.Num n1; Term.Num n1])
                              ; matcher= no_match () }
                        | Word "pop" ->
                            Some {value= Some Vect.empty; matcher= no_match ()}
                        | Term.Num n2 ->
                            Some
                              { value= None
                              ; matcher=
                                  ( make_table ()
                                  , Some
                                      (function
                                      | Word "+" ->
                                          Some
                                            { value=
                                                Some
                                                  (Vect.singleton
                                                     (Term.Num (Z.add n1 n2)) )
                                            ; matcher= no_match () }
                                      | Word "-" ->
                                          Some
                                            { value=
                                                Some
                                                  (Vect.singleton
                                                     (Term.Num (Z.sub n1 n2)) )
                                            ; matcher= no_match () }
                                      | Word "*" ->
                                          Some
                                            { value=
                                                Some
                                                  (Vect.singleton
                                                     (Term.Num (Z.mul n1 n2)) )
                                            ; matcher= no_match () }
                                      | Word "/" ->
                                          Some
                                            { value=
                                                Some
                                                  (Vect.singleton
                                                     (Term.Num (Z.div n1 n2)) )
                                            ; matcher= no_match () }
                                      | Word "=" ->
                                          Some
                                            { value=
                                                Some
                                                  (Vect.singleton
                                                     (Term.Bool (Z.equal n1 n2)) )
                                            ; matcher= no_match () }
                                      | _ -> None ) ) }
                        | _ -> None ) ) }
          | _ -> None ) ) }

let make_rules defs : rules =
  let rules = primitives () in
  List.iter
    (fun (lhs, rhs) ->
      let rec recur t ks =
        match ks with
        | [] -> t.value <- Some rhs
        | k :: ks' -> (
          match Hashtbl.find (fst t.matcher) k with
          | Some trie -> recur trie ks'
          | None ->
              add_match t.matcher k
                (List.fold_right
                   (fun k trie ->
                     let m = no_match () in
                     add_match m k trie ; {value= None; matcher= m} )
                   ks'
                   {value= Some rhs; matcher= no_match ()} ) ) in
      recur rules (Vect.to_list lhs) )
    (List.rev defs) ;
  rules

let rewrite (rules : rules) main =
  let rec mutate n list =
    let rec search_recur t acc i =
      match
        try Some (Vect.get list (n + i)) with Vect.Out_of_bounds -> None
      with
      | None -> (
        match (t.value, acc) with
        | Some f, _ -> Some (f, i)
        | None, Some (f, depth) -> Some (f, depth)
        | None, None -> None )
      | Some k -> (
        match (apply_match t.matcher k, t.value) with
        | Some trie, Some f -> search_recur trie (Some (f, i)) (i + 1)
        | Some trie, None -> search_recur trie acc (i + 1)
        | None, Some v -> Some (v, i)
        | None, None -> None ) in
    match search_recur rules None 0 with
    | Some (xs, depth) ->
        let list' = Vect.insert n xs (Vect.remove n depth list) in
        (true, snd (mutate n list'))
    | None ->
        if n + 1 < Vect.length list then
          let b, list' = mutate (n + 1) list in
          if b then
            (true, snd (mutate n list'))
          else (false, list')
        else (false, list) in
  snd (mutate 0 main)
