open! Batteries

let rec mutate defs before after =
  let change_after after' =
    (* after has changed, we should re-check for redexes *)
    let _, list' = mutate defs before after' in
    (true, list') in
  Term.(
    match after with
    (* optimizations, rely on prelude existing *)
    | Lit x :: Word "dup" :: rest -> change_after (Lit x :: Lit x :: rest)
    | Lit _ :: Word "pop" :: rest -> change_after rest
    | Word "id" :: rest -> change_after rest
    | Lit x1 :: Lit x2 :: Word "swap" :: rest ->
        change_after (Lit x2 :: Lit x1 :: rest)
    | Lit (Quote q) :: Word "i" :: rest -> change_after (q @ rest)
    | Lit (Quote x1) :: Lit (Quote x2) :: Word "cat" :: rest ->
        change_after (Lit (Quote (x1 @ x2)) :: rest)
    | Lit x :: Word "unit" :: rest -> change_after (Lit (Quote [Lit x]) :: rest)
    | Lit x1 :: Lit (Quote x2) :: Word "dip" :: rest ->
        change_after (x2 @ (Lit x1 :: rest))
    | Lit (Num n1) :: Word "succ" :: rest ->
        change_after (Lit (Num (Z.succ n1)) :: rest)
    | Lit (Num n1) :: Word "pred" :: rest ->
        change_after (Lit (Num (Z.pred n1)) :: rest)
    | Lit (Num n1) :: Word "zero?" :: rest ->
        change_after (Word (if Z.equal Z.zero n1 then "t" else "f") :: rest)
    (* standard operator definitions *)
    | Lit (Num n1) :: Lit (Num n2) :: Word "+" :: rest ->
        change_after (Lit (Num (Z.add n1 n2)) :: rest)
    | Lit (Num n1) :: Lit (Num n2) :: Word "-" :: rest ->
        change_after (Lit (Num (Z.sub n1 n2)) :: rest)
    | Lit (Num n1) :: Lit (Num n2) :: Word "*" :: rest ->
        change_after (Lit (Num (Z.mul n1 n2)) :: rest)
    | Lit (Num n1) :: Lit (Num n2) :: Word "/" :: rest ->
        change_after (Lit (Num (Z.div n1 n2)) :: rest)
    | Lit (Num n1) :: Lit (Num n2) :: Word "=" :: rest ->
        change_after (Word (if Z.equal n1 n2 then "t" else "f") :: rest)
    | Word "t" :: Lit x :: _ :: Word "?" :: rest -> change_after (Lit x :: rest)
    | Word "f" :: _ :: Lit x :: Word "?" :: rest -> change_after (Lit x :: rest)
    | Lit _ :: Lit (Quote q) :: Word "ky" :: rest -> change_after (q @ rest)
    | Lit x :: Lit (Quote q) :: Word "cy" :: rest ->
        change_after
          (Lit (Quote (Lit x :: q)) :: Lit (Quote (q @ [Lit x])) :: rest)
        (* cy *)
    | Word name :: rest when Map.String.mem name defs ->
        change_after (Map.String.find name defs @ rest)
    | h :: t ->
        let b, list' = mutate defs (before @ [h]) t in
        if b then
          let new_before, new_after = List.split_at (List.length before) list' in
          let _, list'' = mutate defs new_before new_after in
          (true, list'')
        else (false, list')
    | _ -> (false, before @ after))

let rewrite defs main = mutate defs [] main |> snd
