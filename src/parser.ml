open! BatteriesExceptionless
module DA = DynArray

type pstate =
  | InWord of Buffer.t
  | InNumber of Buffer.t
  | InQuote of (pstate * Term.t DA.t) * pstate
  | ReadyForNext

let single_buf i c =
  let buf = UTF8.Buf.create i in
  let () = Buffer.add_utf_8_uchar buf c in
  buf

let end_word_line acc buf =
  let s = UTF8.Buf.contents buf in
  DA.add acc
    ( match s with
    | "t" -> Term.Bool true
    | "f" -> Term.Bool false
    | _ -> Term.Word s )

let end_number_line acc buf =
  let s = UTF8.Buf.contents buf in
  DA.add acc Term.(Num (Z.of_string s))

let rec end_state_line acc = function
  | InNumber buf -> end_number_line acc buf ; Ok acc
  | ReadyForNext -> Ok acc
  | InWord buf -> end_word_line acc buf ; Ok acc
  | InQuote (parent, inner) -> end_quote_line acc parent inner

and end_quote_line acc parent inner =
  match end_state_line acc inner with
  | Ok terms ->
      let darr = snd parent in
      DA.add darr (Quote (Vect.of_array (DA.to_array terms))) ;
      Ok darr
  | Error e -> Error e

let rec handle_whitespace_line continue acc state =
  match state with
  | InNumber buf ->
      (* whitespace : end number and continue *)
      end_number_line acc buf ; continue acc ReadyForNext
  | InWord buf ->
      (* whitespace : end word and continue *)
      end_word_line acc buf ; continue acc ReadyForNext
  | InQuote (parent, inner) ->
      (* whitespace : delegate to inner state *)
      handle_whitespace_line
        (fun acc' new_inner -> continue acc' (InQuote (parent, new_inner)))
        acc inner
  | ReadyForNext ->
      (* whitespace : ignore and continue *) continue acc ReadyForNext

let is_digit c =
  let i = Stdlib.Uchar.to_int c in
  i >= 0x0030 && i <= 0x0039

let rec handle_char_line continue acc state c =
  let i = Stdlib.Uchar.to_int c in
  match state with
  | InNumber buf ->
      if i = 0x0028 then (
        (* left parenthesis : end buffer and continue in quote *)
        end_number_line acc buf ;
        continue (DA.create ()) (InQuote ((ReadyForNext, acc), ReadyForNext)) )
      else if i = 0x0029 then Error Errors.unmatched_rparen
      else if is_digit c then (
        (* another digit : add to buffer and continue *)
        Buffer.add_utf_8_uchar buf c ;
        continue acc (InNumber buf) )
      else if
        (i = 0x0062 || i = 0x006F || i = 0x0078) && Buffer.contents buf = "0"
      then
        (* a base prefix : add to buffer and continue *)
        let () = Buffer.add_utf_8_uchar buf c in
        continue acc (InNumber buf)
      else (
        (* a non-digit character : end number and start word *)
        end_number_line acc buf ;
        continue acc (InWord (single_buf 16 c)) )
  | InWord buf ->
      if i = 0x0028 then (
        (* left parenthesis : end buffer and continue in quote *)
        end_word_line acc buf ;
        continue (DA.create ()) (InQuote ((ReadyForNext, acc), ReadyForNext)) )
      else if i = 0x0029 then Error Errors.unmatched_rparen
      else
        (* another character : add to buffer and continue *)
        let () = Buffer.add_utf_8_uchar buf c in
        continue acc (InWord buf)
  | InQuote (((parent_state, parent_acc) as parent), inner) ->
      if i = 0x0028 then
        (* left parenthesis : start new quote*)
        continue (DA.create ()) (InQuote ((state, acc), ReadyForNext))
      else if i = 0x0029 then
        (* right parenthesis : end quote and continue *)
        match end_state_line acc inner with
        | Ok terms ->
            DA.add parent_acc Term.(Quote (Vect.of_array (DA.to_array terms))) ;
            continue parent_acc parent_state
        | Error e -> Error e
      else
        (* delegate to inner state *)
        handle_char_line
          (fun acc' new_inner -> continue acc' (InQuote (parent, new_inner)))
          acc inner c
  | ReadyForNext ->
      if i = 0x0028 then
        (* left parenthesis : end buffer and continue in quote *)
        continue (DA.create ()) (InQuote ((ReadyForNext, acc), ReadyForNext))
      else if i = 0x0029 then Error Errors.unmatched_rparen
      else
        let buf = single_buf 16 c in
        if is_digit c then
          (* a digit : start number *)
          continue acc (InNumber buf)
        else
          (* a non-digit non-sign character : start word *)
          continue acc (InWord buf)

let rec line_loop d acc state =
  match Uutf.decode d with
  | `Uchar c ->
      let i = Stdlib.Uchar.to_int c in
      if i = 0x000A then
        (* normalized newline : delegate ending *)
        end_state_line acc state
      else if i = 0x0009 || i = 0x0020 then
        (* tab or space : delegate handling *)
        handle_whitespace_line (line_loop d) acc state
      else if i = 0x003A || i = 0x003B then
        (* semicolon or colon : *)
        Error Errors.illegal_definition
      else
        (* not whitespace or parenthesis : delegate handling *)
        handle_char_line (line_loop d) acc state c
  | `End -> end_state_line acc state
  | `Malformed _ -> Error Errors.malformed_unicode
  | `Await -> assert false

let line ?encoding (src : [`Channel of Stdlib.in_channel | `String of string]) =
  let nln = `Readline (Uchar.of_int 0x000A) in
  let result =
    line_loop (Uutf.decoder ~nln ?encoding src) (DA.create ()) ReadyForNext
  in
  Result.map (fun da -> Vect.of_array (DA.to_array da)) result

type dstate =
  | InBefore of Term.t DA.t * pstate
  | MaybeInArrow of Term.t DA.t * pstate
  | InAfter of Term.t Vect.t * Term.t DA.t * pstate
  | ReadyForNextDef

let rec handle_whitespace_file continue acc state c =
  match state with
  | InBefore (terms, inner) ->
      handle_whitespace_line
        (fun terms' inner' -> continue acc (InBefore (terms', inner')))
        terms inner
  | MaybeInArrow (terms, inner) ->
      handle_char_line
        (fun terms' inner' ->
          handle_whitespace_file continue acc (InBefore (terms', inner')) c )
        terms inner (Uchar.of_char '-')
  | InAfter (before, terms, inner) ->
      let i = Stdlib.Uchar.to_int c in
      if i = 0x000A then
        match end_state_line terms inner with
        | Ok terms' ->
            continue
              ((before, Vect.of_array (DA.to_array terms')) :: acc)
              ReadyForNextDef
        | Error e -> Error e
      else
        handle_whitespace_line
          (fun terms' inner' -> continue acc (InAfter (before, terms', inner')))
          terms inner
  | ReadyForNextDef -> continue acc ReadyForNextDef

let handle_char_file continue acc state c =
  let i = Stdlib.Uchar.to_int c in
  match state with
  | InBefore (terms, inner) ->
      if i = 0x002D then continue acc (MaybeInArrow (terms, inner))
      else if i = 0x2192 then
        match end_state_line terms inner with
        | Ok terms' ->
            continue acc
              (InAfter
                 (Vect.of_array (DA.to_array terms'), DA.create (), ReadyForNext)
              )
        | Error e -> Error e
      else
        handle_char_line
          (fun terms' inner' -> continue acc (InBefore (terms', inner')))
          terms inner c
  | MaybeInArrow (terms, inner) ->
      if i = 0x003E then
        match end_state_line terms inner with
        | Ok terms' ->
            continue acc
              (InAfter
                 (Vect.of_array (DA.to_array terms'), DA.create (), ReadyForNext)
              )
        | Error e -> Error e
      else
        handle_char_line
          (fun terms' inner' -> continue acc (InBefore (terms', inner')))
          terms inner c
  | InAfter (before, terms, inner) ->
      handle_char_line
        (fun terms' inner' -> continue acc (InAfter (before, terms', inner')))
        terms inner c
  | ReadyForNextDef ->
      handle_char_line
        (fun terms' inner' -> continue acc (InBefore (terms', inner')))
        (DA.create ()) ReadyForNext c

let rec file_loop d acc state =
  match Uutf.decode d with
  | `Uchar c ->
      let i = Stdlib.Uchar.to_int c in
      if i = 0x000A || i = 0x0009 || i = 0x0020 then
        (* whitespace : delegate handling *)
        handle_whitespace_file (file_loop d) acc state c
      else
        (* not whitespace : delegate handling *)
        handle_char_file (file_loop d) acc state c
  | `End -> (
    match state with
    | ReadyForNextDef -> Ok acc
    | InBefore _ | MaybeInArrow _ -> Error Errors.only_lhs
    | InAfter (before, terms, inner) -> (
      match end_state_line terms inner with
      | Ok terms' -> Ok ((before, Vect.of_array (DA.to_array terms')) :: acc)
      | Error e -> Error e ) )
  | `Malformed _ -> Error Errors.malformed_unicode
  | `Await -> assert false

let file ?encoding filename =
  let chan = Stdlib.open_in filename in
  let src = `Channel chan in
  let result =
    file_loop
      (Uutf.decoder ~nln:(`Readline (Uchar.of_int 0x000A)) ?encoding src)
      [] ReadyForNextDef in
  Stdlib.close_in chan ; result
