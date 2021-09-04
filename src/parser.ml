open! Batteries
open I18n

type pstate =
  | InWord of Buffer.t
  | InMaybeNumber of Buffer.t
  | InNumber of Buffer.t
  | InQuote of (pstate * Term.t list) * pstate
  | ReadyForNext

let single_buf i c =
  let buf = UTF8.Buf.create i in
  let () = Buffer.add_utf_8_uchar buf c in
  buf

let end_word_line acc buf = Ok (Term.Word (UTF8.Buf.contents buf) :: acc)

let end_number_line acc buf =
  let s = UTF8.Buf.contents buf in
  Ok Term.(Lit (Num (Z.of_string s)) :: acc)

let rec end_state_line acc = function
  | InNumber buf -> end_number_line acc buf
  | ReadyForNext -> Ok acc
  | InMaybeNumber buf -> end_word_line acc buf
  | InWord buf -> end_word_line acc buf
  | InQuote (parent, inner) -> end_quote_line acc parent inner

and end_quote_line acc parent inner =
  match end_state_line acc inner with
  | Ok terms -> Ok Term.(Lit (Quote (List.rev terms)) :: snd parent)
  | Error e -> Error e

let rec handle_whitespace_line continue acc state =
  match state with
  | InNumber buf -> (
    (* whitespace : end number and continue *)
    match end_number_line acc buf with
    | Ok acc' -> continue acc' ReadyForNext
    | Error e -> Error e )
  | InMaybeNumber buf -> (
    (* whitespace : end sign as word and continue *)
    match end_word_line acc buf with
    | Ok acc' -> continue acc' ReadyForNext
    | Error e -> Error e )
  | InWord buf -> (
    (* whitespace : end word and continue *)
    match end_word_line acc buf with
    | Ok acc' -> continue acc' ReadyForNext
    | Error e -> Error e )
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
  let rparen = s_ "Unmatched right parenthesis ')'" in
  let i = Stdlib.Uchar.to_int c in
  match state with
  | InNumber buf -> (
      if i = 0x0028 then
        (* left parenthesis : end buffer and continue in quote *)
        match end_number_line acc buf with
        | Ok acc' -> continue [] (InQuote ((ReadyForNext, acc'), ReadyForNext))
        | Error e -> Error e
      else if i = 0x0029 then Error rparen
      else if is_digit c then
        (* another digit : add to buffer and continue *)
        let () = Buffer.add_utf_8_uchar buf c in
        continue acc (InNumber buf)
      else if
        (i = 0x0062 || i = 0x006F || i = 0x0078) && Buffer.contents buf = "0"
      then
        (* a base prefix : add to buffer and continue *)
        let () = Buffer.add_utf_8_uchar buf c in
        continue acc (InNumber buf)
      else
        (* a non-digit character : end number and start word *)
        match end_number_line acc buf with
        | Ok acc -> continue acc (InWord (single_buf 16 c))
        | Error e -> Error e )
  | InMaybeNumber buf ->
      if i = 0x0028 then
        (* left parenthesis : end buffer and continue in quote *)
        match end_word_line acc buf with
        | Ok acc' -> continue [] (InQuote ((ReadyForNext, acc'), ReadyForNext))
        | Error e -> Error e
      else if i = 0x0029 then Error rparen
      else
        let () = Buffer.add_utf_8_uchar buf c in
        if is_digit c then
          (* a digit : add to number buffer and continue *)
          continue acc (InNumber buf)
        else
          (* a non-digit character : add to word buffer and continue *)
          continue acc (InWord buf)
  | InWord buf ->
      if i = 0x0028 then
        (* left parenthesis : end buffer and continue in quote *)
        match end_word_line acc buf with
        | Ok acc' -> continue [] (InQuote ((ReadyForNext, acc'), ReadyForNext))
        | Error e -> Error e
      else if i = 0x0029 then Error rparen
      else
        (* another character : add to buffer and continue *)
        let () = Buffer.add_utf_8_uchar buf c in
        continue acc (InWord buf)
  | InQuote (((parent_state, parent_acc) as parent), inner) ->
      if i = 0x0028 then
        (* left parenthesis : start new quote*)
        continue [] (InQuote ((state, acc), ReadyForNext))
      else if i = 0x0029 then
        (* right parenthesis : end quote and continue *)
        match end_state_line acc inner with
        | Ok terms ->
            continue
              Term.(Lit (Quote (List.rev terms)) :: parent_acc)
              parent_state
        | Error e -> Error e
      else
        (* delegate to inner state *)
        handle_char_line
          (fun acc' new_inner -> continue acc' (InQuote (parent, new_inner)))
          acc inner c
  | ReadyForNext ->
      if i = 0x0028 then
        (* left parenthesis : end buffer and continue in quote *)
        continue [] (InQuote ((ReadyForNext, acc), ReadyForNext))
      else if i = 0x0029 then Error rparen
      else
        let buf = single_buf 16 c in
        if is_digit c then
          (* a digit : start number *)
          continue acc (InNumber buf)
        else if i = 0x002B || i = 0x002D then
          (* a sign character : start what might be a number or a word *)
          continue acc (InMaybeNumber buf)
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
        Error (s_ "unhelpful error message")
      else
        (* not whitespace or parenthesis : delegate handling *)
        handle_char_line (line_loop d) acc state c
  | `End -> end_state_line acc state
  | `Malformed _ -> Error (s_ "Malformed Unicode")
  | `Await -> assert false

let line ?encoding (src : [`Channel of Stdlib.in_channel | `String of string]) =
  let nln = `Readline (Uchar.of_int 0x000A) in
  Result.map List.rev
    (line_loop (Uutf.decoder ~nln ?encoding src) [] ReadyForNext)

type dstate =
  | InName of Buffer.t
  | AfterName of string
  | InDef of string * Term.t list * pstate
  | ReadyForNextDef

let handle_whitespace_file continue acc = function
  | InName buf -> continue acc (AfterName (Buffer.contents buf))
  | AfterName name -> continue acc (AfterName name)
  | InDef (name, terms, inner) ->
      handle_whitespace_line
        (fun terms' inner' -> continue acc (InDef (name, terms', inner')))
        terms inner
  | ReadyForNextDef -> continue acc ReadyForNextDef

let handle_char_file continue acc state c =
  match state with
  | InName buf ->
      Stdlib.Buffer.add_utf_8_uchar buf c ;
      continue acc (InName buf)
  | AfterName name ->
      let i = Stdlib.Uchar.to_int c in
      if i = 0x003A then continue acc (InDef (name, [], ReadyForNext))
      else Error (s_ "I expect a colon ':' after I see a name in a definition.")
  | InDef (name, terms, inner) ->
      let i = Stdlib.Uchar.to_int c in
      if i = 0x003B then
        match end_state_line terms inner with
        | Ok terms' -> continue ((name, List.rev terms') :: acc) ReadyForNextDef
        | Error e -> Error e
      else
        handle_char_line
          (fun terms' inner' -> continue acc (InDef (name, terms', inner')))
          terms inner c
  | ReadyForNextDef -> continue acc (InName (single_buf 16 c))

let rec file_loop d acc state =
  match Uutf.decode d with
  | `Uchar c ->
      let i = Stdlib.Uchar.to_int c in
      if i = 0x000A || i = 0x0009 || i = 0x0020 then
        (* whitespace : delegate handling *)
        handle_whitespace_file (file_loop d) acc state
      else
        (* not whitespace : delegate handling *)
        handle_char_file (file_loop d) acc state c
  | `End -> (
    match state with
    | ReadyForNextDef -> Ok (Map.String.of_seq (Seq.of_list acc))
    | InName _ ->
        Error
          (s_
             "I expect whitespace, colon ':', a body, and a semicolon ';' \
              after I see a name." )
    | AfterName _ ->
        Error
          (s_
             "I expect a colon ':', a body, and a semicolon ';' after I see a \
              name and whitespace." )
    | InDef _ ->
        Error
          (s_
             "I expect a semicolon ';' after I see a name, whitespace, colon \
              ':', and body." ) )
  | `Malformed _ -> Error (s_ "Malformed Unicode")
  | `Await -> assert false

let file ?encoding filename =
  let chan = Stdlib.open_in filename in
  let src = `Channel chan in
  let result =
    file_loop
      (Uutf.decoder ~nln:(`Readline (Uchar.of_int 0x000A)) ?encoding src)
      [] ReadyForNextDef in
  Stdlib.close_in chan ; result
