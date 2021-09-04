open! Batteries
open I18n

let prompt = "> "
let input_files = ref ["bin/prelude.mlt"]

let load_files =
  let decide name def1 _def2 =
    print_endline ("Warning: '" ^ name ^ "' is defined twice") ;
    Some def1 in
  let rec loop defs = function
    | [] -> Ok defs
    | fn :: fns -> (
      match Mlatu_api.Parser.file fn with
      | Ok defs' -> loop (Map.String.union decide defs defs') fns
      | Error e -> Error e ) in
  loop Map.String.empty

let checkers =
  let () = Sites.Plugins.Plugins.load_all () in
  Mlatu_api.Plugins.get_checkers ()

let typecheck if_ok if_error defs terms =
  let rec loop = function
    | [] -> if_ok ()
    | c :: cs -> (
      match c defs terms with Ok () -> loop cs | Error e -> if_error e ) in
  loop checkers

let main () =
  print_endline "Loading files..." ;
  match load_files !input_files with
  | Ok defs ->
      let rec loop () =
        let () =
          IO.write_string stdout prompt ;
          IO.flush stdout in
        match Mlatu_api.Parser.(line (`Channel Stdlib.stdin)) with
        | Ok main ->
            typecheck
              (fun () ->
                let rewritten = Mlatu_api.Rewrite.rewrite defs main in
                print_endline ("= " ^ Mlatu_api.Term.display_terms rewritten) ;
                loop () )
              (fun s -> print_endline s ; loop ())
              defs main
        | Error s -> print_endline s ; loop () in
      loop ()
  | Error s -> print_endline s

let () =
  let anon_fun filename = input_files := filename :: !input_files in
  let spf x = Printf.sprintf x in
  let gettext_args, gettext_copyright = I18n.init in
  let args = Stdlib.Arg.align gettext_args in
  let () =
    Stdlib.Arg.parse args anon_fun
      (spf
         (f_
            "The mlatu programming language by Caden Haustein\n\n\
             %s\n\n\
             Command: mlatu [options]\n\n\
             Options:" )
         gettext_copyright ) in
  main ()
