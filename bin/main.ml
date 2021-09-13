open! BatteriesExceptionless

let prompt = "> "
let input_files = ref ["bin/prelude.mlt"]

let load_files =
  let rec loop defs = function
    | [] -> Ok defs
    | fn :: fns -> (
      match Mlatu_api.Parser.file fn with
      | Ok defs' -> loop (defs @ defs') fns
      | Error e -> Error e ) in
  loop []

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
      let rules = Mlatu_api.Rewrite.make_rules defs in
      let rec loop () =
        let () =
          IO.write_string stdout prompt ;
          IO.flush stdout in
        match Mlatu_api.Parser.(line (`Channel Stdlib.stdin)) with
        | Ok main ->
            typecheck
              (fun () ->
                let main' = Mlatu_api.Rewrite.rewrite rules main in
                print_endline ("= " ^ Mlatu_api.Term.display_terms main') ;
                loop () )
              (fun s -> print_endline s ; loop ())
              defs main
        | Error s -> print_endline s ; loop () in
      loop ()
  | Error s -> print_endline s

let () =
  let anon_fun filename = input_files := filename :: !input_files in
  let () =
    Stdlib.Arg.parse [] anon_fun
      "The mlatu programming language by Caden Haustein\n\n\
       Command: mlatu [options]\n\n\
       Options:" in
  main ()
