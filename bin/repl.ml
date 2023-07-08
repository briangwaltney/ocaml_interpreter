open Ocaml_int.Eval
open Ocaml_int.Object
open Ocaml_int.Parser

let rec repl env =
  let env =
    match env with
    | None -> new_env ()
    | Some env -> env
  in
  print_string ">>";
  let input = read_line () in
  let program, env = input |> parse_program |> eval_program env in
  print_newline ();
  let _ = program |> List.map print_obj in
  print_newline ();
  repl (Some env)
;;

let () = repl None
