module E = Ocaml_int.Eval
module O = Ocaml_int.Object
module P = Ocaml_int.Parser

let rec repl env =
  let env =
    match env with
    | None -> O.new_env ()
    | Some env -> env
  in
  print_string ">>";
  let input = read_line () in
  let program, env = input |> P.parse_program |> E.eval_program env in
  print_newline ();
  let _ = program |> List.map O.print_obj in
  print_newline ();
  repl (Some env)
;;

let () = repl None
