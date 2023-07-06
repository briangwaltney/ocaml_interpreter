open Ocaml_int.Parser

let rec repl () =
  print_string ">>";
  let input = read_line () in
  let _ = input |> parse_program |> Ocaml_int.Ast.string_of_stmts |> print_endline in
  repl ()
;;

let () = repl ()
