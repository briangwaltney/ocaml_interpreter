open Ocaml_int.Lexer

let rec repl () =
  print_string "Give me that sweet, sweet Monkey lang \n";
  let input = read_line () in
  let _ = tokens_of_string input |> Ocaml_int.Token.print_token_list in
  repl ()
;;

let () = repl ()
