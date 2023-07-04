open Ast
open Token

let prec_of_tkn = function
  | Equal | NotEqual -> 2
  | LessThan | GreaterThan -> 3
  | Plus | Minus -> 4
  | Asterisk | Slash -> 5
  | LeftParen -> 6
  | LeftBrace -> 7
  | _ -> 1
;;

let is_infix = function
  | Equal
  | NotEqual
  | LessThan
  | GreaterThan
  | Plus
  | Minus
  | Asterisk
  | Slash
  | LeftParen
  | LeftBrace -> true
  | _ -> false
;;

(* let rec parse_let_statement = function *)
(*   | ident :: assign :: t -> *)
(*     (match assign with *)
(*      | Assign -> *)
(*        (match ident with *)
(*         | Ident name -> *)
(*           let exp_opt, t = parse_exp_stmt t in *)
(*           (match exp_opt with *)
(*            | None -> failwith "bad let value" *)
(*            | Some exp -> LetStatement { idt = name; value = exp }, t) *)
(*         | tkn -> failwith "identity issue") *)
(*      | tkn -> failwith "assign issue") *)
(*   | _ -> failwith "not long enough" *)
(* and parse_return lst = *)
(*   match lst with *)
(*   | [] -> ReturnStatement (IntegerLiteral 69), [] *)
(*   | h :: t -> ReturnStatement (IntegerLiteral 420), t *)

let rec parse_pre_exp = function
  | [] -> failwith "can't get here with an empty array"
  | h :: t ->
    let prefix, t =
      match h with
      | Ident str -> Identifier str, t
      | Integer str -> IntegerLiteral (int_of_string str), t
      | Bang | Minus ->
        let exp, t = parse_pre_exp t in
        PrefixExpression { op = string_of_token h; right = exp }, t
      | tkn ->
        tkn |> string_of_token |> print_endline;
        failwith "bad prefix exp"
    in
    prefix, t

and parse_exp left lst =
  match lst with
  | [] -> left, lst
  | h :: t ->
    (match h with
     | Semicolon -> left, t
     | tkn when is_infix tkn ->
       let right, t = parse_pre_exp t in
       let op = string_of_token tkn in
       let infix = InfixExpression { op; left; right } in
       parse_exp infix t
     | _ -> failwith "end of parse exp")

and parse_exp_stmt lst =
  let prefix, t = parse_pre_exp lst in
  let exp, t = parse_exp prefix t in
  let stmt = ExpressionStatement exp in
  Some stmt, t
;;

let parse_program lexer_lst =
  let rec parse_next lst acc =
    match lst with
    | [] -> acc
    | h :: t ->
      (match h with
       | Semicolon -> parse_next t acc
       (* | Let -> *)
       (*   let stmt, t = parse_let_statement t in *)
       (*   parse_next t (stmt :: acc) *)
       (* | Return -> *)
       (*   let stmt, t = parse_return t in *)
       (*   parse_next t (stmt :: acc) *)
       | _ ->
         let statment_opt, t = parse_exp_stmt (h :: t) in
         (match statment_opt with
          | None -> parse_next t acc
          | Some stmt -> parse_next t (stmt :: acc)))
  in
  parse_next lexer_lst [] |> List.rev
;;

let test_string = "-1 + 2 + 3 * 4;"
let program = test_string |> Lexer.tokens_of_string |> parse_program
let () = program |> List.map string_of_stmt |> String.concat "\n" |> print_endline
