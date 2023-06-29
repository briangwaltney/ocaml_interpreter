let read_file file = In_channel.with_open_text file In_channel.input_all

type t =
  { input : string
  ; position : int
  ; ch : char option
  }

let init input =
  if Base.String.is_empty input
  then { input; position = 0; ch = None }
  else { input; position = 0; ch = Some (String.get input 0) }
;;

let advance lexer =
  if lexer.position >= String.length lexer.input - 1
  then { lexer with ch = None }
  else (
    let position = lexer.position + 1 in
    { lexer with position; ch = Some (String.get lexer.input position) })
;;

let is_letter ch = ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch = '_'

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let read_while lexer con =
  let rec aux lexer con acc =
    match lexer.ch with
    | None -> advance lexer, acc
    | Some ch ->
      (match con ch with
       | true -> aux (advance lexer) con (acc ^ String.make 1 ch)
       | false -> lexer, acc)
  in
  aux lexer con ""
;;

let read_identifier lexer =
  let lexer, ident = read_while lexer is_letter in
  lexer, Token.ident_lookup ident
;;

let read_number lexer =
  let lexer, num = read_while lexer is_digit in
  lexer, Token.Integer num
;;

let skip_whitespace lexer =
  let lexer, _ = read_while lexer is_whitespace in
  lexer
;;

let peak_char lexer =
  match lexer.position + 1 >= String.length lexer.input with
  | true -> None
  | false ->
    let lexer = advance lexer in
    lexer.ch
;;

let if_peaked lexer ch ~default ~matched =
  let lexer, result =
    match peak_char lexer with
    | Some peeked when Char.(peeked = ch) -> advance lexer, matched
    | _ -> lexer, default
  in
  advance lexer, result
;;

let rec next_token lexer =
  let lexer = skip_whitespace lexer in
  let open Token in
  match lexer.ch with
  | None -> lexer, None
  | Some ch ->
    let lexer, token =
      match ch with
      | ';' -> advance lexer, Semicolon
      | '(' -> advance lexer, LeftParen
      | ')' -> advance lexer, RightParen
      | ',' -> advance lexer, Comma
      | '+' -> advance lexer, Plus
      | '{' -> advance lexer, LeftBrace
      | '}' -> advance lexer, RightBrace
      | '-' -> advance lexer, Minus
      | '/' -> advance lexer, Slash
      | '*' -> advance lexer, Asterisk
      | '<' -> advance lexer, LessThan
      | '>' -> advance lexer, GreaterThan
      | '!' -> if_peaked lexer '=' ~default:Bang ~matched:NotEqual
      | '=' -> if_peaked lexer '=' ~default:Assign ~matched:Equal
      | ch when is_letter ch -> read_identifier lexer
      | num when is_digit num -> read_number lexer
      | ch -> failwith ("unkown char: " ^ String.make 1 ch)
    in
    lexer, Some token
;;

let tokens_of_string input =
  let lexer = init input in
  let rec aux lexer acc =
    let new_lexer, token = next_token lexer in
    match token with
    | None -> List.rev acc
    | Some token -> aux new_lexer (token :: acc)
  in
  aux lexer []
;;

let print_lexer lexer =
  let _ = print_endline lexer.input in
  let _ = print_int lexer.position in
  let _ = print_endline "" in
  match lexer.ch with
  | None -> print_endline "No char"
  | Some ch -> print_char ch
;;
