type t =
  | Illegal
  | Ident of string
  | Integer of string
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | LessThan
  | GreaterThan
  | Equal
  | NotEqual
  | Comma
  | Semicolon
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Function
  | Let
  | True
  | False
  | If
  | Else
  | Return

let ident_lookup str =
  match str with
  | "fn" -> Function
  | "let" -> Let
  | "true" -> True
  | "false" -> False
  | "if" -> If
  | "else" -> Else
  | "return" -> Return
  | str -> Ident str
;;

let string_of_token = function
  | Illegal -> "Illegal"
  | Ident a -> "Identity " ^ a
  | Integer a -> "Integer " ^ a
  | Assign -> "Assign"
  | Plus -> "+"
  | Minus -> "-"
  | Bang -> "!"
  | Asterisk -> "*"
  | Slash -> "/"
  | LessThan -> "<"
  | GreaterThan -> ">"
  | Equal -> "="
  | NotEqual -> "!="
  | Comma -> ","
  | Semicolon -> ";"
  | LeftParen -> "("
  | RightParen -> ")"
  | LeftBrace -> "{"
  | RightBrace -> "}"
  | Function -> "Function"
  | Let -> "Let"
  | True -> "True"
  | False -> "False"
  | If -> "If"
  | Else -> "Else"
  | Return -> "Return"
;;

let rec print_token_list = function
  | [] -> ()
  | h :: t ->
    let () = string_of_token h |> print_endline in
    print_token_list t
;;
