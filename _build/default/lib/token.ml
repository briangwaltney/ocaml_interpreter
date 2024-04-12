open! Base

type t =
  | Illegal
  | Ident of string
  | Integer of int
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
[@@deriving show, compare, sexp]

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
