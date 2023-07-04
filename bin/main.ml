open Ocaml_int.Lexer
open Ocaml_int.Token
(* open Ocaml_int.Parser *)

let lexer_test_str =
  "let five = 5;\n\
   let ten = 10;\n\
  \   let add = fn(x, y) {\n\
  \     x + y;\n\
   };\n\
  \   let result = add(five, ten);\n\
  \   !-/*5;\n\
  \   5 < 10 > 5;\n\
  \   if (5 < 10) {\n\
  \       return true;\n\
  \   } else {\n\
  \       return false;}\n\n\n\
   10 == 10; 10 != 9;"
;;

assert (
  tokens_of_string lexer_test_str
  = [ Let
    ; Ident "five"
    ; Assign
    ; Integer "5"
    ; Semicolon
    ; Let
    ; Ident "ten"
    ; Assign
    ; Integer "10"
    ; Semicolon
    ; Let
    ; Ident "add"
    ; Assign
    ; Function
    ; LeftParen
    ; Ident "x"
    ; Comma
    ; Ident "y"
    ; RightParen
    ; LeftBrace
    ; Ident "x"
    ; Plus
    ; Ident "y"
    ; Semicolon
    ; RightBrace
    ; Semicolon
    ; Let
    ; Ident "result"
    ; Assign
    ; Ident "add"
    ; LeftParen
    ; Ident "five"
    ; Comma
    ; Ident "ten"
    ; RightParen
    ; Semicolon
    ; Bang
    ; Minus
    ; Slash
    ; Asterisk
    ; Integer "5"
    ; Semicolon
    ; Integer "5"
    ; LessThan
    ; Integer "10"
    ; GreaterThan
    ; Integer "5"
    ; Semicolon
    ; If
    ; LeftParen
    ; Integer "5"
    ; LessThan
    ; Integer "10"
    ; RightParen
    ; LeftBrace
    ; Return
    ; True
    ; Semicolon
    ; RightBrace
    ; Else
    ; LeftBrace
    ; Return
    ; False
    ; Semicolon
    ; RightBrace
    ; Integer "10"
    ; Equal
    ; Integer "10"
    ; Semicolon
    ; Integer "10"
    ; NotEqual
    ; Integer "9"
    ; Semicolon
    ])
