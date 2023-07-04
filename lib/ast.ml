open Token

type expression =
  | Identifier of string
  | IntegerLiteral of int
  | PrefixExpression of
      { op : string
      ; right : expression
      }
  | InfixExpression of
      { op : string
      ; left : expression
      ; right : expression
      }

type statement =
  | LetStatement of
      { idt : string
      ; value : expression
      }
  | ReturnStatement of expression
  | ExpressionStatement of expression

type node =
  | Statement of statement
  | Expression of expression

type program = statement list

let rec string_of_exp exp =
  match exp with
  | Identifier str -> str
  | IntegerLiteral num -> string_of_int num
  | PrefixExpression { op; right } -> op ^ string_of_exp right
  | InfixExpression { op; left; right } ->
    "(" ^ string_of_exp left ^ op ^ string_of_exp right ^ ")"
;;

let rec string_of_stmt stmt =
  match stmt with
  | LetStatement { idt; value } -> "let " ^ idt ^ " = " ^ string_of_exp value ^ ";"
  | ReturnStatement exp -> "return " ^ string_of_exp exp ^ ";"
  | ExpressionStatement exp -> string_of_exp exp ^ ";"
;;
