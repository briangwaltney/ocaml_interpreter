open Token

type expression =
  | Identifier of string
  | IntegerLiteral of int
  | BooleanLiteral of bool
  | PrefixExpression of
      { op : string
      ; right : expression
      }
  | InfixExpression of
      { op : string
      ; left : expression
      ; right : expression
      }
  | IfExpression of
      { con : expression
      ; cons : statement list
      ; alt : statement list option
      }
  | FunctionLiteral of
      { params : expression list option
      ; body : statement
      }
  | CallExpression of
      { func : expression
      ; args : expression list
      }

and statement =
  | LetStatement of
      { idt : string
      ; value : expression
      }
  | ReturnStatement of expression
  | ExpressionStatement of expression
  | BlockStatement of statement list

type node =
  | Statement of statement
  | Expression of expression

type program = statement list

let rec string_of_exp exp =
  match exp with
  | Identifier str -> str
  | IntegerLiteral num -> string_of_int num
  | BooleanLiteral boolean -> string_of_bool boolean
  | PrefixExpression { op; right } -> op ^ string_of_exp right
  | InfixExpression { op; left; right } ->
    "(" ^ string_of_exp left ^ op ^ string_of_exp right ^ ")"
  | IfExpression { con; cons; alt } ->
    "if ("
    ^ string_of_exp con
    ^ ") "
    ^ string_of_stmts cons
    ^ " else "
    ^ string_of_stmts
        (match alt with
         | Some alt -> alt
         | None -> [])
  | FunctionLiteral { params; body } ->
    "fn("
    ^ String.concat
        ", "
        (List.map
           string_of_exp
           (match params with
            | Some lst -> lst
            | None -> []))
    ^ ") "
    ^ string_of_stmt body
  | CallExpression { func; args } ->
    string_of_exp func ^ "(" ^ String.concat ", " (List.map string_of_exp args) ^ ")"

and string_of_stmts stmts =
  "{\n" ^ String.concat "\n" (List.map string_of_stmt stmts) ^ "\n}"

and string_of_stmt stmt =
  match stmt with
  | LetStatement { idt; value } -> "let " ^ idt ^ " = " ^ string_of_exp value ^ ";"
  | ReturnStatement exp -> "return " ^ string_of_exp exp ^ ";"
  | ExpressionStatement exp -> string_of_exp exp ^ ";"
  | BlockStatement stmts ->
    "{\n" ^ String.concat "\n" (List.map string_of_stmt stmts) ^ "\n}"
;;
