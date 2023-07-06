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

let rec parse_let_statement = function
  | ident :: assign :: t ->
    (match assign with
     | Assign ->
       (match ident with
        | Ident name ->
          let exp_statment_opt, t = parse_exp_stmt t in
          (match exp_statment_opt with
           | ExpressionStatement exp -> LetStatement { idt = name; value = exp }, t
           | _ -> failwith "bad let value")
        | tkn -> failwith "identity issue")
     | tkn -> failwith "assign issue")
  | _ -> failwith "not long enough"

and parse_return lst =
  match lst with
  | [] -> ReturnStatement (IntegerLiteral 69), []
  | h :: t -> ReturnStatement (IntegerLiteral 420), t

and expect_peek tkn lst =
  match lst with
  | h :: t when h = tkn -> t
  | _ ->
    let _ = failwith ("bad " ^ string_of_token tkn ^ " statement") in
    lst

and parse_pre_exp = function
  | [] -> failwith "parsing pre with empty token list"
  | h :: t ->
    let prefix, t =
      match h with
      | Ident str -> Identifier str, t
      | Integer str -> IntegerLiteral (int_of_string str), t
      | Bang | Minus ->
        let exp, t = parse_pre_exp t in
        PrefixExpression { op = string_of_token h; right = exp }, t
      | True -> BooleanLiteral true, t
      | False -> BooleanLiteral false, t
      | LeftParen ->
        let exp, t = parse_exp t 1 in
        (match t with
         | h :: t when h = RightParen -> exp, t
         | _ -> failwith "bad grouped expression")
      | If ->
        let t = expect_peek LeftParen t in
        let con, t = parse_exp t 1 in
        let t = expect_peek RightParen t in
        let t = expect_peek LeftBrace t in
        let cons, t = parse_statements true t in
        (* IntegerLiteral 8, t *)
        IfExpression { con; cons; alt = cons }, t
      | tkn ->
        print_token_list t;
        failwith ("bad prefix exp - " ^ (tkn |> string_of_token))
    in
    prefix, t

and parse_infix left lst =
  match lst with
  | [] -> failwith "bad parse infix"
  | op_tkn :: t ->
    let op = string_of_token op_tkn in
    let prec = prec_of_tkn op_tkn in
    let right, t = parse_exp t prec in
    InfixExpression { left; op; right }, t

and parse_exp lst precedence =
  let left, t = parse_pre_exp lst in
  let rec loop t prec left =
    match t with
    | [] -> left, t
    | h :: _ ->
      (match h with
       | Semicolon -> left, t
       | tkn when is_infix tkn = false -> left, t
       | infix when prec_of_tkn infix < precedence -> left, t
       | op ->
         let exp, new_t = parse_infix left t in
         loop new_t (prec_of_tkn op) exp)
  in
  loop t precedence left

and parse_exp_stmt lst =
  let exp, t = parse_exp lst 1 in
  let stmt = ExpressionStatement exp in
  stmt, t

and parse_statements block lexer_lst =
  let rec parse_next lst acc =
    match lst with
    | [] -> acc, []
    | h :: t ->
      (match h with
       | RightBrace when block = true ->
         let statements, t = parse_statements true t in
         parse_next t (statements @ acc)
       | Semicolon -> parse_next t acc
       | Let ->
         let stmt, t = parse_let_statement t in
         parse_next t (stmt :: acc)
       | Return ->
         let stmt, t = parse_return t in
         parse_next t (stmt :: acc)
       | _ ->
         let statement, t = parse_exp_stmt (h :: t) in
         parse_next t (statement :: acc))
  in
  parse_next lexer_lst []
;;

let test_string = "if (x > 5) { let five = 5 };"
let program, _ = test_string |> Lexer.tokens_of_string |> parse_statements false
let () = program |> string_of_stmts |> print_endline
