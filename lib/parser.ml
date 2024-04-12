open! Base
open Ast
open Token

let rec prec_of_tkn = function
  | Equal | NotEqual -> 2
  | LessThan | GreaterThan -> 3
  | Plus | Minus -> 4
  | Asterisk | Slash -> 5
  | LeftParen -> 6
  | LeftBrace -> 7
  | _ -> 1

and is_infix = function
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

and expect_peek tkn lst =
  let open Base.Poly in
  match lst with
  | h :: t when h = tkn -> t
  | _ -> failwith ("bad " ^ (tkn |> Token.show) ^ " statement")

and parse_pre_exp li =
  let open Base.Poly in
  match li with
  | h :: t ->
    let prefix, t =
      match h with
      | Ident str -> Identifier str, t
      | Integer num -> IntegerLiteral num, t
      | Bang | Minus ->
        let exp, t = parse_pre_exp t in
        PrefixExpression { op = h; right = exp }, t
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
        let t = expect_peek RightParen t |> expect_peek LeftBrace in
        let cons, t = parse_statements true t in
        let if_expression, t =
          match t with
          | h :: t when h = Else ->
            let t = expect_peek LeftBrace t in
            let alt, t = parse_statements true t in
            ( IfExpression
                { con; cons = BlockStatement cons; alt = Some (BlockStatement alt) }
            , t )
          | _ -> IfExpression { con; cons = BlockStatement cons; alt = None }, t
        in
        if_expression, t
      | Function ->
        let t = expect_peek LeftParen t in
        let rec parse_params lst acc =
          match lst with
          | [] -> failwith "Bad param statement"
          | h :: t ->
            (match h with
             | RightParen -> acc, t
             | Ident str -> parse_params t (Identifier str :: acc)
             | Comma -> parse_params t acc
             | _ -> failwith "Bad params")
        in
        let params, t = parse_params t [] in
        let body, t = parse_statements true t in
        FunctionLiteral { body = BlockStatement body; params = Some params }, t
      | tkn ->
        tkn :: t |> List.iter ~f:(fun tkn -> tkn |> Token.show |> Stdlib.print_endline);
        failwith ("bad prefix exp - " ^ (tkn |> Token.show))
    in
    prefix, t
  | _ -> failwith "parsing pre with empty token list"

and parse_infix left lst =
  let open Base.Poly in
  match lst with
  | [] -> failwith "bad parse infix"
  | op_tkn :: t when op_tkn = LeftParen ->
    let rec gather_args lst acc =
      match lst with
      | h :: t when h = RightParen -> List.rev acc, t
      | h :: t when h = Comma ->
        let new_arg, t = parse_exp t 1 in
        gather_args t (new_arg :: acc)
      | _ ->
        let new_arg, t = parse_exp t 1 in
        gather_args t (new_arg :: acc)
    in
    let args, t = gather_args t [] in
    CallExpression { func = left; args }, t
  | op :: t ->
    let prec = prec_of_tkn op in
    let right, t = parse_exp t prec in
    InfixExpression { left; op; right }, t

and parse_exp lst precedence =
  let open Base.Poly in
  let left, t = parse_pre_exp lst in
  let rec loop t _ left =
    match t with
    | [] -> left, t
    | h :: _ ->
      (match h with
       | Semicolon -> left, t
       | RightBrace -> left, t
       | Comma -> left, t
       | tkn when is_infix tkn = false -> left, t
       | infix when prec_of_tkn infix <= precedence -> left, t
       | op ->
         let exp, new_t = parse_infix left t in
         loop new_t (prec_of_tkn op) exp)
  in
  loop t precedence left

and parse_exp_stmt lst =
  let exp, t = parse_exp lst 1 in
  let stmt = ExpressionStatement exp in
  stmt, t

and parse_statements is_block lexer_lst =
  let open Base.Poly in
  let rec parse_next lst acc =
    match lst with
    | [] -> acc, []
    | h :: t ->
      (match h with
       | RightBrace when is_block = true -> acc, t
       | LeftBrace ->
         let stmts, t = parse_next t acc in
         BlockStatement (List.rev stmts) :: acc, t
       | Semicolon -> parse_next t acc
       | Let ->
         (match t with
          | h :: t ->
            (match h with
             | Ident idt ->
               let t = expect_peek Assign t in
               let exp, t = parse_exp t 1 in
               parse_next t (LetStatement { idt; value = exp } :: acc)
             | _ -> failwith "bad name")
          | _ -> failwith "bad let statement")
       | Return ->
         let exp, t = parse_exp t 1 in
         parse_next t (ReturnStatement exp :: acc)
       | _ ->
         let statement, t = parse_exp_stmt (h :: t) in
         parse_next t (statement :: acc))
  in
  let program, remaining = parse_next lexer_lst [] in
  List.rev program, remaining

and parse_program str =
  let statement_lst, _ = parse_statements false (str |> Lexer.tokens_of_string) in
  statement_lst
;;
