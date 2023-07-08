open Ast
open Parser
open Object

let rec eval node acc env =
  match node with
  | IntegerLiteral num -> Int num :: acc, env
  | BooleanLiteral boolean -> Bool boolean :: acc, env
  | PrefixExpression { right; op } ->
    (match op with
     | "!" ->
       let right, env = eval right acc env in
       let right, acc = right |> first in
       (match right with
        | Bool true -> Bool false :: acc, env
        | Bool false -> Bool true :: acc, env
        | Int _ -> Bool false :: acc, env
        | Null -> Bool true :: acc, env
        | _ -> Error "bad ! prefix exp" :: acc, env)
     | "-" ->
       let right, env = eval right acc env in
       let right, acc = right |> first in
       (match right with
        | Int num -> Int (-num) :: acc, env
        | _ -> Error "bad - prefix exp" :: acc, env)
     | _ -> acc, env)
  | InfixExpression { left; op; right } ->
    let left, env = eval left acc env in
    let left, acc = left |> first in
    let right, env = eval right acc env in
    let right, acc = right |> first in
    (match left, right with
     | Int left, Int right ->
       (match op with
        | "+" -> Int (left + right) :: acc, env
        | "-" -> Int (left - right) :: acc, env
        | "*" -> Int (left * right) :: acc, env
        | "/" -> Int (left / right) :: acc, env
        | "<" -> Bool (left < right) :: acc, env
        | ">" -> Bool (left > right) :: acc, env
        | "=" -> Bool (left = right) :: acc, env
        | "!=" -> Bool (left <> right) :: acc, env
        | str -> Error ("Bad Int infix op " ^ str) :: acc, env)
     | Bool left, Bool right ->
       (match op with
        | "=" -> Bool (left = right) :: acc, env
        | "!=" -> Bool (left <> right) :: acc, env
        | str -> Error ("Bad boolean infix op " ^ str) :: acc, env)
     | _ -> Error "Left and Right of infix are not equal types" :: acc, env)
  | IfExpression { con; cons; alt } ->
    let con, env = eval con acc env in
    let con, acc = con |> first in
    (match con with
     | Bool true ->
       let obj_lst, env = eval_stmt cons [] env in
       obj_lst @ acc, env
     | Bool false ->
       (match alt with
        | None -> Null :: acc, env
        | Some alt ->
          let obj_lst, env = eval_stmt alt [] (new_env ()) in
          obj_lst @ acc, env)
     | _ -> Null :: acc, env)
  | Identifier str ->
    (match get env str with
     | None -> Error "Unable to find variable assignment" :: acc, env
     | Some obj -> obj :: acc, env)
  | FunctionLiteral { params; body } ->
    let params =
      match params with
      | Some params -> params
      | None -> []
    in
    Func { params; body } :: acc, env
  | CallExpression { func; args } ->
    let func, env = eval func [] env in
    let func, _ = first func in
    let params, body =
      match func with
      | Func { body; params } -> params, body
      | _ -> failwith "incorrect function call"
    in
    let rec eval_args args env acc =
      match args with
      | [] -> acc, env
      | h :: t ->
        let res, env = eval h [] env in
        eval_args t env (res @ acc)
    in
    let args, fn_env = eval_args args (new_env ()) [] in
    let apply_params_to_env fn_env params args =
      let _ =
        List.mapi
          (fun i idt_exp ->
            let value = List.nth args i in
            match idt_exp with
            | Identifier str ->
              (match Hashtbl.mem fn_env.store str with
               | true -> Hashtbl.replace fn_env.store str value
               | false -> Hashtbl.add fn_env.store str value)
            | _ -> failwith "Bad function params")
          params
      in
      fn_env
    in
    let fn_env = apply_params_to_env fn_env params args in
    let res, fn_env = eval_stmt body [] fn_env in
    res @ acc, env

and first = function
  | [ h ] -> h, []
  | h :: t -> h, t
  | _ -> failwith "no obj to get"

and eval_stmt stmt acc env =
  match stmt with
  | ExpressionStatement exp ->
    let obj_lst, env = eval exp acc env in
    obj_lst, env
  | ReturnStatement exp ->
    (match acc with
     | [] ->
       let obj_lst, env = eval exp acc env in
       obj_lst, env
     | h :: _ ->
       (match h with
        | Return obj -> [ obj ], env
        | Error _ -> [ h ], env
        | _ ->
          let obj_lst, env = eval exp acc env in
          obj_lst, env))
  | BlockStatement stmt_lst ->
    let obj_lst, env = eval_stmt_lst stmt_lst env in
    obj_lst @ acc, env
  | LetStatement { idt; value } ->
    let value, env = eval value [] env in
    let value, rest = value |> first in
    (match Hashtbl.mem env.store idt with
     | true -> Hashtbl.replace env.store idt value
     | false -> Hashtbl.add env.store idt value);
    acc, env

and eval_stmt_lst stmt_lst env =
  let rec aux stmt_lst acc env =
    match stmt_lst with
    | [] -> acc, env
    | h :: t ->
      (match h with
       | ReturnStatement _ ->
         let rtn_obj, env = eval_stmt h acc env in
         let rtn_obj, acc = rtn_obj |> first in
         Return rtn_obj :: acc, env
       | _ ->
         let acc, env = eval_stmt h acc env in
         aux t acc env)
  in
  aux stmt_lst [] env

and eval_program env program =
  let obj_lst, env = eval_stmt_lst program env in
  List.rev obj_lst, env
;;

let test = "let add = fn(x, y) { x + y }"

let blah =
  "\n\
   let newAdder = fn(x) {\n\
  \  fn(y) { x + y };\n\
   };\n\n\
   let addTwo = newAdder(2);\n\
   addTwo(2);\n"
;;
