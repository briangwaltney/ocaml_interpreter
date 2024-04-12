type obj =
  | Int of int
  | Bool of bool
  | Null
  | Return of obj
  | Error of string
  | Func of
      { params : Ast.expression list
      ; body : Ast.statement
      ; env : env
      }

and env =
  { store : (string, obj) Hashtbl.t
  ; outer : env option
  }

let rec new_env () = { store = Hashtbl.create 100; outer = None }

and env_with_outer outer =
  let base = new_env () in
  { base with outer = Some outer }

and get env key =
  if Hashtbl.mem env.store key
  then Some (Hashtbl.find env.store key)
  else (
    match env.outer with
    | Some env -> get env key
    | None -> None)

and string_of_obj = function
  | Int num -> string_of_int num
  | Bool boolean ->
    (match boolean with
     | true -> "true"
     | false -> "false")
  | Null -> "null"
  | Return obj -> "Return - " ^ string_of_obj obj
  | Error str -> "Error - " ^ str
  | Func { params; body; env } ->
    let str =
      "Func - Params: "
      ^ string_of_int (List.length params)
      ^ " Body: "
      ^ (body |> Ast.show_statement)
      ^ " Function env: "
      ^ string_of_env env
    in
    str

and print_obj obj = obj |> string_of_obj |> print_endline

and string_of_env env =
  let aux env =
    match env.outer with
    | None -> "END"
    | Some outer ->
      let str_of_store = Hashtbl.fold (fun k _ acc -> acc ^ k ^ " ") env.store "" in
      let str = string_of_env outer in
      str_of_store ^ str
  in
  aux env

and print_env env =
  let print store =
    Hashtbl.iter
      (fun key value ->
        let str = "Key: " ^ key ^ " Value: " ^ string_of_obj value in
        print_endline str)
      store
  in
  let rec aux env =
    match env.outer with
    | None -> print env.store
    | Some outer ->
      print_endline "OUTER";
      let _ = print env.store in
      print_endline "END OUTER";
      aux outer
  in
  aux env
;;
