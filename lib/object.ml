open Ast

type obj =
  | Int of int
  | Bool of bool
  | Null
  | Return of obj
  | Error of string
  | Func of
      { params : Ast.expression list
      ; body : Ast.statement
      }

and env = { store : (string, obj) Hashtbl.t }

let rec new_env () = { store = Hashtbl.create 100 }

and get env key =
  match Hashtbl.mem env.store key with
  | true -> Some (Hashtbl.find env.store key)
  | false -> None

and string_of_obj = function
  | Int num -> string_of_int num
  | Bool boolean ->
    (match boolean with
     | true -> "true"
     | false -> "false")
  | Null -> "null"
  | Return obj -> "Return - " ^ string_of_obj obj
  | Error str -> "Error - " ^ str
  | Func { params; body } ->
    let str =
      "Func - Params: "
      ^ string_of_int (List.length params)
      ^ " Body: "
      ^ Ast.string_of_stmt body
    in
    str

and print_obj obj = obj |> string_of_obj |> print_endline

and string_of_env env =
  Hashtbl.fold (fun k v acc -> acc ^ k ^ " " ^ string_of_obj v) env.store ""

and print_env env = env |> string_of_env |> print_endline
