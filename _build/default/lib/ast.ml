open! Base

type expression =
  | Identifier of string
  | IntegerLiteral of int
  | BooleanLiteral of bool
  | PrefixExpression of
      { op : Token.t
      ; right : expression
      }
  | InfixExpression of
      { op : Token.t
      ; left : expression
      ; right : expression
      }
  | IfExpression of
      { con : expression
      ; cons : statement
      ; alt : statement option
      }
  | FunctionLiteral of
      { params : expression list option
      ; body : statement
      }
  | CallExpression of
      { func : expression
      ; args : expression list
      }
[@@deriving show, sexp, compare]

and statement =
  | LetStatement of
      { idt : string
      ; value : expression
      }
  | ReturnStatement of expression
  | ExpressionStatement of expression
  | BlockStatement of statement list
[@@deriving show, sexp, compare]

type node =
  | Statement of statement
  | Expression of expression
[@@deriving show, sexp, compare]
