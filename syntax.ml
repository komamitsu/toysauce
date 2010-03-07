type symbol = string

type expr =
  | Value of value
  | SetVar of symbol * expr
  | GetVar of symbol
  | CallFunc of symbol * expr
  | CallInnarFunc of symbol * expr
  | ExprList of expr list
and value =
  | Symbol of symbol
  | Int of int
  | String of string
  | Bool of bool
  | Func of symbol list * expr
  | Null
