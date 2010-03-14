open Printf

type var = string

type expr =
  | Value of value
  | SetVar of var * expr
  | GetVar of var
  | CallFunc of var * expr
  | ExprList of expr list
and value =
  | Var of var
  | Int of int
  | String of string
  | Bool of bool
  | Func of var list * expr
  | Collection of value list
  | Null

let rec string_of_value = function
  | Var v -> sprintf "(var: %s)" v
  | Int i -> sprintf "(int: %d)" i
  | String s -> sprintf "(string: %s)" s
  | Bool b -> "(bool: " ^ (if b then "true" else "false") ^ ")"
  | Func (ss, e) -> 
      sprintf "(func: {args: %s} {expr: %s})"
        (String.concat ", " ss) (string_of_expr e)
  | Collection vs ->
      sprintf "(collection: %s)" 
        (List.fold_left
          (fun acc v -> acc ^ (string_of_value v)) "" vs)
  | Null -> "(null)"
and string_of_expr = function
  | Value v -> sprintf "[value: %s]" (string_of_value v)
  | SetVar (s, e) ->  
      sprintf "[setvar: {var: %s}; {expr: %s}]" s (string_of_expr e)
  | GetVar s -> sprintf "[getvar: {var: %s}]" s
  | CallFunc (s, e) ->
      sprintf "[callfunc: {var: %s}; {expr: %s}]" s (string_of_expr e)
  | ExprList es -> 
      sprintf "[exprlist: %s]" 
        (String.concat ", " 
          (List.map (fun e -> string_of_expr e) es))

