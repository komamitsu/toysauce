open Printf

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

let rec string_of_value = function
  | Symbol s -> sprintf "(symbol: %s)" s
  | Int i -> sprintf "(int: %d)" i
  | String s -> sprintf "(string: %s)" s
  | Bool b -> "(bool: " ^ (if b then "true" else "false") ^ ")"
  | Func (ss, e) -> 
      sprintf "(func: args: %s; expr: %s)"
        (String.concat ", " ss) (string_of_expr e)
  | Null -> "(null)"
and string_of_expr = function
  | Value v -> sprintf "[value: %s]" (string_of_value v)
  | SetVar (s, e) ->  
      sprintf "[setvar: symbol: %s; expr: %s]" s (string_of_expr e)
  | GetVar s -> sprintf "[getvar: symbol: %s]" s
  | CallFunc (s, e) ->
      sprintf "[callfunc: symbol: %s; expr: %s]" s (string_of_expr e)
  | CallInnarFunc (s, e) ->
      sprintf "[callifunc: symbol: %s; expr: %s]" s (string_of_expr e)
  | ExprList es -> 
      sprintf "[exprlist: %s]" 
        (String.concat ", " 
          (List.map (fun e -> string_of_expr e) es))

