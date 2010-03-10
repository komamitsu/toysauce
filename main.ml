open Printf
open Env
open Syntax

let rec eval_expr env = function
  | Value v -> (env, v)
  | SetVar (k, e) -> 
      let env, v = eval_expr env e in
      let new_env = append_to_current_env env k v in
      (new_env, v)
  | GetVar k -> 
      let v = find_from_env env k in (env, v)
  | CallFunc (f, params) -> (
      try (
        match find_from_env env f with
        | Func (args, expr_list) -> (
          match params with
          | ExprList ps ->
              let env_with_args = 
                List.fold_left2 
                (fun env arg param -> 
                  let new_env, v = eval_expr env param in
                  append_to_current_env new_env arg v) 
                (create_next_env env) args ps in
              eval_expr env_with_args expr_list
          | _ -> failwith "CallFunc: Not ExprList was given"
        )
        | _ -> raise Not_found
      ) 
      with Not_found ->
        let v = innar_func env params f in (env, v)
    )
  | CallInnarFunc (f, params) -> 
      let v = innar_func env params f in (env, v)
  | ExprList expr_list ->
      List.fold_left
      (fun (env, _) expr -> eval_expr env expr)
      (env, Null) expr_list
and innar_func env params name =
  let get_values () =
    match params with
    | ExprList ps ->
        let env, vs = 
          List.fold_left 
          (fun (env, vs) expr ->
            let new_env, v = eval_expr env expr in
            (new_env, v::vs)) (env, []) ps in
        (env, List.rev vs)
    | _ -> failwith "CallFunc: Not ExprList was given" in
  let exc () = failwith (sprintf "%s: Invalid type. Params are %s" name 
                          (string_of_expr params)) in
  match name with
  | "print" -> 
      let env, vs = get_values () in
      let v = List.nth vs 0 in (
        match v with
        | Symbol s -> printf "(symbol : %s)" s
        | Int i -> print_int i
        | String s -> print_string s
        | Bool b -> print_string (if b then "(true)" else "(false)")
        | Func _ -> print_string "(function)"
        | Null -> print_string "(null)"
      ); v
  | "concat" -> 
      let env, vs = get_values () in
      String (
        List.fold_left 
          (fun acc v ->
            match v with
            | String s -> acc ^ s
            | _ -> exc ()
          ) "" vs
      )
  | ">" -> 
      let env, vs = get_values () in (
        match (List.nth vs 0), (List.nth vs 1) with
        | Int a, Int b -> Bool (a > b)
        | String a, String b -> Bool (String.compare a b > 0)
        | _ -> exc ()
      )
  | ">=" -> 
      let env, vs = get_values () in (
        match (List.nth vs 0), (List.nth vs 1) with
        | Int a, Int b -> Bool (a >= b)
        | String a, String b -> Bool (String.compare a b >= 0)
        | _ -> exc ()
      )
  | "<" ->
      let env, vs = get_values () in (
        match (List.nth vs 0), (List.nth vs 1) with
        | Int a, Int b -> Bool (a < b)
        | String a, String b -> Bool (String.compare a b < 0)
        | _ -> exc ()
      )
  | "<=" ->
      let env, vs = get_values () in (
        match (List.nth vs 0), (List.nth vs 1) with
        | Int a, Int b -> Bool (a <= b)
        | String a, String b -> Bool (String.compare a b <= 0)
        | _ -> exc ()
      )
  | "==" ->
      let env, vs = get_values () in (
        match (List.nth vs 0), (List.nth vs 1) with
        | Int a, Int b -> Bool (a = b)
        | String a, String b -> Bool (String.compare a b = 0)
        | _ -> exc ()
      )
  | "!=" ->
      let env, vs = get_values () in (
        match (List.nth vs 0), (List.nth vs 1) with
        | Int a, Int b -> Bool (a != b)
        | String a, String b -> Bool (String.compare a b != 0)
        | _ -> exc ()
      )
  | "if" -> (
        match params with
        | ExprList (expr_cond::expr_true::expr_false::[]) ->
            let env, v = eval_expr env expr_cond in (
              match v with
              | Bool b ->
                  let env, v =
                    eval_expr env 
                      (if b then expr_true else expr_false) in
                  v
              | _ -> exc ()
            )
        | _ -> exc ()
      )
  | "+" | "-" | "*" | "/" ->
      let env, vs = get_values () in (
        let f = 
          match name with 
          | "+" -> (+) | "-" -> (-) | "*" -> ( * ) | "/" -> (/) 
          | _ -> failwith (sprintf "%s: Unknown Error" name) in
        match (List.nth vs 0), (List.nth vs 1) with
        | Int a, Int b -> Int (f a b)
        | _ -> exc ()
      )
  | _ -> 
      failwith (sprintf "innar_func: %s was not found" name)

let _ =
  Printexc.record_backtrace true;
  try 
    let lexbuf = Lexing.from_channel stdin in
    let rec loop parsed =
      let result = Parser.main Lexer.token lexbuf in
      result::parsed in
    let expr_list = loop [] in
    print_endline (string_of_expr (ExprList expr_list)); flush stdout;
    ignore (eval_expr (empty_env ()) (ExprList expr_list))
  with e -> 
    Printf.eprintf "Unexpected exception : %s\n" (Printexc.to_string e);
    Printexc.print_backtrace stderr

