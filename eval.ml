open Printf
open Syntax

let rec eval_expr env = function
  | Value v -> (env, v)
  | SetVar (k, e) -> 
      let env, v = eval_expr env e in
      let new_env = Env.append_to_current env k v in
      (new_env, v)
  | GetVar k -> 
      let v = Env.find env k in (env, v)
  | CallFunc (f, params) -> (
      try (
        match Env.find env f with
        | Func (args, expr_list) -> (
          match params with
          | ExprList ps ->
              let env_with_args = 
                List.fold_left2 
                (fun env arg param -> 
                  let new_env, v = 
                    eval_expr env param in
                  Env.append_to_current new_env arg v)
                (Env.create_new env) args ps in
              let env, v = eval_expr env_with_args expr_list in
              let env, v = after_call_func env v in
              (Env.drop_current env, v)
          | _ -> failwith "CallFunc: Not ExprList was given"
        )
        | _ -> raise Not_found
      ) 
      with Not_found ->
        let v = innar_func env params f in 
        after_call_func env v
    )
  | ExprList expr_list ->
      List.fold_left
      (fun (env, _) expr -> eval_expr env expr)
      (env, Null) expr_list
and after_call_func env v =
  match v with
  | Var v -> eval_expr env (GetVar v)
  | _ -> (env, v)
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
  let exc () = 
    eprintf  "Params are %s. \nEnv is %s" 
      (string_of_expr params) (Env.string_of_env env);
    failwith (sprintf "%s: Invalid type." name) in
  match name with
  | "print" | "puts" -> 
      let env, vs = get_values () in
      List.fold_left
        (fun acc v -> (
            match v with
            | Int i -> print_int i
            | String s -> print_string s
            | _ -> print_string (string_of_value v)
          );
          if name = "puts" then print_newline ();
          v
        ) Null vs
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
  | "length" ->
      let env, vs = get_values () in (
        match List.nth vs 0 with
        | Collection vs -> Int (List.length vs)
        | _ -> exc ()
      )
  | "nth" ->
      let env, vs = get_values () in (
        match List.nth vs 0, List.nth vs 1 with
        | Collection vs, Int i -> List.nth vs i
        | _ -> exc ()
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

