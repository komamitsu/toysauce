open Syntax

type t = (symbol, value) Hashtbl.t list

let empty_env_unit () = Hashtbl.create 10

let empty_env () = [empty_env_unit ()]

let append_to_current_env env k v = 
  match env with
  | [] -> failwith "append_to_current_env: No current_env"
  | hd :: tl -> 
      Hashtbl.replace hd k v;
      hd :: tl

let rec find_from_env env k = 
  match env with
  | [] -> raise Not_found
  | hd :: tl -> 
      try Hashtbl.find hd k with 
      | Not_found -> find_from_env tl k

let create_next_env env =
  (empty_env_unit ()) :: env

let drop_current_env env =
  match env with
  | [] -> failwith "drop_current_env: No current_env"
  | hd :: tl -> tl
