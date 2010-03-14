open Syntax
open Printf

type t = (var, value) Hashtbl.t list

let empty_unit () = Hashtbl.create 10

let empty () = [empty_unit ()]

let append_to_current env k v = 
  match env with
  | [] -> failwith "append_to_current_env: No current_env"
  | hd :: tl -> 
      Hashtbl.replace hd k v;
      hd :: tl

let rec find env k = 
  match env with
  | [] -> raise Not_found
  | hd :: tl -> 
      try Hashtbl.find hd k with 
      | Not_found -> find tl k

let create_new env =
  empty_unit () :: env

let drop_current env =
  match env with
  | [] -> failwith "drop_current_env: No current_env"
  | hd :: tl -> tl

let string_of_env env =
  let string_of_env_unit eu =
    Hashtbl.fold
      (fun k v acc -> acc ^ sprintf "(%s: %s)" k (string_of_value v))
      eu "" in
  List.fold_left
    (fun acc eu -> acc ^ sprintf "[%s]\n" (string_of_env_unit eu))
    "" env
