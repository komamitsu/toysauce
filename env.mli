open Syntax

type t
val empty_env : unit -> t
val append_to_current_env : t -> symbol -> value -> t
val find_from_env : t -> symbol -> value
val create_next_env : t -> t
val drop_current_env : t -> t
