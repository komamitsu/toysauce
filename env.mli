open Syntax

type t
val empty : unit -> t
val append_to_current : t -> symbol -> value -> t
val find : t -> symbol -> value
val create_new : t -> t
val drop_current : t -> t
val string_of_env : t -> string
