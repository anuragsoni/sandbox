type t

val create : unit -> t
val pop : t -> Optional_thunk.t
val try_push : t -> Optional_thunk.t -> bool
val try_pop : t -> Optional_thunk.t
val push : t -> Optional_thunk.t -> unit
