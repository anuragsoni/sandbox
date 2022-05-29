type 'a t

val create : unit -> 'a t
val pop : 'a t -> 'a
val try_push : 'a t -> 'a -> bool
val try_pop : 'a t -> 'a option
val push : 'a t -> 'a -> unit
