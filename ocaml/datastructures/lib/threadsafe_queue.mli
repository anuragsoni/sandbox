type 'a t

val create : unit -> 'a t
val pop : 'a t -> 'a
val push : 'a t -> 'a -> unit
