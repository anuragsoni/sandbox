type task = unit -> unit

type _ Effect.t += Spawn : (unit -> unit) -> unit Effect.t
type _ Effect.t += Yield : unit Effect.t
type _ Effect.t += Wait_read : Unix.file_descr -> unit Effect.t
type _ Effect.t += Wait_write : Unix.file_descr -> unit Effect.t