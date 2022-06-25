type t = unit -> unit
type _ Effect.t += Spawn : (unit -> unit) -> unit Effect.t
type _ Effect.t += Yield : unit Effect.t

let yield () = Effect.perform Yield
let spawn fn = Effect.perform (Spawn fn)
