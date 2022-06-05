type task = unit -> unit

type executor =
  { domains : unit Domain.t Array.t
  ; size : int Atomic.t
  ; jobs : Threadsafe_queue.t Array.t
  ; index : int Atomic.t
  }

type _ Effect.t += Spawn : (unit -> unit) -> unit Effect.t
type _ Effect.t += Yield : executor -> unit Effect.t