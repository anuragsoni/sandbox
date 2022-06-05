open Types

type t = task

let spawn fn = Effect.perform (Spawn fn)
