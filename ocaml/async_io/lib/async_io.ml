module Std = struct
  module Task = Task
end

type t = { executor : Executor.t }

let create () = { executor = Executor.create () }

let main fn =
  let t = create () in
  Effect.Deep.try_with
    fn
    ()
    { effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Types.Spawn fn ->
            Some
              (fun (k : (a, _) Effect.Deep.continuation) ->
                Executor.async t.executor fn;
                Effect.Deep.continue k ())
          | _ -> None)
    }
;;
