open Types

module Std = struct
  module Task = Task
  module Async_unix = Async_unix
end

let run main =
  let executor = Executor.create () in
  Effect.Deep.try_with
    main
    ()
    { effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Spawn task ->
            Some
              (fun (k : (a, _) Effect.Deep.continuation) ->
                Executor.async executor task;
                Effect.Deep.continue k ())
          | _ -> None)
    }
;;
