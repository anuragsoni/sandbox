open Async_io

module Promise = struct
  type 'a state =
    | Pending
    | Resolved of 'a

  type 'a t =
    { mutable state : 'a state
    ; mutex : Mutex.t
    ; resolved : Condition.t
    }

  let create () =
    { state = Pending; mutex = Mutex.create (); resolved = Condition.create () }
  ;;

  let resolve t data =
    Mutex.lock t.mutex;
    match t.state with
    | Pending ->
      t.state <- Resolved data;
      Mutex.unlock t.mutex;
      Condition.broadcast t.resolved
    | Resolved _ ->
      Mutex.unlock t.mutex;
      failwith "Attempting to fill a non empty promise"
  ;;

  let await t =
    match t.state with
    | Resolved v -> v
    | Pending ->
      Mutex.lock t.mutex;
      (match t.state with
       | Resolved v ->
         Mutex.unlock t.mutex;
         v
       | Pending ->
         Condition.wait t.resolved t.mutex;
         (match t.state with
          | Pending ->
            Mutex.unlock t.mutex;
            assert false
          | Resolved v ->
            Mutex.unlock t.mutex;
            v))
  ;;
end

let () =
  let promise = Promise.create () in
  Scheduler.run (fun () ->
    let now = Ptime_clock.now () in
    Format.printf "Now (%d): %a\n%!" (Domain.self () :> int) Ptime.pp now;
    Timer.sleep Mtime.Span.s;
    let now = Ptime_clock.now () in
    Format.printf "Now (%d): %a\n%!" (Domain.self () :> int) Ptime.pp now;
    Promise.resolve promise ());
  Promise.await promise
;;
