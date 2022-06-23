type 'a state =
  | Pending
  | Resolved of 'a
  | Failed of (exn * Printexc.raw_backtrace)

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
  | Failed _ | Resolved _ ->
    Mutex.unlock t.mutex;
    failwith "Attempting to fill a non empty promise"
;;

let fail t exn bt =
  Mutex.lock t.mutex;
  match t.state with
  | Pending ->
    t.state <- Failed (exn, bt);
    Mutex.unlock t.mutex;
    Condition.broadcast t.resolved
  | Failed _ | Resolved _ ->
    Mutex.unlock t.mutex;
    failwith "Attempting to fill a non empty promise"
;;

let await t =
  match t.state with
  | Resolved v -> v
  | Failed (exn, bt) -> Printexc.raise_with_backtrace exn bt
  | Pending ->
    Mutex.lock t.mutex;
    (match t.state with
    | Resolved v ->
      Mutex.unlock t.mutex;
      v
    | Failed (exn, bt) ->
      Mutex.unlock t.mutex;
      Printexc.raise_with_backtrace exn bt
    | Pending ->
      Condition.wait t.resolved t.mutex;
      (match t.state with
      | Pending ->
        Mutex.unlock t.mutex;
        assert false
      | Failed (exn, bt) ->
        Mutex.unlock t.mutex;
        Printexc.raise_with_backtrace exn bt
      | Resolved v ->
        Mutex.unlock t.mutex;
        v))
;;