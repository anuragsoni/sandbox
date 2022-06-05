type 'a state =
  | Resolved of 'a
  | Failed of (exn * Printexc.raw_backtrace)
  | Pending

type 'a t =
  { mutable state : 'a state
  ; mutex : Mutex.t
  ; cond : Condition.t
  }

let create () = { state = Pending; mutex = Mutex.create (); cond = Condition.create () }

let fill t v =
  Mutex.lock t.mutex;
  match t.state with
  | Pending ->
    t.state <- Resolved v;
    Mutex.unlock t.mutex;
    Condition.broadcast t.cond
  | Resolved _ | Failed _ -> failwith "Attempting to resolve a fulfilled promise"
;;

let fail t exn bt =
  Mutex.lock t.mutex;
  match t.state with
  | Pending ->
    t.state <- Failed (exn, bt);
    Mutex.unlock t.mutex;
    Condition.broadcast t.cond
  | Resolved _ | Failed _ -> failwith "Attempting to resolve a fulfilled promise"
;;

let read t =
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
      Condition.wait t.cond t.mutex;
      Mutex.unlock t.mutex;
      (match t.state with
      | Resolved v -> v
      | Failed (exn, bt) -> Printexc.raise_with_backtrace exn bt
      | Pending -> assert false))
;;
