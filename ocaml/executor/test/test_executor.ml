module Promise = struct
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
end

let execute_task executor fn =
  let p = Promise.create () in
  Executor.async executor (fun () ->
      Printf.printf "Hello from %d\n%!" (Domain.self () :> int);
      try
        let v = fn () in
        Promise.fill p v
      with
      | exn ->
        let bt = Printexc.get_raw_backtrace () in
        Promise.fail p exn bt);
  p
;;

let () =
  Printexc.record_backtrace true;
  let executor = Executor.create () in
  let promise_a = execute_task executor (fun () -> Unix.sleep 1) in
  let promise_b = execute_task executor (fun () -> 1 + 2) in
  let promise_c = execute_task executor (fun () -> 3) in
  let result_b = Promise.read promise_b in
  let result_c = Promise.read promise_c in
  Printf.printf "Result: %d\n%!" (result_b + result_c);
  Promise.read promise_a;
  let boom =
    execute_task executor (fun () ->
        Unix.sleep 2;
        (* This is bad *)
        Promise.fill promise_a ())
  in
  Promise.read boom
;;
