module Promise = struct
  type 'a state =
    | Resolved of 'a
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
    | Resolved _ -> failwith "Attempting to resolve a fulfilled promise"
  ;;

  let read t =
    match t.state with
    | Resolved v -> v
    | Pending ->
      Mutex.lock t.mutex;
      (match t.state with
      | Resolved v ->
        Mutex.unlock t.mutex;
        v
      | Pending ->
        Condition.wait t.cond t.mutex;
        Mutex.unlock t.mutex;
        (match t.state with
        | Resolved v -> v
        | Pending -> assert false))
  ;;
end

let execute_task executor fn =
  let p = Promise.create () in
  Executor.async executor (fun () ->
      Printf.printf "Hello from %d\n%!" (Domain.self () :> int);
      let v = fn () in
      Promise.fill p v);
  p
;;

let () =
  let executor = Executor.create () in
  let promise_a = execute_task executor (fun () -> Unix.sleep 1) in
  let promise_b = execute_task executor (fun () -> 1 + 2) in
  let promise_c = execute_task executor (fun () -> 3) in
  let result_b = Promise.read promise_b in
  let result_c = Promise.read promise_c in
  Printf.printf "Result: %d\n%!" (result_b + result_c);
  Promise.read promise_a
;;
