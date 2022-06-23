type t =
  { queues : Local_run_queue.t array
  ; domains : unit Domain.t array
  ; task_count : int Atomic.t
  ; size : int
  }

let enqueue t job =
  let i = Atomic.fetch_and_add t.task_count 1 in
  let pushed = ref false in
  let j = ref t.size in
  while (not !pushed) && !j > 0 do
    let idx = (i + (!j - 1)) mod t.size in
    let job_queue = Array.unsafe_get t.queues idx in
    if Local_run_queue.try_push job_queue job then pushed := true else decr j
  done;
  if not !pushed
  then Local_run_queue.push_job (Array.unsafe_get t.queues (i mod t.size)) job
;;

let create ?num_threads () =
  let num_threads =
    match num_threads with
    | None -> Num_cpus.get ()
    | Some c -> c
  in
  if num_threads < 1
  then
    invalid_arg (Printf.sprintf "Invalid number of threads for scheduler: %d" num_threads);
  let queues = Array.init num_threads (fun _ -> Threadsafe_queue.create ()) in
  let queues = Array.init num_threads (fun i -> Local_run_queue.create i queues) in
  let domains =
    Array.map (fun queue -> Domain.spawn (fun () -> Local_run_queue.run queue)) queues
  in
  { queues; domains; task_count = Atomic.make 0; size = num_threads }
;;

let shutdown t = Array.iter (fun q -> Local_run_queue.shutdown q) t.queues

let run ?num_threads main =
  let t = create ?num_threads () in
  Effect.Deep.try_with
    main
    ()
    { effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Task.Spawn task ->
            Some
              (fun (k : (a, _) Effect.Deep.continuation) ->
                enqueue t task;
                Effect.Deep.continue k ())
          | _ -> None)
    }
;;