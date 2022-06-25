type t =
  { queue : Threadsafe_queue.t
  ; local_queue : Optional_thunk.t Queue.t
  ; id : int
  ; queues : Threadsafe_queue.t array
  ; poll : Poll.t
  ; timer : Timer.t
  ; fd_events : (unit, unit) Effect.Deep.continuation Fd_table.t
  ; shutdown : bool Atomic.t
  ; size : int
  ; task_count : int Atomic.t
  }

let create idx queues =
  let queue = Array.get queues idx in
  { queue
  ; local_queue = Queue.create ()
  ; id = idx
  ; queues
  ; poll = Poll.create ~num_events:64 ()
  ; fd_events = Fd_table.create 1024
  ; shutdown = Atomic.make false
  ; timer = Timer.create ()
  ; size = Array.length queues
  ; task_count = Atomic.make 0
  }
;;

let push_job t fn = Threadsafe_queue.push t.queue fn
let try_push t fn = Threadsafe_queue.try_push t.queue fn

let push_local t fn =
  let fn = Optional_thunk.some fn in
  Queue.push fn t.local_queue
;;

let try_pop_local t =
  match Queue.pop t.local_queue with
  | job -> job
  | exception Queue.Empty -> Threadsafe_queue.try_pop t.queue
;;

let enqueue t fn =
  let i = Atomic.fetch_and_add t.task_count 1 in
  let rec loop i n jobs job =
    if n >= Array.length jobs
    then
      (* We couldn't push the job to any of the job queues. Use [push] so the thread
         blocks. *)
      Threadsafe_queue.push (Array.get t.queues (i mod t.size)) job
    else (
      (* Attempt to push a job using [try_push]. If this succeeds the job will be enqueued
         without the thread being suspended. *)
      let job_queue = Array.get jobs ((i + n) mod Array.length jobs) in
      if not (Threadsafe_queue.try_push job_queue job) then loop i (n + 1) jobs job)
  in
  loop i 0 t.queues fn
;;

let perform_io ~timeout t =
  match Poll.wait t.poll timeout with
  | `Timeout -> false
  | `Ok ->
    Poll.iter_ready t.poll ~f:(fun fd event ->
        if event.Poll.Event.readable
        then (
          try
            let k = Fd_table.find t.fd_events fd in
            Fd_table.remove t.fd_events fd;
            let job () = Effect.Deep.continue k () in
            push_local t job
          with
          | Not_found -> ());
        if event.Poll.Event.writable
        then (
          try
            let k = Fd_table.find t.fd_events fd in
            Fd_table.remove t.fd_events fd;
            let job () = Effect.Deep.continue k () in
            push_local t job
          with
          | Not_found -> ());
        Poll.set t.poll fd Poll.Event.none);
    Poll.clear t.poll;
    true
;;

let try_steal_job t =
  (* Attempt to steal a job from the list of job queues. We use [try_pop] here so the
     thread isn't suspended. *)
  let rec loop n limit =
    if n >= limit
    then Optional_thunk.none
    else (
      match Threadsafe_queue.try_pop (Array.get t.queues ((t.id + n) mod limit)) with
      | job when Optional_thunk.is_none job -> loop (n + 1) limit
      | job -> job)
  in
  loop 0 t.size
;;

let run_job t job =
  let job = Optional_thunk.unsafe_fn job in
  Effect.Deep.try_with
    job
    ()
    { effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Fd.Wait_read (fd, enqueue_on_close) ->
            Some
              (fun (k : (a, _) Effect.Deep.continuation) ->
                Poll.set t.poll fd Poll.Event.read;
                Fd_table.add t.fd_events fd k;
                let on_close () =
                  if Fd_table.mem t.fd_events fd
                  then (
                    Poll.set t.poll fd Poll.Event.none;
                    Fd_table.remove t.fd_events fd)
                in
                enqueue_on_close on_close)
          | Fd.Wait_write (fd, enqueue_on_close) ->
            Some
              (fun k ->
                Poll.set t.poll fd Poll.Event.write;
                Fd_table.add t.fd_events fd k;
                let on_close () =
                  if Fd_table.mem t.fd_events fd
                  then (
                    Poll.set t.poll fd Poll.Event.none;
                    Fd_table.remove t.fd_events fd)
                in
                enqueue_on_close on_close)
          | Task.Yield ->
            Some (fun k -> push_local t (fun () -> Effect.Deep.continue k ()))
          | Task.Spawn fn ->
            Some
              (fun k ->
                enqueue t fn;
                Effect.Deep.continue k ())
          | Timer.Wakup_at at -> Some (fun k -> Timer.add t.timer ~at k)
          | _ -> None)
    }
;;

let rec run t =
  while not (Atomic.get t.shutdown) do
    match try_pop_local t with
    | job when Optional_thunk.is_none job ->
      let next_timeout =
        if Timer.has_events t.timer
        then (
          let now = Mtime_clock.now () in
          Timer.advance_timer t.timer ~now ~push:(fun k ->
              push_local t (fun () -> Effect.Deep.continue k ()));
          Timer.next_wakeup_at ~now t.timer)
        else Poll.Timeout.after 50_000_000L
      in
      if Fd_table.length t.fd_events = 0 || not (perform_io ~timeout:next_timeout t)
      then (
        (* [perform_io] did not enqueue any new jobs to the local queue. Attempt to steal
           any pending tasks from sibling domains. *)
        match try_steal_job t with
        | job when Optional_thunk.is_none job -> Domain.cpu_relax ()
        | job -> run_job t job)
    | job -> run_job t job
  done
;;

let shutdown t = Atomic.set t.shutdown true