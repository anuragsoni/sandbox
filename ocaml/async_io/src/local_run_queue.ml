type t =
  { queue : Threadsafe_queue.t
  ; id : int
  ; queues : Threadsafe_queue.t array
  ; poll : Poll.t
  ; timer : Timer.t
  ; fd_events : (unit, unit) Effect.Deep.continuation Fd_table.t
  ; shutdown : bool Atomic.t
  ; size : int
  }

let create idx queues =
  let queue = Array.get queues idx in
  { queue
  ; id = idx
  ; queues
  ; poll = Poll.create ~num_events:64 ()
  ; fd_events = Fd_table.create 1024
  ; shutdown = Atomic.make false
  ; timer = Timer.create ()
  ; size = Array.length queues
  }
;;

let push_job t fn = Threadsafe_queue.push t.queue fn
let try_push t fn = Threadsafe_queue.try_push t.queue fn

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
            push_job t job
          with
          | Not_found -> ());
        if event.Poll.Event.writable
        then (
          try
            let k = Fd_table.find t.fd_events fd in
            Fd_table.remove t.fd_events fd;
            let job () = Effect.Deep.continue k () in
            push_job t job
          with
          | Not_found -> ());
        Poll.set t.poll fd Poll.Event.none);
    Poll.clear t.poll;
    true
;;

let get_job t = Threadsafe_queue.try_pop t.queue

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
          | Task.Yield -> Some (fun k -> push_job t (fun () -> Effect.Deep.continue k ()))
          | Task.Spawn fn ->
            Some
              (fun k ->
                push_job t fn;
                Effect.Deep.continue k ())
          | Timer.Wakup_at at -> Some (fun k -> Timer.add t.timer ~at k)
          | _ -> None)
    }
;;

let run t =
  while not (Atomic.get t.shutdown) do
    match get_job t with
    | job when Optional_thunk.is_none job ->
      let next_timeout =
        if Timer.has_events t.timer
        then (
          let now = Mtime_clock.now () in
          Timer.advance_timer t.timer ~now ~push:(fun k ->
              push_job t (fun () -> Effect.Deep.continue k ()));
          Timer.next_wakeup_at ~now t.timer)
        else Poll.Timeout.after 50_000_000L
      in
      if Fd_table.length t.fd_events = 0
      then Domain.cpu_relax ()
      else if not (perform_io ~timeout:next_timeout t)
      then Domain.cpu_relax ()
    | job -> run_job t job
  done
;;

let shutdown t = Atomic.set t.shutdown true