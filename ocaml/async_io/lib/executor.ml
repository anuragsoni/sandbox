open! Types

type read = (unit, unit) Effect.Deep.continuation
type write = (unit, unit) Effect.Deep.continuation

type state =
  { jobs : Threadsafe_queue.t
  ; poll : Poll.t
  ; read_continuations : (Unix.file_descr, read) Hashtbl.t
  ; write_continuations : (Unix.file_descr, write) Hashtbl.t
  }

let init_state () =
  let jobs = Threadsafe_queue.create () in
  let poll = Poll.create () in
  { jobs
  ; poll
  ; read_continuations = Hashtbl.create 512
  ; write_continuations = Hashtbl.create 512
  }
;;

let enqueue_cont state cont =
  Threadsafe_queue.push
    state.jobs
    (Optional_thunk.some (fun () -> Effect.Deep.continue cont ()))
;;

type t =
  { domains : unit Domain.t Array.t
  ; size : int Atomic.t
  ; state : state Array.t
  ; index : int Atomic.t
  }

let run_job state job =
  Effect.Deep.try_with
    job
    ()
    { effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Wait_read fd ->
            Some
              (fun (k : (a, _) Effect.Deep.continuation) ->
                Poll.set state.poll fd Poll.Event.read;
                Hashtbl.add state.read_continuations fd k)
          | Wait_write fd ->
            Some
              (fun k ->
                Poll.set state.poll fd Poll.Event.write;
                Hashtbl.add state.write_continuations fd k)
          | _ -> None)
    }
;;

let rec run idx state =
  let current_state = Array.unsafe_get state idx in
  let current_job_queue = current_state.jobs in
  (* Check if we can make progress with this thread's run queue *)
  match Threadsafe_queue.try_pop current_job_queue with
  | job when Optional_thunk.is_none job ->
    if Hashtbl.length current_state.read_continuations = 0
       && Hashtbl.length current_state.write_continuations = 0
    then (
      let job = Threadsafe_queue.pop current_job_queue in
      let job = Optional_thunk.unsafe_fn job in
      run_job current_state job;
      run idx state)
    else perform_io idx state
  | job ->
    let job = Optional_thunk.unsafe_fn job in
    run_job current_state job;
    run idx state

and perform_io idx state =
  let current_state = Array.unsafe_get state idx in
  (match Poll.wait current_state.poll Poll.Timeout.immediate with
  | `Timeout -> ()
  | `Ok ->
    Poll.iter_ready current_state.poll ~f:(fun fd event ->
        if event.Poll.Event.readable
        then (
          try
            let cont = Hashtbl.find current_state.read_continuations fd in
            Hashtbl.remove current_state.read_continuations fd;
            enqueue_cont current_state cont
          with
          | Not_found -> ());
        if event.writable
        then (
          try
            let cont = Hashtbl.find current_state.write_continuations fd in
            Hashtbl.remove current_state.write_continuations fd;
            enqueue_cont current_state cont
          with
          | Not_found -> ()));
    Poll.clear current_state.poll);
  run idx state
;;

let create () =
  let size = Num_cpus.get () in
  let state = Array.init size (fun _ -> init_state ()) in
  let run idx = run idx state in
  let domains = Array.init size (fun idx -> Domain.spawn (fun () -> run idx)) in
  { domains; size = Atomic.make size; state; index = Atomic.make 0 }
;;

let async t fn =
  let i = Atomic.fetch_and_add t.index 1 in
  let fn = Optional_thunk.some fn in
  let rec loop i n state job =
    if n >= Array.length state
    then
      (* We couldn't push the job to any of the job queues. Use [push] so the thread
         blocks. *)
      Threadsafe_queue.push (Array.get t.state (i mod Array.length t.state)).jobs job
    else (
      (* Attempt to push a job using [try_push]. If this succeeds the job will be enqueued
         without the thread being suspended. *)
      let job_queue = (Array.get state ((i + n) mod Array.length state)).jobs in
      if not (Threadsafe_queue.try_push job_queue job) then loop i (n + 1) state job)
  in
  loop i 0 t.state fn
;;
