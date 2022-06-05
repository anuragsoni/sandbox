open Types

type t = executor =
  { domains : unit Domain.t Array.t
  ; size : int Atomic.t
  ; jobs : Threadsafe_queue.t Array.t
  ; index : int Atomic.t
  }

let rec run idx jobs =
  let current_job_queue = Array.get jobs idx in
  (* Check if we can make progress with this thread's run queue *)
  match Threadsafe_queue.try_pop current_job_queue with
  | job when Optional_thunk.is_none job ->
    (* Attempt to steal a job from the list of job queues. We use [try_pop] here so the
       thread isn't suspended. *)
    let rec loop n limit =
      if n >= limit
      then Optional_thunk.none
      else (
        match Threadsafe_queue.try_pop (Array.get jobs ((idx + n) mod limit)) with
        | job when Optional_thunk.is_none job -> loop (n + 1) limit
        | job -> job)
    in
    (match loop 0 (Array.length jobs) with
    | job when Optional_thunk.is_none job ->
      (* No jobs were found. Use [pop] to block til the job queue for the current thread
         has an available job. *)
      let fn = Threadsafe_queue.pop current_job_queue in
      Optional_thunk.unsafe_call fn;
      run idx jobs
    | job ->
      (* Successfully stole a job *)
      Optional_thunk.unsafe_call job;
      run idx jobs)
  | job ->
    Optional_thunk.unsafe_call job;
    run idx jobs
;;

let create () =
  let size = Num_cpus.get () in
  let jobs = Array.init size (fun _ -> Threadsafe_queue.create ()) in
  let run idx = run idx jobs in
  let domains = Array.init size (fun idx -> Domain.spawn (fun () -> run idx)) in
  { domains; size = Atomic.make size; jobs; index = Atomic.make 0 }
;;

let async t fn =
  let i = Atomic.fetch_and_add t.index 1 in
  let fn = Optional_thunk.some fn in
  let rec loop i n jobs job =
    if n >= Array.length jobs
    then
      (* We couldn't push the job to any of the job queues. Use [push] so the thread
         blocks. *)
      Threadsafe_queue.push (Array.get t.jobs (i mod Array.length t.jobs)) job
    else (
      (* Attempt to push a job using [try_push]. If this succeeds the job will be enqueued
         without the thread being suspended. *)
      let job_queue = Array.get jobs ((i + n) mod Array.length jobs) in
      if not (Threadsafe_queue.try_push job_queue job) then loop i (n + 1) jobs job)
  in
  loop i 0 t.jobs fn
;;
