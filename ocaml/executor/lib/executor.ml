type job = unit -> unit

type t =
  { domains : unit Domain.t Array.t
  ; size : int Atomic.t
  ; jobs : job Threadsafe_queue.t Array.t
  ; index : int Atomic.t
  }

let rec run idx jobs =
  let current_job_queue = Array.get jobs idx in
  (* Check if we can make progress with this thread's run queue *)
  match Threadsafe_queue.try_pop current_job_queue with
  | None ->
    (* Attempt to steal a job from the list of job queues. We use [try_pop] here so the
       thread isn't suspended. *)
    let rec loop n limit =
      if n >= limit
      then None
      else (
        match Threadsafe_queue.try_pop (Array.get jobs ((idx + n) mod limit)) with
        | None -> loop (n + 1) limit
        | Some job -> Some job)
    in
    (match loop 0 (Array.length jobs) with
    | None ->
      (* No jobs were found. Use [pop] to block til the job queue for the current thread
         has an available job. *)
      let fn = Threadsafe_queue.pop current_job_queue in
      fn ();
      run idx jobs
    | Some job ->
      (* Successfully stole a job *)
      job ();
      run idx jobs)
  | Some job ->
    job ();
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
