type _ Effect.t += Fork : (unit -> unit) -> unit Effect.t
type _ Effect.t += Wait_read : Unix.file_descr -> unit Effect.t
type _ Effect.t += Wait_write : Unix.file_descr -> unit Effect.t

let fork f = Effect.perform (Fork f)
let wait_read fd = Effect.perform (Wait_read fd)
let wait_write fd = Effect.perform (Wait_write fd)

type read = (unit, unit) Effect.Deep.continuation
type write = (unit, unit) Effect.Deep.continuation

type job =
  | Thread : ('a, unit) Effect.Deep.continuation * 'a -> job
  | Read : read -> job
  | Write : write -> job

type state =
  { jobs : job Queue.t
  ; read_continuations : (Unix.file_descr, read) Hashtbl.t
  ; write_continuations : (Unix.file_descr, write) Hashtbl.t
  ; poll : Poll.t
  }

let init () =
  let poll = Poll.create () in
  { jobs = Queue.create ()
  ; read_continuations = Hashtbl.create 1024
  ; write_continuations = Hashtbl.create 1024
  ; poll
  }
;;

let enqueue_read state cont = Queue.push (Read cont) state.jobs
let enqueue_write state cont = Queue.push (Write cont) state.jobs
let enqueue_thread state thread v = Queue.push (Thread (thread, v)) state.jobs

let dequeue state =
  match Queue.pop state.jobs with
  | Thread (thread, v) -> Effect.Deep.continue thread v
  | Read cont -> Effect.Deep.continue cont ()
  | Write cont -> Effect.Deep.continue cont ()
;;

let block_read state fd cont = Hashtbl.add state.read_continuations fd cont
let block_write state fd cont = Hashtbl.add state.write_continuations fd cont

let rec schedule state =
  if Queue.is_empty state.jobs
  then
    if Hashtbl.length state.read_continuations = 0
       && Hashtbl.length state.write_continuations = 0
    then ()
    else perform_io state
  else dequeue state

and perform_io state =
  (match Poll.wait state.poll Poll.Timeout.never with
  | `Ok ->
    Poll.iter_ready state.poll ~f:(fun fd event ->
        if event.Poll.Event.readable
        then (
          try
            let cont = Hashtbl.find state.read_continuations fd in
            Hashtbl.remove state.read_continuations fd;
            enqueue_read state cont
          with
          | Not_found -> ());
        if event.writable
        then (
          try
            let cont = Hashtbl.find state.write_continuations fd in
            Hashtbl.remove state.write_continuations fd;
            enqueue_write state cont
          with
          | Not_found -> ()));
    Poll.clear state.poll
  | `Timeout -> ());
  schedule state
;;

let rec read fd buf ~pos ~len =
  match Unix.read fd buf pos len with
  | count when count = 0 -> `Eof
  | count -> `Ok count
  | exception Unix.(Unix_error (EINTR, _, _)) -> read fd buf ~pos ~len
  | exception Unix.(Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) ->
    wait_read fd;
    read fd buf ~pos ~len
;;

let rec write fd buf ~pos ~len =
  match Unix.write fd buf pos len with
  | count when count = 0 -> `Eof
  | count -> `Ok count
  | exception Unix.(Unix_error (EINTR, _, _)) -> write fd buf ~pos ~len
  | exception Unix.(Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) ->
    wait_write fd;
    write fd buf ~pos ~len
;;

let rec accept ?(cloexec = true) sock =
  match Unix.accept ~cloexec sock with
  | (client_sock, _) as client ->
    Unix.set_nonblock client_sock;
    client
  | exception Unix.(Unix_error (EINTR, _, _)) -> accept ~cloexec sock
  | exception Unix.(Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) ->
    wait_read sock;
    accept ~cloexec sock
;;

let run main =
  let state = init () in
  let rec loop state f =
    Effect.Deep.match_with
      f
      ()
      { retc = (fun () -> schedule state)
      ; exnc =
          (fun exn ->
            print_string (Printexc.to_string exn);
            schedule state)
      ; effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | Fork f ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  enqueue_thread state k ();
                  loop state f)
            | Wait_read fd ->
              Some
                (fun k ->
                  Poll.set state.poll fd Poll.Event.read;
                  block_read state fd k;
                  schedule state)
            | Wait_write fd ->
              Some
                (fun k ->
                  Poll.set state.poll fd Poll.Event.write;
                  block_write state fd k;
                  schedule state)
            | _ -> None)
      }
  in
  loop state main
;;
