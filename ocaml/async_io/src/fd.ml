type _ Effect.t +=
  | Wait_read : (Unix.file_descr * ((unit -> unit) -> unit)) -> unit Effect.t

type _ Effect.t +=
  | Wait_write : (Unix.file_descr * ((unit -> unit) -> unit)) -> unit Effect.t

type t =
  { fd : Unix.file_descr
  ; mutable on_close : (unit -> unit) list
  ; mutable closed : bool
  ; supports_nonblock : bool
  ; kind : Unix.file_kind
  }

let create fd =
  let stat = Unix.fstat fd in
  let kind = stat.st_kind in
  let supports_nonblock =
    match stat.st_kind with
    | Unix.S_FIFO -> true
    | S_CHR -> false
    | S_SOCK -> true
    | S_BLK | S_DIR | S_REG | S_LNK -> false
  in
  { fd; kind; supports_nonblock; closed = false; on_close = [] }
;;

let wait_read t =
  let push_on_close fn =
    let old = t.on_close in
    t.on_close <- fn :: old
  in
  Effect.perform (Wait_read (t.fd, push_on_close))
;;

let wait_write t =
  let push_on_close fn =
    let old = t.on_close in
    t.on_close <- fn :: old
  in
  Effect.perform (Wait_write (t.fd, push_on_close))
;;

let ready_to t event =
  if t.closed
  then `Closed
  else (
    let () =
      match event with
      | `Read -> wait_read t
      | `Write -> wait_write t
    in
    if t.closed then `Closed else `Ready)
;;

let close t =
  if not t.closed
  then (
    t.closed <- true;
    (try Unix.close t.fd with
    | _ -> ());
    let callbacks = t.on_close in
    List.iter (fun fn -> fn ()) callbacks)
;;

let is_closed t = t.closed

let syscall t a f =
  if t.closed
  then `Already_closed
  else (
    try `Ok (f t.fd a) with
    | exn -> `Error exn)
;;

let syscall_exn t a f =
  match syscall t a f with
  | `Ok a -> a
  | `Already_closed -> failwith "File descriptor is already closed"
  | `Error exn -> raise exn
;;
