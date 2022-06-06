open Types

type t = Unix.file_descr

let pipe ?(cloexec = true) () =
  let r, w = Unix.pipe ~cloexec () in
  Unix.set_nonblock r;
  Unix.set_nonblock w;
  r, w
;;

let rec read fd buf ~pos ~len =
  match Unix.read fd buf pos len with
  | count -> count
  | exception Unix.(Unix_error (EINTR, _, _)) -> read fd buf ~pos ~len
  | exception Unix.(Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) ->
    Effect.perform (Wait_read fd);
    read fd buf ~pos ~len
;;

let rec write fd buf ~pos ~len =
  match Unix.write fd buf pos len with
  | count -> count
  | exception Unix.(Unix_error (EINTR, _, _)) -> write fd buf ~pos ~len
  | exception Unix.(Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) ->
    Effect.perform (Wait_write fd);
    write fd buf ~pos ~len
;;
