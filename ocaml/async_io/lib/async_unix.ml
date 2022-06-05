open Types

module Fd = struct
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
end

module Sock = struct
  type t = Fd.t

  let create_sock port =
    let sock_addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    Unix.bind sock sock_addr;
    Unix.listen sock 256;
    Unix.set_nonblock sock;
    sock
  ;;

  let create ?cloexec ?(backlog = 128) addr domain kind =
    let sock = Unix.socket ?cloexec domain kind 0 in
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    Unix.bind sock addr;
    Unix.listen sock backlog;
    Unix.set_nonblock sock;
    sock
  ;;

  let rec accept ?(cloexec = true) sock =
    match Unix.accept ~cloexec sock with
    | (client_sock, _) as client ->
      Unix.set_nonblock client_sock;
      client
    | exception Unix.(Unix_error (EINTR, _, _)) -> accept ~cloexec sock
    | exception Unix.(Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) ->
      Effect.perform (Wait_read sock);
      accept ~cloexec sock
  ;;

  let close t =
    try
      Unix.shutdown t Unix.SHUTDOWN_ALL;
      Unix.close t
    with
    | _ -> ()
  ;;
end
