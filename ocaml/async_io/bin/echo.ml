open Async_io

let create_sock port =
  let sock_addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock sock_addr;
  Unix.listen sock 256;
  sock
;;

let close_sock sock =
  try
    Unix.shutdown sock Unix.SHUTDOWN_ALL;
    Unix.close sock
  with
  | _ -> ()
;;

let echo_loop fd =
  let buf = Bytes.create 1024 in
  try
    let rec loop fd buf =
      match
        Fd.syscall fd buf (fun fd buf ->
            match Unix.read fd buf 0 1024 with
            | 0 -> `Eof
            | n -> `Read_some n
            | exception Unix.Unix_error ((EWOULDBLOCK | EAGAIN | EINTR), _, _) ->
              `Poll_again
            | exception
                Unix.Unix_error
                  ( ( EPIPE
                    | ECONNRESET
                    | EHOSTUNREACH
                    | ENETDOWN
                    | ENETRESET
                    | ENETUNREACH
                    | ETIMEDOUT )
                  , _
                  , _ ) -> `Eof
            | exception exn ->
              Printf.printf "%s\n%!" (Printexc.to_string exn);
              `Eof)
      with
      | `Already_closed -> ()
      | `Error exn -> Printf.printf "%s\n%!" (Printexc.to_string exn)
      | `Ok `Eof -> ()
      | `Ok `Poll_again ->
        (match Fd.ready_to fd `Read with
        | `Ready -> loop fd buf
        | `Closed -> ())
      | `Ok (`Read_some count) ->
        ignore (Fd.syscall_exn fd () (fun fd () -> Unix.write fd buf 0 count));
        loop fd buf
    in
    loop fd buf;
    Fd.close fd
  with
  | _ -> Fd.close fd
;;

let () =
  Printexc.record_backtrace true;
  let port = int_of_string Sys.argv.(1) in
  let num_threads =
    try Some (int_of_string Sys.argv.(2)) with
    | _ -> None
  in
  let sock = create_sock port in
  Printf.printf "Listening on http://localhost:%d\n%!" port;
  Scheduler.run ?num_threads (fun () ->
      try
        while true do
          let client_sock, _ = Unix.accept sock in
          Unix.set_nonblock client_sock;
          Task.spawn (fun () -> echo_loop (Fd.create client_sock))
        done
      with
      | exn ->
        Printf.printf "%s\n%!" (Printexc.to_string exn);
        close_sock sock)
;;