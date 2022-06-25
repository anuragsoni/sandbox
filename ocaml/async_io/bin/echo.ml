open Async_io

let create_sock port =
  let sock_addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock sock_addr;
  Unix.set_nonblock sock;
  Unix.listen sock 256;
  Fd.create sock
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

let rec accept fd =
  Fd.syscall_exn fd () (fun sock () ->
      match Unix.accept sock with
      | client_sock, _ ->
        Unix.set_nonblock client_sock;
        `Ok (Fd.create client_sock)
      | exception Unix.Unix_error ((EWOULDBLOCK | EAGAIN | EINTR), _, _) ->
        (match Fd.ready_to fd `Read with
        | `Ready -> accept fd
        | `Closed -> `Eof)
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
      let stop = ref false in
      try
        while not !stop do
          match accept sock with
          | `Ok client_sock -> Task.spawn (fun () -> echo_loop client_sock)
          | `Eof -> stop := true
        done
      with
      | exn ->
        Printf.printf "%s\n%!" (Printexc.to_string exn);
        Fd.close sock)
;;
